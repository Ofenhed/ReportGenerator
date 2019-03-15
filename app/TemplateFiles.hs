{-# LANGUAGE TemplateHaskell #-}
module TemplateFiles (runTemplate) where

import System.IO (openBinaryFile, IOMode(ReadMode), hGetContents)
import Data.FileEmbed (embedDir)
import Language.Haskell.TH
import Control.Exception (try)
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import Data.List (stripPrefix)
import Text.Ginger.Parse (parseGinger, ParserError(..))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef)
import Text.Ginger.Run (makeContextHtmlM, runGingerT, runtimeErrorMessage)
import Control.Monad.Writer (execWriter, tell)
import Text.Ginger.Html (htmlSource)

import qualified Data.DList                     as D
import qualified Data.Map                       as Map
import qualified Data.ByteString.Char8          as C8
import qualified Data.Text.Encoding             as Encoding

cachedAtCompile = flip lookup $(embedDir "templates")

-- | Read a file at compile time, that's read again at runtime. Use runtime version if it's available.
cachedReadFile filename = catchIOError (openBinaryFile ("templates/" ++ filename ++ ".tpl") ReadMode >>= hGetContents >>= return . Just) $
                                       \error -> if isDoesNotExistError error
                                                   then case cachedAtCompile $ filename ++ ".tpl" of
                                                          Just content -> return $ Just $ C8.unpack content
                                                          Nothing -> return Nothing
                                                   else return Nothing

runTemplate filename lookup = do
  content <- cachedReadFile filename
  case content of
    Nothing -> return $ Left $ "Template " ++ filename ++ " does not exist"
    Just f -> do
      parsed <- parseGinger cachedReadFile Nothing f
      case parsed of
        Left err -> return $ Left $ "Parser error: " ++ show err
        Right parsed' -> do
          vec <- newIORef $ D.empty
          let context = makeContextHtmlM lookup (\n -> atomicModifyIORef' vec (\state -> (D.snoc state n, ())))
          result <- runGingerT context parsed'
          case result of
            Left err -> return $ Left $ "Runtime error: " ++ show err
            Right _ -> do
              vec' <- readIORef vec
              return $ Right $ execWriter $ mapM (tell . htmlSource) $ D.toList vec'


