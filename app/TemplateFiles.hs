{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TemplateFiles (runTemplate) where

import Types
import Database.Types
import Common

import System.IO (openBinaryFile, IOMode(ReadMode), hGetContents)
import Data.FileEmbed (embedDir)
import Language.Haskell.TH
import Control.Exception (try, throw)
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import Data.List (stripPrefix)
import Text.Ginger.Parse (parseGinger, ParserError(..))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef)
import Text.Ginger.Run (makeContextHtmlM, runGingerT, runtimeErrorMessage)
import Data.Default.Class (def)
import Control.Monad.Writer (execWriter, tell)
import Text.Ginger.Html (htmlSource)
import Data.Maybe (maybe, isNothing)
import Text.Ginger.GVal (ToGVal(..))

import qualified Data.DList                     as D
import qualified Data.Map                       as Map
import qualified Data.ByteString.Char8          as C8
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Text                      as Text

instance ToGVal m User where
  toGVal u = def { isNull = False
                 , asText = asText $ toGVal $ userUsername u
                 , asHtml = asHtml $ toGVal $ userUsername u
                 , asLookup = Just $ \name -> case name of
                                       "id" -> Just $ toGVal $ userId u
                                       _ -> Nothing }

cachedAtCompile = flip lookup $(embedDir "templates")

-- | Read a file at compile time, that's read again at runtime. Use runtime version if it's available.
cachedReadFile filename = catchIOError (openBinaryFile ("templates/" ++ filename ++ ".tpl") ReadMode >>= hGetContents >>= return . Just) $
                                       \error -> if isDoesNotExistError error
                                                   then case cachedAtCompile $ filename ++ ".tpl" of
                                                          Just content -> return $ Just $ C8.unpack content
                                                          Nothing -> return Nothing
                                                   else return Nothing

runTemplate context fetcher filename lookup = do
  content <- cachedReadFile filename
  case content of
    Nothing -> throw $ VisibleError $ Text.pack $ "Template " ++ filename ++ " does not exist"
    Just f -> do
      parsed <- parseGinger (maybe cachedReadFile (\a -> a cachedReadFile) fetcher) Nothing f
      case parsed of
        Left err -> throw $ VisibleError $ Text.pack $ "Parser error: " ++ show err
        Right parsed' -> do
          vec <- newIORef $ D.empty
          let lookup' name = case name of
                          "user" -> return $ toGVal $ sessionUser context
                          "None" -> return $ toGVal $ (Nothing :: Maybe Int)
                          _ -> lookup name
              context' = makeContextHtmlM lookup' (\n -> atomicModifyIORef' vec (\state -> (D.snoc state n, ())))
          result <- runGingerT context' parsed'
          case result of
            Left err -> throw $ VisibleError $ Text.pack $ "Runtime error: " ++ show err
            Right _ -> do
              vec' <- readIORef vec
              return $ execWriter $ mapM (tell . htmlSource) $ D.toList vec'


