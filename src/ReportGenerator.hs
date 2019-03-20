{-# LANGUAGE OverloadedStrings #-}
module ReportGenerator where

import Database.Resolver
import Database.Types
import Heading
import Types

import qualified Data.Text as Text

import Data.Default.Class
import Text.Ginger.Run (makeContextHtmlM, runGingerT, runtimeErrorMessage)
import Text.Ginger.Parse (parseGinger)
import Text.Ginger (toGVal, ParserError(..), VarName, Run, liftRun)
import Text.Ginger.GVal (fromFunction, GVal(..))
import Text.Ginger.Html (unsafeRawHtml, htmlSource, html, Html)
import Text.Ginger.AST (Template)
import Data.Maybe (maybe)
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef, IORef())
import Data.List (isPrefixOf, foldl')
import Database.SQLite.Simple (Connection)
import qualified Data.DList                     as D
import qualified Data.Text.IO                   as TextIO
import Data.Text.AhoCorasick.Automaton (CaseSensitivity(CaseSensitive))
import qualified Data.Text.AhoCorasick.Replacer as Replacer
import Control.Monad.Writer (execWriter, tell)

import Debug.Trace

includeResolver conn context file = do
  case Text.splitOn "/" $ Text.pack file of
    [".", "template", t] -> getTemplate conn context t True >>= return . (maybe Nothing (Just . Text.unpack))
    _ -> return $ Nothing

data ReportState = ReportState { stateReportId :: Int,
                                 stateTemplateId :: Int,
                                 stateHeadingState :: IOHeading,
                                 stateDbConn :: Connection }

contextLookup :: ReportState -> IOReportContext -> VarName -> Run p IO Html (GVal (Run p IO Html))
contextLookup reportState context var = do
  let headerState = stateHeadingState reportState
  case var of
             "heading" -> return $ fromFunction $ createHeading headerState
             "pop_heading" -> return $ fromFunction $ popHeading headerState
             "push_heading" -> return $ fromFunction $ pushHeading headerState
             "ref" -> return $ fromFunction $ createRef headerState
             "table_of_contents" -> return $ toGVal $ tableOfContentPlaceholder
             "template" -> liftRun $ readIORef context >>= return . toGVal . reportContextVariable
             "report" -> liftRun $ readIORef context >>= return . toGVal . reportContextCustomVariable
             _ -> return def

render conn report = do
  headerState <- createHeaderState
  template' <- getReport conn report
  case template' of
    Nothing -> return "Error: Could not find template for report"
    Just (t, context) -> do
      Right parsed <- parseGinger (includeResolver conn context) Nothing $ Text.unpack $ templateSource $ reportTemplate t
      vec <- newIORef $ D.empty
      let reportState = ReportState { stateReportId = report, stateTemplateId = templateId $ reportTemplate t, stateHeadingState = headerState, stateDbConn = conn }
          gingerContext = makeContextHtmlM (contextLookup reportState context) (\n -> atomicModifyIORef' vec (\state -> (D.snoc state n, ())))
      result <- runGingerT gingerContext parsed
      case result of
        Right _ -> do
                   vec' <- readIORef vec
                   finalized <- finalizeRefs headerState
                   toc <- finalizeTableOfContents headerState
                   let replacer = Replacer.build CaseSensitive $ toc:finalized
                   let h = execWriter $ mapM (tell . (Replacer.run replacer) . htmlSource) $ D.toList vec'
                   return h
                   --case finalized of
                   --  Right h -> finalizeTableOfContents headerState h >>= return
                   --  Left err -> return $ Text.concat ["Error: ", err]
        Left err -> return $ Text.concat ["Error: ", runtimeErrorMessage err]
