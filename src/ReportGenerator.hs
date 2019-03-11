{-# LANGUAGE OverloadedStrings #-}
module ReportGenerator where

import DatabaseResolver
import Heading

import qualified Data.Text as Text

import Data.Default.Class
import Text.Ginger.Run (makeContextHtmlM, runGingerT)
import Text.Ginger.Parse (parseGinger)
import Text.Ginger (toGVal, ParserError(..), VarName, Run)
import Text.Ginger.GVal (fromFunction, GVal(..))
import Text.Ginger.Html (htmlSource, unsafeRawHtml, htmlSource, html, Html)
import Text.Ginger.AST (Template)
import Data.Maybe (maybe)
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef, IORef())
import Data.List (isPrefixOf)
import Database.SQLite.Simple (Connection)
import qualified Data.DList                     as D
import qualified Data.Text.IO                   as TextIO

import Debug.Trace

includeResolver conn file = do
  case Text.splitOn "/" $ Text.pack file of
    [".", "template", t] -> getTemplate conn t True >>= return . (maybe Nothing (Just . Text.unpack))
    _ -> return $ Nothing

data ReportState = ReportState { stateReportId :: Int,
                                 stateTemplateId :: Int,
                                 stateHeadingState :: IOHeading,
                                 stateDbConn :: Connection }

-- getTemplateVar state = do
  

contextLookup :: ReportState -> VarName -> Run p IO Html (GVal (Run p IO Html))
contextLookup reportState var = do
  let headerState = stateHeadingState reportState
  return $ case var of
             -- "template_var" -> fromFunction $ 
             -- "sub_var" -> fromFunction $
             -- "template_vars" -> fromFunction $
             -- "sub_vars" -> fromFunction $
             "username" -> toGVal $ Text.pack "haxxor"
             "customer" -> toGVal $ Text.pack "Some customer"
             "header" -> fromFunction $ createHeading headerState
             "confidential" -> toGVal True
             "ref" -> fromFunction $ createRef headerState
             "table_of_contents" -> toGVal $ tableOfContentPlaceholder
             _ -> def

render conn report = do
  headerState <- createHeaderState
  template' <- getReport conn report
  case template' of
    Nothing -> return "Error: Could not find template for report"
    Just t -> do
      Right parsed <- parseGinger (includeResolver conn) Nothing $ Text.unpack $ templateSource $ reportTemplate t
      vec <- newIORef $ D.empty
      let reportState = ReportState { stateReportId = report, stateTemplateId = templateId $ reportTemplate t, stateHeadingState = headerState, stateDbConn = conn }
          context = makeContextHtmlM (contextLookup reportState) (\n -> atomicModifyIORef' vec (\state -> (D.snoc state (htmlSource n), ())))
      Right _ <- runGingerT context parsed
      vec' <- readIORef vec
      let h = D.foldr Text.append Text.empty vec'
      finalized <- finalizeRefs headerState h
      case finalized of
        Right h -> finalizeTableOfContents headerState h >>= return
        Left err -> return $ Text.concat ["Error: ", err]
