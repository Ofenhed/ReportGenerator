{-# LANGUAGE OverloadedStrings #-}
module Database.Resolver where

import Database.Types
import Types

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import qualified Data.Text as Text
import qualified Data.Map as Map

import Debug.Trace

getVariable :: Connection -> TemplateVarParent -> Int -> IO [(Int, Maybe Int, Text.Text, Maybe Text.Text)]
getVariable conn template parent = do
  let (target, val) = case template of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  query conn (Query $ Text.concat ["SELECT TemplateVar.id, ReportVar.id, TemplateVar.name, CASE WHEN ReportVar.data IS NOT NULL THEN ReportVar.data ELSE TemplateVar.data END AS data FROM TemplateVar LEFT JOIN ReportVar ON ReportVar.template == TemplateVar.id AND ReportVar.parent == ? WHERE TemplateVar.", target, " == ?"]) (parent, val)

getVariables :: Connection -> TemplateVarParent -> Int -> IO [(Int, Text.Text, [(Int, Text.Text)])]
getVariables conn template parent = do
  let (target, val) = case template of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  vars <- query conn (Query $ Text.concat ["SELECT TemplateVars.id, TemplateVars.name FROM TemplateVars WHERE ", target, " == ?"]) (Only val)
  flip mapM vars $ \(tId, tName) -> do
    values <- query conn "SELECT id, data FROM ReportVars WHERE parent == ? AND template == ? ORDER BY weight ASC" (parent, tId)
    return $ (tId, tName, values)

getValue conn report var = do
  result <- query conn "SELECT * FROM ReportVar WHERE report == ? AND parent == ?" (report, var)
  case result of
    [result] -> return $ Just result
    [] -> return $ Nothing

getValues conn report var = do
  query conn "SELECT * FROM ReportVars WHERE report == ? AND parent == ?" (report, var)

data Template = Template { templateId :: Int
                         , templateIncludeName :: Text.Text
                         , templateLongName :: Maybe Text.Text
                         , templateDescription :: Maybe Text.Text
                         , templateSource :: Text.Text
                         , templateIncludable :: Int } deriving Show

data Report = Report { reportId :: Int,
                       reportName :: Text.Text,
                       reportTemplate :: Template } deriving Show

instance FromRow Report where
  fromRow = Report <$> field <*> field <*> (Template <$> field <*> field <*> field <*> field <*> field <*> field)

getReport :: Connection -> Int -> IO (Maybe (Report, IOReportContext))
getReport conn reportId = do
  var <- query conn "SELECT Report.id, Report.name, Template.* FROM Report LEFT JOIN Template ON Report.template == Template.id WHERE Report.id = ?" (Only reportId)
  context <- newIORef $ ReportContext { reportContextId = reportId, reportContextVariable = Map.empty, reportContextCustomVariable = Map.empty }
  case var of
    [] -> return Nothing
    [r] -> do
      var <- query conn "SELECT CustVar.name, CustVar.data FROM CustVar WHERE CustVar.report = ?" (Only reportId)
      atomicModifyIORef' context $ \c -> (c { reportContextCustomVariable = Map.fromList var }, ())
      includeTemplateVariables conn context (templateIncludeName $ reportTemplate r) $ templateId $ reportTemplate r
      return $ Just (r, context)

includeTemplateVariables conn context key template = do
  context' <- readIORef context
  let findVariablesRecursive from parent = do
        var <- getVariable conn from parent
        var' <- flip mapM var $ \(tempId, varId, name, d) -> do
            -- vars <- getVariables conn from
            otherVar <- case varId of Just varId' -> findVariablesRecursive (TemplateVarParentVar tempId) varId' ; Nothing -> return Map.empty
            -- otherVars <- mapM (findVariablesRecursive . TemplateVarParentVars) vars
            return $ (name, ReportVar { reportVarVariables = otherVar, reportVarValue = d, reportVarArray = [] })
        vars <- getVariables conn from parent
        vars' <- flip mapM vars $ \(tempId, name, v) -> do
            v' <- flip mapM v $ \(rId, val) -> do
                others <- findVariablesRecursive (TemplateVarParentVars tempId) rId
                return $ ReportVar { reportVarVariables = others, reportVarValue = Just val, reportVarArray = [] }
            return (name, ReportVar { reportVarVariables = Map.empty, reportVarValue = Nothing, reportVarArray = v' })
        let merged = Map.unionWith (\var vars -> var { reportVarArray = reportVarArray vars }) (Map.fromList var') (Map.fromList vars')
        return merged
  vars <- findVariablesRecursive (TemplateVarParent template) (reportContextId context')
  atomicModifyIORef' context $ \c -> (c { reportContextVariable = Map.insert key (ReportVar { reportVarVariables = vars, reportVarValue = Nothing, reportVarArray = []}) (reportContextVariable c) }, ())

getTemplate :: Connection -> IOReportContext -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplate conn context template included = do
  traceShowM template
  var <- query conn "SELECT id, includeName, source FROM Template WHERE includeName == ? AND (? == 0 OR includable == 1)" (template, included)
  context' <- readIORef context
  case var of
    [] -> return $ Nothing
    [(tId, name, v)] -> do
      includeTemplateVariables conn context name tId
      c <- readIORef context
      return $ Just v
