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

templateParentName (TemplateVarParent i) = ("template", i)
templateParentName (TemplateVarParentVar i) = ("templateVar", i)
templateParentName (TemplateVarParentVars i) = ("templateVars", i)

getVariable :: Connection -> TemplateVarParent -> Int -> IO [(Int, Int, Text.Text, Maybe Text.Text)]
getVariable conn template parent = do
  let (target, val) = templateParentName template
  query conn (Query $ Text.concat ["SELECT TemplateVar.id, ReportVar.id, TemplateVar.name, CASE WHEN ReportVar.data IS NOT NULL THEN ReportVar.data ELSE TemplateVar.data END AS data FROM TemplateVar INNER JOIN ReportVar ON ReportVar.template == TemplateVar.id AND ReportVar.parent == ? WHERE TemplateVar.", target, " == ?"]) (parent, val)

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
  let findVariablesRecursive from parent path = do
        var <- getVariable conn from parent
        var' <- flip mapM var $ \(tempId, varId, name, d) -> do
            -- vars <- getVariables conn from
            let path' = path ++ [IndexVal varId]
            otherVar <- findVariablesRecursive (TemplateVarParentVar tempId) varId path'
            -- otherVars <- mapM (findVariablesRecursive . TemplateVarParentVars) vars
            return $ (name, ReportVar { reportVarVariables = otherVar, reportVarValue = Just (path', d), reportVarArray = Nothing })
        vars <- getVariables conn from parent
        vars' <- flip mapM vars $ \(tempId, name, v) -> do
            v' <- flip mapM v $ \(rId, val) -> do
                let path' = path ++ [IndexArr rId]
                others <- findVariablesRecursive (TemplateVarParentVars tempId) rId path'
                return $ ReportVar { reportVarVariables = others, reportVarValue = Just (path', Just val), reportVarArray = Nothing }
            return (name, ReportVar { reportVarVariables = Map.empty, reportVarValue = Nothing, reportVarArray = Just (path, v') })
        let merged = Map.unionWith (\var vars -> var { reportVarArray = reportVarArray vars }) (Map.fromList var') (Map.fromList vars')
        return merged
  vars <- findVariablesRecursive (TemplateVarParent template) (reportContextId context') []
  atomicModifyIORef' context $ \c -> (c { reportContextVariable = Map.insert key (ReportVar { reportVarVariables = vars, reportVarValue = Nothing, reportVarArray = Nothing}) (reportContextVariable c) }, ())

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

-- Editor

getTemplates :: Connection -> IO [Template]
getTemplates conn = query_ conn "SELECT * FROM Template" :: IO [Template]

type TemplateVarTree = ([TemplateVars], [TemplateVar])
data TemplateVars = TemplateVars { templateVarsId :: Int
                                 , templateVarsName :: Text.Text
                                 , templateVarsDescription :: Maybe Text.Text
                                 , templateVarsChildren :: TemplateVarTree } deriving Show

data TemplateVar = TemplateVar { templateVarId :: Int
                               , templateVarName :: Text.Text
                               , templateVarDescription :: Maybe Text.Text
                               , templateVarDefault :: Maybe Text.Text
                               , templateVarChildren :: TemplateVarTree } deriving Show

getTemplateAndVariables :: Connection -> Int -> IO (Maybe (Template, TemplateVarTree))
getTemplateAndVariables conn id = do
  template <- query conn "SELECT * FROM Template WHERE Template.id == ?" (Only id) :: IO [Template]
  case template of
    [] -> return Nothing
    [template'] -> do
      let findVarsRecursive parent = do
            let (target, val) = templateParentName parent
            templateVars <- query conn (Query $ Text.concat ["SELECT id, name, description FROM TemplateVars WHERE ", target, " == ?"]) (Only val)
            templateVar <- query conn (Query $ Text.concat ["SELECT id, name, description, data FROM TemplateVar WHERE ", target, " == ?"]) (Only val)
            vars' <- flip mapM templateVars $ \(varsId, varsName, varsDesciption) ->
                                                findVarsRecursive (TemplateVarParentVars varsId) >>=
                                                  \children -> return $ TemplateVars { templateVarsId = varsId
                                                                                     , templateVarsName = varsName
                                                                                     , templateVarsDescription = varsDesciption
                                                                                     , templateVarsChildren = children }
                                                  
            var' <- flip mapM templateVar $ \(varId, varName, varDesciption, varDefault) ->
                                                findVarsRecursive (TemplateVarParentVar varId) >>=
                                                  \children -> return $ TemplateVar { templateVarId = varId
                                                                                    , templateVarName = varName
                                                                                    , templateVarDescription = varDesciption
                                                                                    , templateVarDefault = varDefault
                                                                                    , templateVarChildren = children }
            return (vars', var')
      vars <- findVarsRecursive $ TemplateVarParent $ templateId template'
      return $ Just (template', vars)
      
getReports :: Connection -> IO [Report]
getReports conn = query_ conn "SELECT Report.id, Report.name, Template.* FROM Report LEFT JOIN Template ON Template.id == Report.template"
