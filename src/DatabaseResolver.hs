{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module DatabaseResolver where

import DatabaseTypes

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Text.Ginger.GVal (ToGVal(..), asHtml, asText, isNull, asList, asLookup)
import Data.Maybe (isNothing, fromMaybe)
import Data.Default.Class (def)

import qualified Data.Text as Text
import qualified Data.Map as Map

import Debug.Trace

data ReportVar = ReportVar { reportVarValue :: Maybe Text.Text
                           , reportVarVariables :: Map.Map Text.Text ReportVar
                           , reportVarArray :: [ReportVar] } deriving Show

instance ToGVal m ReportVar where
  toGVal xs = def { asHtml = asHtml $ toGVal $ fromMaybe "" $ reportVarValue xs
                  , asText = asText $ toGVal $ fromMaybe "" $ reportVarValue xs
                  , isNull = (isNothing $ reportVarValue xs) && (null $ reportVarArray xs)
                  , asList = if null $ reportVarArray xs
                               then Nothing
                               else Just $ map toGVal $ reportVarArray xs
                  , asLookup = Just $ flip Map.lookup $ Map.map toGVal $ reportVarVariables xs
                  }

instance ToGVal m (Map.Map Text.Text ReportVar) where
  toGVal xs = def { asLookup = Just $ flip Map.lookup $ Map.map toGVal xs
                  , isNull = Map.null xs
                  }

data ReportContext = ReportContext { reportContextId :: Int
                                   , reportContextVariable :: Map.Map Text.Text ReportVar } deriving Show
type IOReportContext = IORef ReportContext

data TemplateVarParent = TemplateVarParent Int
                       | TemplateVarParentVar Int
                       | TemplateVarParentVars Int deriving Show

getVariable :: Connection -> TemplateVarParent -> Int -> IO [(Int, Maybe Int, Text.Text, Maybe Text.Text)]
getVariable conn parent report = do
  let (target, val) = case parent of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  query conn (Query $ Text.concat ["SELECT TemplateVar.id, ReportVar.id, TemplateVar.name, CASE WHEN ReportVar.data IS NOT NULL THEN ReportVar.data ELSE TemplateVar.data END AS data FROM TemplateVar LEFT JOIN ReportVar ON ReportVar.template == TemplateVar.id AND ReportVar.parent == ? WHERE TemplateVar.", target, " == ?"]) (report, val)

getVariables :: Connection -> TemplateVarParent -> Int -> IO [(Int, Text.Text, Maybe Text.Text)]
getVariables conn parent report = do
  let (target, val) = case parent of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  query conn (Query $ Text.concat ["SELECT TemplateVars.id, TemplateVars.name, ReportVars.data RIGHT JOIN ReportVars ON ReportsVar.parent == TemplateVar.id and ReportVars.report == ? FROM TemplateVars WHERE ", target, " == ?"]) (report, val)

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
  context <- newIORef $ ReportContext { reportContextId = reportId, reportContextVariable = Map.empty }
  case var of
    [] -> return Nothing
    [r] -> do
      includeTemplateVariables conn context (templateIncludeName $ reportTemplate r) $ templateId $ reportTemplate r
      return $ Just (r, context)

includeTemplateVariables conn context key template = do
  context' <- readIORef context
  let findVariablesRecursive from parent = do
        var <- getVariable conn from parent
        flip mapM var $ \(tempId, varId, name, d) -> do
            -- vars <- getVariables conn from
            otherVar <- case varId of Just varId' -> findVariablesRecursive (TemplateVarParentVar tempId) varId' ; Nothing -> return []
            -- otherVars <- mapM (findVariablesRecursive . TemplateVarParentVars) vars
            return $ (name, ReportVar { reportVarVariables = Map.fromList otherVar, reportVarValue = d, reportVarArray = [] })
  vars <- findVariablesRecursive (TemplateVarParent template) (reportContextId context')
  atomicModifyIORef' context $ \c -> (c { reportContextVariable = Map.insert key (ReportVar { reportVarVariables = Map.fromList vars, reportVarValue = Nothing, reportVarArray = []}) (reportContextVariable c) }, ())

getTemplate :: Connection -> IOReportContext -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplate conn context template included = do
  var <- if included
           then query conn "SELECT id, includeName, source FROM Template WHERE includeName == ? AND includable == 1" (Only template)
           else query conn "SELECT id, includeName, source FROM Template WHERE includeName == ?" (Only template)
  context' <- readIORef context
  case var of
    [] -> return $ Nothing
    [(tId, name, v)] -> do
      includeTemplateVariables conn context name tId
      c <- readIORef context
      return $ Just v
