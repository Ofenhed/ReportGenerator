{-# LANGUAGE OverloadedStrings #-}
module DatabaseResolver where

import DatabaseTypes

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import qualified Data.Text as Text

data TemplateVarParent = TemplateVarParent Int
                       | TemplateVarParentVar Int
                       | TemplateVarParentVars Int deriving Show

getVariable conn parent name = do
  let (target, val) = case parent of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  result <- query conn (Query $ Text.concat ["SELECT * FROM TemplateVar WHERE ", target, " == ? AND name == ?"]) (val, name)
  case result of 
    [result] -> return $ Just result
    [] -> return $ Nothing

getVariables conn parent name = do
  let (target, val) = case parent of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  result <- query conn (Query $ Text.concat ["SELECT * FROM TemplateVars WHERE ", target, " == ? AND name == ?"]) (val, name)
  case result of 
    [result] -> return $ Just result
    [] -> return $ Nothing

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

getReport :: Connection -> Int -> IO (Maybe Report)
getReport conn reportId = do
  var <- query conn "SELECT Report.id, Report.name, Template.* FROM Report LEFT JOIN Template ON Report.template == Template.id WHERE Report.id = ?" (Only reportId)
  case var of
    [r] -> return $ Just r
    [] -> return Nothing

getTemplate :: Connection -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplate conn template included = do
  var <- if included
           then query conn "SELECT source FROM Template WHERE includeName == ? AND includable == 1" (Only template)
           else query conn "SELECT source FROM Template WHERE includeName == ?" (Only template)
  return $ case var of
    [Only v] -> Just v
    [] -> Nothing

