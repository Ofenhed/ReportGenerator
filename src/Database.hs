{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Database where
import Control.Applicative
import Data.Default.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.FileEmbed (embedFile)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import Debug.Trace

data TestField = TestField { testId :: Int
                           , testTitle :: String
                           , testDescription :: String
                           , testPerformed :: Int
                           , testParent :: Maybe Int} deriving Show

data FindingField = FindingField { findingId :: Int
                                 , findingTestId :: Int
                                 , findingTitle :: String
                                 , findingDescription :: String
                                 , findingCvssVector :: String} deriving Show

instance Default TestField where
  def = TestField { testId=0, testTitle="", testDescription="", testPerformed=0, testParent=Nothing }

instance Default FindingField where
  def = FindingField { findingId=0, findingTestId=0, findingTitle="", findingDescription="", findingCvssVector="" }

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field <*> field
instance FromRow FindingField where
  fromRow = FindingField <$> field <*> field <*> field <*> field <*> field
instance ToRow TestField where
  toRow (TestField {testId=c1, testTitle=c2, testDescription=c3, testPerformed=c4, testParent=c5}) = toRow (c1, c2, c3, c4, c5)
instance ToRow FindingField where
  toRow (FindingField {findingId=c1, findingTestId=c2, findingTitle=c3, findingDescription=c4, findingCvssVector=c5}) = toRow (c1, c2, c3, c4, c5)

data TemplateVarParent = Template Int
                       | TemplateVar Int
                       | TemplateVars Int deriving Show

getVariable conn parent name = do
  let (target, val) = case parent of
                        Template i -> ("template", i)
                        TemplateVar i -> ("templateVar", i)
                        TemplateVars i -> ("templateVars", i)
  result <- query conn (Query $ Text.concat ["SELECT * FROM TemplateVar WHERE ", target, " == ? AND name == ?"]) (val, name)
  case result of 
    [result] -> return $ Just result
    [] -> return $ Nothing

getVariables conn parent name = do
  let (target, val) = case parent of
                        Template i -> ("template", i)
                        TemplateVar i -> ("templateVar", i)
                        TemplateVars i -> ("templateVars", i)
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

getTemplate :: Connection -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplate conn template included = do
  traceShowM template
  var <- if included
           then query conn "SELECT source FROM Template WHERE includeName == ? AND includable == 1" (Only template)
           else query conn "SELECT source FROM Template WHERE includeName == ?" (Only template)
  return $ case var of
    [Only v] -> Just v
    [] -> Nothing

openDatabase = do
  conn <- open "Reports.db"
  execute_ conn "PRAGMA foreign_key=ON;"
  withTransaction conn $ do
    execute_ conn "CREATE TABLE IF NOT EXISTS Template (id INTEGER PRIMARY KEY ASC, includeName TEXT NOT NULL, longName TEXT NULL, description TEXT NULL, source TEXT NOT NULL, includable INTEGER NOT NULL, CONSTRAINT unique_name UNIQUE (includeName));"
    let templateVarRelation = "FOREIGN KEY (template) REFERENCES Template(id), FOREIGN KEY (templateVar) REFERENCES TemplateVar(id), FOREIGN KEY (templateVars) REFERENCES TemplateVars(id) \
                             \, CONSTRAINT one_parent CHECK (((template, templateVars) IS (NULL, NULL) AND templateVar IS NOT NULL) OR \
                                                           \((template, templateVar) IS (NULL, NULL) AND templateVars IS NOT NULL) OR \
                                                           \((templateVar, templateVars) IS (NULL, NULL) AND template IS NOT NULL))\
                             \, CONSTRAINT unique_name_template UNIQUE (template, name)\
                             \, CONSTRAINT unique_name_template_var UNIQUE (templateVar, name)\
                             \, CONSTRAINT unique_name_template_vars UNIQUE (templateVars, name)"
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVar (id INTEGER PRIMARY KEY ASC, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, type TEXT NOT NULL, data TEXT NULL, ", templateVarRelation, ");"]
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVars (id INTEGER PRIMARY KEY ASC, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, type TEXT NOT NULL, ", templateVarRelation, ");"]
    execute_ conn "CREATE TABLE IF NOT EXISTS Report (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, name TEXT NOT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVar (id INTEGER PRIMARY KEY ASC, parent INTEGER NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (parent) REFERENCES TemplateVar(id), FOREIGN KEY (report) REFERENCES ReportVar(id), CONSTRAINT no_arrays UNIQUE (parent, report));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVars (id INTEGER PRIMARY KEY ASC, parent INTEGER NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, weight INTEGER NOT NULL, FOREIGN KEY (parent) REFERENCES TemplateVars(id), FOREIGN KEY (report) REFERENCES ReportVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS CustVar (id INTEGER PRIMARY KEY ASC, name TEXT NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT no_arrays UNIQUE (report, name));"

    -- Test data

    execute conn "INSERT INTO Template (includeName, source, includable) VALUES ('pentest', ?, 0);" (Only $ Encoding.decodeUtf8 $(embedFile "templates/default_report.txt"))
    execute_ conn "INSERT INTO Template (includeName, source, includable) VALUES ('exec_summary', 'This is the executive summary', 1);"
    tempId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'customer', 'The name of the customer', 'text');" (Only tempId)
    custId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (template, name, description, type) VALUES (?, 'people', 'The people involved in this report', 'text');" (Only tempId)
    peopleId <- lastInsertRowId conn
    execute conn "INSERT INTO Report (template, name) VALUES (?, 'Some customer')" (Only tempId)
    reportId <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVar (parent, report, data) VALUES (?, ?, 'Super Important Customer #1');" (custId, reportId)
    execute conn "INSERT INTO ReportVars (parent, report, data, weight) VALUES (?, ?, 'Marcus Ofenhed', 1);" (peopleId, reportId)
    execute conn "INSERT INTO ReportVars (parent, report, data, weight) VALUES (?, ?, 'Someone Else', 2);" (peopleId, reportId)
  return conn

