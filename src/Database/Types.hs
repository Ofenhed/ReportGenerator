{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.Types where
import Data.FileEmbed (embedFile)

import Control.Applicative
import Data.Default.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

data Template = Template { templateId :: Int
                         , templateIncludeName :: Text.Text
                         , templateLongName :: Maybe Text.Text
                         , templateDescription :: Maybe Text.Text
                         , templateSource :: Text.Text
                         , templateIncludable :: Int } deriving Show

data Report = Report { reportId :: Int,
                       reportName :: Text.Text,
                       reportTemplate :: Template } deriving Show

instance FromRow Template where
  fromRow = Template <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Report where
  fromRow = Report <$> field <*> field <*> (Template <$> field <*> field <*> field <*> field <*> field <*> field)


setupDatabase conn = withTransaction conn $ do
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
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVar (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id), CONSTRAINT no_arrays UNIQUE (template, parent));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVars (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, weight INTEGER NOT NULL, FOREIGN KEY (template) REFERENCES TemplateVars(id), FOREIGN KEY (parent) REFERENCES ReportVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS CustVar (id INTEGER PRIMARY KEY ASC, name TEXT NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT no_arrays UNIQUE (report, name));"

    -- Test data

    execute conn "INSERT INTO Template (includeName, source, includable) VALUES ('pentest', ?, 0);" (Only $ Encoding.decodeUtf8 $(embedFile "temp/default_report.txt"))
    tempId <- lastInsertRowId conn
    execute conn "INSERT INTO Template (includeName, source, includable) VALUES ('exec_summary', ?, 1);" (Only $ Text.pack "{{ heading(1, 'Executive Summary') }} This is the executive summary. Stuff was {{template.exec_summary.summary}}. {{template.exec_summary.summary.explained}}")
    execSum <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'confidential', 'Whether this report is confidential', 'text');" (Only tempId)
    confidential <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'customer', 'The name of the customer', 'text');" (Only tempId)
    custId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'customer_address', 'The address of the customer', 'text');" (Only tempId)
    cust2Id <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'summary', 'How bad was it?', 'text');" (Only execSum)
    execVar <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (templateVar, name, description, type, data) VALUES (?, 'explained', 'How bad was it really?', 'text', 'Pretty damn bad');" (Only execVar)
    recVar <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (template, name, description, type) VALUES (?, 'people', 'The people involved in this report', 'text');" (Only tempId)
    peopleId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (templateVars, name, description, type) VALUES (?, 'title', 'Titles for a person', 'text');" (Only peopleId)
    titleId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (templateVars, name, description, type) VALUES (?, 'email', 'Email to person for report', 'text');" (Only peopleId)
    emailId <- lastInsertRowId conn
    execute conn "INSERT INTO Report (template, name) VALUES (?, 'Some customer')" (Only tempId)
    reportId <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Super Important Customer #1');" (custId, reportId)
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, '1');" (confidential, reportId)
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Super Important Customer Street');" (cust2Id, reportId)
    execute conn "INSERT INTO ReportVars (template, parent, data, weight) VALUES (?, ?, 'Marcus Ofenhed', 1);" (peopleId, reportId)
    marcus <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVars (template, parent, data, weight) VALUES (?, ?, 'Someone Else', 2);" (peopleId, reportId)
    other <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Marcus.Ofenhed@hotmail.com');" (emailId, marcus)
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Someone.Else@hotmail.com');" (emailId, other)
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Awesome');" (execVar, reportId)
    execute conn "INSERT INTO ReportVars (template, parent, data, weight) VALUES (?, ?, 'Super 1337 Haxxor', 1);" (titleId, other)
    execute conn "INSERT INTO ReportVars (template, parent, data, weight) VALUES (?, ?, 'Has won an Apex Legend match', 2);" (titleId, other)
    prevRef <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, 'Still awesome');" (recVar, prevRef)
