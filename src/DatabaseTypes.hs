{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DatabaseTypes where
import Data.FileEmbed (embedFile)

import Control.Applicative
import Data.Default.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

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
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVar (id INTEGER PRIMARY KEY ASC, parent INTEGER NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (parent) REFERENCES TemplateVar(id), FOREIGN KEY (report) REFERENCES ReportVar(id), CONSTRAINT no_arrays UNIQUE (parent, report));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVars (id INTEGER PRIMARY KEY ASC, parent INTEGER NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, weight INTEGER NOT NULL, FOREIGN KEY (parent) REFERENCES TemplateVars(id), FOREIGN KEY (report) REFERENCES ReportVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS CustVar (id INTEGER PRIMARY KEY ASC, name TEXT NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT no_arrays UNIQUE (report, name));"

    -- Test data

    execute conn "INSERT INTO Template (includeName, source, includable) VALUES ('pentest', ?, 0);" (Only $ Encoding.decodeUtf8 $(embedFile "templates/default_report.txt"))
    tempId <- lastInsertRowId conn
    execute conn "INSERT INTO Template (includeName, source, includable) VALUES ('exec_summary', ?, 1);" (Only $ Text.pack "This is the executive summary. Stuff was {{variable('exec_summary', 'summary')}}.")
    execSum <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'customer', 'The name of the customer', 'text');" (Only tempId)
    custId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, type) VALUES (?, 'summary', 'How bad was it?', 'text');" (Only execSum)
    execVar <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (template, name, description, type) VALUES (?, 'people', 'The people involved in this report', 'text');" (Only tempId)
    peopleId <- lastInsertRowId conn
    execute conn "INSERT INTO Report (template, name) VALUES (?, 'Some customer')" (Only tempId)
    reportId <- lastInsertRowId conn
    execute conn "INSERT INTO ReportVar (parent, report, data) VALUES (?, ?, 'Super Important Customer #1');" (custId, reportId)
    execute conn "INSERT INTO ReportVars (parent, report, data, weight) VALUES (?, ?, 'Marcus Ofenhed', 1);" (peopleId, reportId)
    execute conn "INSERT INTO ReportVars (parent, report, data, weight) VALUES (?, ?, 'Someone Else', 2);" (peopleId, reportId)
    execute conn "INSERT INTO ReportVar (parent, report, data) VALUES (?, ?, 'Awesome');" (execVar, reportId)
