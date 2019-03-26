{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.Types where
import Types

import Data.FileEmbed (embedFile)

import Control.Applicative
import Data.Default.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Int (Int64)

templateParentName (TemplateVarParent i) = ("template", i)
templateParentName (TemplateVarParentVar i) = ("templateVar", i)
templateParentName (TemplateVarParentVars i) = ("templateVars", i)

data Template = Template { templateId :: Int64
                         , templateIncludeName :: Text.Text
                         , templateLongName :: Maybe Text.Text
                         , templateDescription :: Maybe Text.Text
                         , templateSource :: Text.Text
                         , templateEditor :: Text.Text
                         , templateIncludable :: Int } deriving Show

data Report = Report { reportId :: Int64,
                       reportName :: Text.Text,
                       reportTemplate :: Template } deriving Show

instance FromRow Template where
  fromRow = Template <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Report where
  fromRow = Report <$> field <*> field <*> (Template <$> field <*> field <*> field <*> field <*> field <*> field <*> field)

data User = User { userId :: Int64
                 , userUsername :: Text.Text
                 , userPassId :: Int64
                 , userKey :: Maybe (Text.Text, Text.Text) } deriving Show

setupDatabase conn = withTransaction conn $ do
    execute_ conn "CREATE TABLE IF NOT EXISTS Template (id INTEGER PRIMARY KEY ASC, includeName TEXT NOT NULL, longName TEXT NULL, description TEXT NULL, source TEXT NOT NULL, editor TEXT NOT NULL, includable INTEGER NOT NULL, CONSTRAINT unique_name UNIQUE (includeName));"
    let templateVarRelation = "FOREIGN KEY (template) REFERENCES Template(id), FOREIGN KEY (templateVar) REFERENCES TemplateVar(id), FOREIGN KEY (templateVars) REFERENCES TemplateVars(id) \
                             \, CONSTRAINT one_parent CHECK (((template, templateVars) IS (NULL, NULL) AND templateVar IS NOT NULL) OR \
                                                           \((template, templateVar) IS (NULL, NULL) AND templateVars IS NOT NULL) OR \
                                                           \((templateVar, templateVars) IS (NULL, NULL) AND template IS NOT NULL))\
                             \, CONSTRAINT unique_name_template UNIQUE (template, name)\
                             \, CONSTRAINT unique_name_template_var UNIQUE (templateVar, name)\
                             \, CONSTRAINT unique_name_template_vars UNIQUE (templateVars, name)"
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVar (id INTEGER PRIMARY KEY ASC, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, data TEXT NULL, ", templateVarRelation, ");"]
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVars (id INTEGER PRIMARY KEY ASC, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, ", templateVarRelation, ");"]
    execute_ conn "CREATE TABLE IF NOT EXISTS Report (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, name TEXT NOT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVar (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id), CONSTRAINT no_arrays UNIQUE (template, parent));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVars (id INTEGER PRIMARY KEY ASC, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, weight INTEGER NOT NULL, FOREIGN KEY (template) REFERENCES TemplateVars(id), FOREIGN KEY (parent) REFERENCES ReportVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS CustVar (id INTEGER PRIMARY KEY ASC, name TEXT NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT no_arrays UNIQUE (report, name));"
    execute_ conn "CREATE TABLE IF NOT EXISTS User (id INTEGER PRIMARY KEY ASC, username TEXT NOT NULL, passhash TEXT NOT NULL, salt TEXT NOT NULL, passid INTEGER NOT NULL DEFAULT 0, publicKey TEXT, privateKey TEXT, CONSTRAINT full_set_of_keys CHECK ((publicKey IS NULL AND privateKey IS NULL) OR (publicKey IS NOT NULL AND privateKey IS NOT NULL)), CONSTRAINT unique_username UNIQUE (username));"
    execute_ conn "CREATE TRIGGER IF NOT EXISTS user_pass_counter AFTER UPDATE ON User WHEN OLD.passhash != NEW.passhash BEGIN UPDATE User SET passid = OLD.passid + 1 WHERE id = NEW.id; END;"

    -- Test data

    execute conn "INSERT INTO Template (includeName, source, includable, editor) VALUES ('pentest', ?, 0, ?);" (Encoding.decodeUtf8 $(embedFile "temp/default_report.txt"), Encoding.decodeUtf8 $(embedFile "temp/pentest_editor.txt"))
    tempId <- lastInsertRowId conn
    execute conn "INSERT INTO Template (includeName, source, includable, editor) VALUES ('exec_summary', ?, 1, ?);" (Text.pack "{{ heading(1, 'Executive Summary') }} This is the executive summary. Stuff was {{template.exec_summary.summary}}. {{template.exec_summary.summary.explained}}", Text.pack "{% if report.templateIncludeName == 'exec_summary' %}{{ make_text(variables.exec_summary.children.summary) }}{% else %}<a href='/report/sub/{{report.templateId}}/2/'>Exec Summary</a> {{variables.exec_summary.children.summary.val}}{%endif%}")
    execSum <- lastInsertRowId conn
    execute conn "INSERT INTO Template (includeName, source, includable, editor) VALUES ('eval', ?, 1, '');" (Only $ Text.pack "{% macro _(e, c) -%} \
                                                                                                     \ {{- eval(src=e, context=merge(c, {'heading': heading, 'push_heading': push_heading, 'pop_heading': pop_heading, 'ref': ref, 'report': report})) -}} \
                                                                                                     \{%- endmacro %}")
    execute conn "INSERT INTO TemplateVar (template, name, description) VALUES (?, 'confidential', 'Whether this report is confidential');" (Only tempId)
    confidential <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description, data) VALUES (?, 'customer', 'The name of the customer', 'Secret Customer');" (Only tempId)
    custId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description) VALUES (?, 'customer_address', 'The address of the customer');" (Only tempId)
    cust2Id <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (template, name, description) VALUES (?, 'summary', 'How bad was it?');" (Only execSum)
    execVar <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (templateVar, name, description, data) VALUES (?, 'explained', 'How bad was it really?', 'Pretty damn bad');" (Only execVar)
    recVar <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (template, name, description) VALUES (?, 'people', 'The people involved in this report');" (Only tempId)
    peopleId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVars (templateVars, name, description) VALUES (?, 'title', 'Titles for a person');" (Only peopleId)
    titleId <- lastInsertRowId conn
    execute conn "INSERT INTO TemplateVar (templateVars, name, description) VALUES (?, 'email', 'Email to person for report');" (Only peopleId)
    emailId <- lastInsertRowId conn
    execute conn "INSERT INTO Report (template, name) VALUES (?, 'Some customer')" (Only tempId)
    reportId <- lastInsertRowId conn
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
