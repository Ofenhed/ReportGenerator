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
import Crypto.PubKey.RSA (PublicKey)
import qualified Data.Text as Text
import qualified Data.Map  as Map
import qualified Data.Text.Encoding as Encoding
import Data.Int (Int64)

templateParentName (TemplateVarParent i) = ("template", i)
templateParentName (TemplateVarParentVar i) = ("templateVar", i)
templateParentName (TemplateVarParentVars i) = ("templateVars", i)

type EncryptedPrivateKey = Text.Text

data Template = Template { templateId :: Int64
                         , templateIncludeName :: Text.Text
                         , templateLongName :: Maybe Text.Text
                         , templateDescription :: Maybe Text.Text
                         , templateSource :: Text.Text
                         , templateEditor :: Text.Text
                         , templateIncludable :: Int
                         , templateMain :: Int } deriving Show

data Report = Report { reportId :: Int64,
                       reportName :: Text.Text,
                       reportEncrypted :: Int,
                       reportTemplate :: Template } deriving Show

instance FromRow Template where
  fromRow = Template <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Report where
  fromRow = Report <$> field <*> field <*> field <*> (Template <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field)

data User = User { userId :: Int64
                 , userUsername :: Text.Text
                 , userPassId :: Int64
                 , userKey :: Maybe (PublicKey, EncryptedPrivateKey) } deriving Show

type TemplateVarTree = ([TemplateVars], [TemplateVar])
data TemplateVars = TemplateVars { templateVarsId :: Int64
                                 , templateVarsName :: Text.Text
                                 , templateVarsDescription :: Maybe Text.Text
                                 , templateVarsChildren :: TemplateVarTree } deriving Show

data TemplateVar = TemplateVar { templateVarId :: Int64
                               , templateVarName :: Text.Text
                               , templateVarDescription :: Maybe Text.Text
                               , templateVarDefault :: Maybe Text.Text
                               , templateVarChildren :: TemplateVarTree } deriving Show

data SavedVars = SavedVars { savedVarsId :: Int64
                           , savedVarsTemplate :: Int64
                           , savedVarsName :: Text.Text
                           , savedVarsDescription :: Maybe Text.Text
                           , savedVarsData :: Maybe Text.Text
                           , savedVarsVar :: Map.Map Int64 (Int64, Text.Text) } deriving Show
instance Default SavedVars where
  def = SavedVars 0 0 Text.empty Nothing Nothing Map.empty

setupDatabase conn = withTransaction conn $ do
    execute_ conn "CREATE TABLE IF NOT EXISTS SettingEscrowAccount (id INTEGER PRIMARY KEY, user INTEGER NOT NULL, FOREIGN KEY (user) REFERENCES user(id) ON DELETE CASCADE, CONSTRAINT only_one_escrow CHECK (id = 1));"
    execute_ conn "CREATE TABLE IF NOT EXISTS DatabaseVersion (id INTEGER PRIMARY KEY, version INTEGER NOT NULL, CONSTRAINT only_one_version CHECK (id = 1));"
    execute_ conn "CREATE TABLE IF NOT EXISTS User (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL, passhash TEXT NOT NULL, salt TEXT NOT NULL, passid INTEGER NOT NULL DEFAULT 0, publicKey TEXT, privateKey TEXT, CONSTRAINT full_set_of_keys CHECK ((publicKey IS NULL AND privateKey IS NULL) OR (publicKey IS NOT NULL AND privateKey IS NOT NULL)), CONSTRAINT unique_username UNIQUE (username));"
    execute_ conn "CREATE TRIGGER IF NOT EXISTS user_pass_counter AFTER UPDATE ON User WHEN OLD.passhash != NEW.passhash BEGIN UPDATE User SET passid = OLD.passid + 1 WHERE id = NEW.id; END;"
    execute_ conn "CREATE TABLE IF NOT EXISTS Template (id INTEGER PRIMARY KEY AUTOINCREMENT, includeName TEXT NOT NULL, longName TEXT NULL, description TEXT NULL, source TEXT NOT NULL, editor TEXT NOT NULL, includable INTEGER NOT NULL DEFAULT 0, main_template INTEGER NOT NULL DEFAULT 0, CONSTRAINT unique_name UNIQUE (includeName));"
    let templateVarRelation = "FOREIGN KEY (template) REFERENCES Template(id), FOREIGN KEY (templateVar) REFERENCES TemplateVar(id), FOREIGN KEY (templateVars) REFERENCES TemplateVars(id) \
                             \, CONSTRAINT one_parent CHECK (((template, templateVars) IS (NULL, NULL) AND templateVar IS NOT NULL) OR \
                                                           \((template, templateVar) IS (NULL, NULL) AND templateVars IS NOT NULL) OR \
                                                           \((templateVar, templateVars) IS (NULL, NULL) AND template IS NOT NULL))\
                             \, CONSTRAINT unique_name_template UNIQUE (template, name)\
                             \, CONSTRAINT unique_name_template_var UNIQUE (templateVar, name)\
                             \, CONSTRAINT unique_name_template_vars UNIQUE (templateVars, name)"
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVar (id INTEGER PRIMARY KEY AUTOINCREMENT, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, data TEXT NULL, ", templateVarRelation, ");"]
    execute_ conn $ Query $ Text.concat ["CREATE TABLE IF NOT EXISTS TemplateVars (id INTEGER PRIMARY KEY AUTOINCREMENT, template INTEGER NULL, templateVar INTEGER NULL, templateVars INTEGER NULL, name TEXT NOT NULL, description TEXT, ", templateVarRelation, ");"]

    execute_ conn "CREATE TABLE IF NOT EXISTS SavedVar (id INTEGER PRIMARY KEY, templateVars INTEGER NOT NULL, name TEXT NOT NULL, description TEXT NULL, data TEXT NULL, FOREIGN KEY (templateVars) REFERENCES TemplateVars(id), CONSTRAINT unique_name UNIQUE (templateVars, name));"
    execute_ conn "CREATE TABLE IF NOT EXISTS SavedTemplateVar (id INTEGER PRIMARY KEY AUTOINCREMENT, savedVar INTEGER NOT NULL, templateVar INTEGER NOT NULL, data TEXT NOT NULL, FOREIGN KEY (templateVar) REFERENCES TemplateVar(id), FOREIGN KEY (savedVar) REFERENCES SavedVar(id));"

    execute_ conn "CREATE TABLE IF NOT EXISTS Report (id INTEGER PRIMARY KEY AUTOINCREMENT, template INTEGER NOT NULL, name TEXT NOT NULL, owner INTEGER NOT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id), FOREIGN KEY (owner) REFERENCES User(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVar (id INTEGER PRIMARY KEY AUTOINCREMENT, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (template) REFERENCES TemplateVar(id), CONSTRAINT no_arrays UNIQUE (template, parent));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportVars (id INTEGER PRIMARY KEY AUTOINCREMENT, template INTEGER NOT NULL, parent INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (template) REFERENCES TemplateVars(id), FOREIGN KEY (parent) REFERENCES ReportVar(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS CustVar (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, report INTEGER NOT NULL, data TEXT NULL, iv TEXT NULL, FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT no_arrays UNIQUE (report, name));"
    execute_ conn "CREATE TABLE IF NOT EXISTS ReportKey (id INTEGER PRIMARY KEY AUTOINCREMENT, user INTEGER NOT NULL, report INTEGER NOT NULL, key TEXT NOT NULL, FOREIGN KEY (user) REFERENCES User(id), FOREIGN KEY (report) REFERENCES Report(id), CONSTRAINT only_one_key_per_report_and_user UNIQUE (user, report));"

