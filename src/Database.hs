{-# LANGUAGE OverloadedStrings #-}
module Database where
import Control.Applicative
import Data.Default.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
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

openDatabase = do
  conn <- open "Reports.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS template (id INTEGER PRIMARY KEY ASC, name TEXT NOT NULL, html TEXT NOT NULL);"
  execute_ conn "CREATE TABLE IF NOT EXISTS tests (id INTEGER PRIMARY KEY ASC, title TEXT NOT NULL, description TEXT NOT NULL, performed INT NOT NULL, parent INTEGER, FOREIGN KEY (parent) REFERENCES tests(id));"
  execute_ conn "CREATE TABLE IF NOT EXISTS findings (id INTEGER PRIMARY KEY ASC, test INTEGER NOT NULL, title TEXT NOT NULL, description TEXT NOT NULL, cvss_vector TEXT NOT NULL, FOREIGN KEY (test) REFERENCES tests(id));"
  execute_ conn "PRAGMA foreign_key=ON;"
  return conn

