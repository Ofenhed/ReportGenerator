module Types where

import qualified Network.Wai.Session as S
import Database.SQLite.Simple (Connection)
import qualified Data.Text as Text
import qualified Data.Vault.Lazy                as Vault

data SessionType = Session { sessionDbConn :: Connection
                           , sessionSession :: Vault.Key (S.Session IO Text.Text Text.Text) }
