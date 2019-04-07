module Database.Extra where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Control.Exception (finally)

withBindNamed :: Statement -> [NamedParam] -> IO a -> IO a
withBindNamed stmt params io = do
  bindNamed stmt params
  io `finally` reset stmt

handleRows :: (FromRow a) => Statement -> (a -> IO b) -> [b] -> IO [b]
handleRows statement mapper prev = do
  maybeRow <- nextRow statement
  case maybeRow of
    Just r -> do val <- mapper (seq r r)
                 handleRows statement mapper $ val:prev
    Nothing -> return $ reverse prev
