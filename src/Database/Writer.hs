{-# LANGUAGE OverloadedStrings #-}
module Database.Writer where

import Database.Types
import Types

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import qualified Data.Text as Text
import qualified Data.Map as Map

changeTemplate :: Connection -> (Maybe Template -> (Maybe Template, a)) -> Int -> IO a
changeTemplate conn f id = do
  template <- query conn "SELECT * FROM Template WHERE id == ?" (Only id)
  let var = case template of
              [t] -> Just t
              _ -> Nothing
      (newTemplate, ret) = f var
  case (var, newTemplate) of
    (Nothing, Just _) -> throw $ VisibleError "Tried to create a new template in a change function"
    (Just _, Just newTemplate') ->
      if templateId newTemplate' /= id
        then throw $ VisibleError "Tried to change id of a template"
        else do
          executeNamed conn "UPDATE Template SET includeName = :includeName\
                                              \, longName = :longName \
                                              \, description = :description \
                                              \, source = :source \
                                              \, editor = :editor \
                                              \, includable = :includable \
                                              \ WHERE id== :id"
                            [":includeName" := templateIncludeName newTemplate'
                            ,":longName" := templateLongName newTemplate'
                            ,":description" := templateDescription newTemplate'
                            ,":source" := templateSource newTemplate'
                            ,":editor" := templateEditor newTemplate'
                            ,":includable" := templateIncludable newTemplate'
                            ,":id" := id]
          return ret
        
    _ -> return ret

setVariable :: (FromField a, ToField a) => Connection -> Int -> IndexPathType -> a -> IO Bool
setVariable conn report path value = do
  case take 2 $ reverse path of
    [IndexTempVar i] -> do v <- query conn "SELECT data == ? FROM TemplateVar WHERE id = ?" (value, i)
                           case v of
                             [Only False] -> execute conn "INSERT INTO ReportVar (template, parent, data) VALUES (?, ?, ?)" (i, report, value)
                             _ -> return ()
                           return True
    [IndexVal i] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, report) >> return True
    [IndexArr i] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, report) >> return True
                       
    [IndexVal i, IndexVal parent] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexVal i, IndexArr parent] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexArr i, IndexArr parent] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexArr i, IndexVal parent] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    _ -> return False
