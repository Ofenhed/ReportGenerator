{-# LANGUAGE OverloadedStrings #-}
module Database.Writer where

import Database.Types
import Database.Resolver
import Types
import UserType

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import System.Random (randomRIO)

import qualified Data.Text as Text
import qualified Data.Map as Map

changeTemplate :: Connection -> (Maybe Template -> (Maybe Template, a)) -> Int64 -> IO a
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

deleteTemplateVariable :: Connection -> TemplateVarParent -> IO ()
deleteTemplateVariable conn idx = withTransaction conn $ deleteTemplateVariable' conn idx
  where
  deleteTemplateVariable' conn (TemplateVarParentVar i) = do
    var <- query conn "SELECT id FROM TemplateVar WHERE templateVar = ?" (Only i)
    flip mapM var $ (deleteTemplateVariable' conn) . TemplateVarParentVar . fromOnly
    vars <- query conn "SELECT id FROM TemplateVars WHERE templateVar = ?" (Only i)
    flip mapM vars $ (deleteTemplateVariable' conn) . TemplateVarParentVars . fromOnly
    execute conn "DELETE FROM ReportVar WHERE template = ?" (Only i)
    execute conn "DELETE FROM TemplateVar WHERE id = ?" (Only i)
  
  deleteTemplateVariable' conn (TemplateVarParentVars i) = do
    var <- query conn "SELECT id FROM TemplateVar WHERE templateVars = ?" (Only i)
    flip mapM var $ (deleteTemplateVariable' conn) . TemplateVarParentVar . fromOnly
    vars <- query conn "SELECT id FROM TemplateVars WHERE templateVars = ?" (Only i)
    flip mapM vars $ (deleteTemplateVariable' conn) . TemplateVarParentVars . fromOnly
    execute conn "DELETE FROM ReportVars WHERE template = ?" (Only i)
    execute conn "DELETE FROM TemplateVars WHERE id = ?" (Only i)

  fromOnly (Only x) = x

addTemplateVariable conn parent name value = let (parentType, idx) = templateParentName parent
                                               in execute conn (Query $ Text.concat ["INSERT INTO TemplateVar (", parentType, ", name, data) VALUES (?, ?, ?)"]) (idx, name, value)

addTemplateArray conn parent name = let (parentType, idx) = templateParentName parent
                                      in execute conn (Query $ Text.concat ["INSERT INTO TemplateVars (", parentType, ", name) VALUES (?, ?)"]) (idx, name)

setVariable :: (FromField a, ToField a, Show a) => Connection -> Int64 -> IndexPathType -> a -> IO Bool
setVariable conn report path value = withTransaction conn $ do
  let endOfPath = take 2 $ reverse path
  report' <- case endOfPath of
    i@(IndexArr _):_ -> getParentReport conn i
    i@(IndexVal _):_ -> getParentReport conn i
    (IndexTempVar _):[i] -> getParentReport conn i
    (IndexTempVars _):[i] -> getParentReport conn i
    (IndexTempVar _):[] -> return $ Just report
    (IndexTempVars _):[] -> return $ Just report
  if Just report /= report'
    then throw $ VisibleError "The changed variable does not belong to the current report"
    else return ()
  case endOfPath of
    IndexTempVar i:p -> do let parent = case p of
                                          [IndexArr p] -> p
                                          [IndexVal p] -> p
                                          [] -> report
                           executeNamed conn "INSERT INTO ReportVar (template, parent, data) \
                                               \SELECT :template, :parent, :value \
                                                 \FROM TemplateVar \
                                                   \WHERE (TemplateVar.data IS NULL OR TemplateVar.data != :value) AND TemplateVar.id = :template \
                                             \ON CONFLICT(template, parent) DO UPDATE SET data = :value"
                                                      [":template" := i, ":parent" := parent, ":value" := value]
                           return True
    [IndexVal i] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, report) >> return True
    [IndexArr i] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, report) >> return True
                       
    [IndexVal i, IndexVal parent] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexVal i, IndexArr parent] -> execute conn "UPDATE ReportVar SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexArr i, IndexArr parent] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    [IndexArr i, IndexVal parent] -> execute conn "UPDATE ReportVars SET data = ? WHERE id = ? AND parent = ?" (value, i, parent) >> return True
    _ -> return False

addArray :: Connection -> Int64 -> IndexPathType -> IO IndexPathType
addArray conn report path = withTransaction conn $ do
  let reversedPath = reverse path
      endOfPath = take 2 $ reversedPath
  report' <- case endOfPath !! 0 of
               i@(IndexArr _) -> getParentReport conn i
               i@(IndexVal _) -> getParentReport conn i
               _ -> return $ Just report
  if Just report /= report'
    then throw $ VisibleError "The new array does not belong to the current report"
    else return ()
  case endOfPath of
    IndexTempVars i:p -> do let parent = case p of
                                           [IndexArr p] -> p
                                           [IndexVal p] -> p
                                           [] -> report
                            executeNamed conn "INSERT INTO ReportVars (template, parent, weight) VALUES (:template, :parent, (SELECT MAX((SELECT weight FROM ReportVars WHERE parent = :parent UNION SELECT 0)) + 1))" [":template" := i, ":parent" := parent]
                            newIndex <- lastInsertRowId conn
                            return $ reverse $ IndexArr newIndex:drop 1 reversedPath
    _ -> throw $ VisibleError "Not yet implemented"

-- Users

addUser :: Connection -> Text.Text -> Text.Text -> IO ()
addUser conn username password = do
  salt <- flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))
  let salt' = Text.pack salt
  execute conn "INSERT INTO User (username, salt, passhash) VALUES (?, ?, ?)" (username, salt', show $ hashPasswordAndSalt password salt')

updateUserPassword :: Connection -> Text.Text -> Text.Text -> Text.Text -> IO Bool
updateUserPassword conn username oldpass newpass = withTransaction conn $ do
  u <- getUserWithPassword conn username oldpass
  case u of
    Nothing -> return False
    Just u -> do
      salt <- flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))
      let salt' = Text.pack salt
      execute conn "UPDATE User SET salt = ?, passhash = ? WHERE id = ?" (salt', show $ hashPasswordAndSalt newpass salt', userId u)
      c <- changes conn
      return $ c /= 1
