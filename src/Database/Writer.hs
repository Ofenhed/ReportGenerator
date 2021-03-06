{-# LANGUAGE OverloadedStrings #-}
module Database.Writer where

import Database.Types
import Database.Resolver
import Database.Encryption
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

setVariable :: Connection -> Maybe EncryptionKey -> Int64 -> IndexPathType -> Maybe Text.Text -> IO Bool
setVariable conn encKey report path value = do
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
  (iv, value, plain) <- case encKey of
                          Just k -> generateIv >>= \iv -> return (Just iv, case encryptData k iv value of Right r -> Just r ; Left _ -> Nothing, value)
                          Nothing -> return (Nothing, value, value)
  case (iv, value) of
    (Just _, Nothing) -> throw $ VisibleError "Encryption failed"
    _ -> return ()
  case endOfPath of
    IndexTempVar i:p -> do let parent = case p of
                                          [IndexArr p] -> p
                                          [IndexVal p] -> p
                                          [] -> report
                           executeNamed conn "INSERT INTO ReportVar (template, parent, data, iv) \
                                               \SELECT :template, :parent, :value, :iv \
                                                 \FROM TemplateVar \
                                                   \WHERE ((TemplateVar.data IS NULL AND :plain IS NOT NULL) OR TemplateVar.data != :plain) AND TemplateVar.id = :template \
                                             \ON CONFLICT(template, parent) DO UPDATE SET data = :value"
                                                      [":template" := i, ":parent" := parent, ":value" := value, ":iv" := iv, ":plain" := plain]
                           return True
    [IndexVal i] -> execute conn "UPDATE ReportVar SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, report) >> return True
    [IndexArr i] -> execute conn "UPDATE ReportVars SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, report) >> return True
    [IndexTempVars i] -> execute conn "INSERT INTO ReportVars (template, parent, data, iv) VALUES (?, ?, ?, ?)" (i, report, value, iv) >> return True
                       
    [IndexVal i, IndexVal parent] -> execute conn "UPDATE ReportVar SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, parent) >> return True
    [IndexVal i, IndexArr parent] -> execute conn "UPDATE ReportVar SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, parent) >> return True
    [IndexArr i, IndexArr parent] -> execute conn "UPDATE ReportVars SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, parent) >> return True
    [IndexArr i, IndexVal parent] -> execute conn "UPDATE ReportVars SET data = ?, iv = ? WHERE id = ? AND parent = ?" (value, iv, i, parent) >> return True
    _ -> return False

addArray :: Connection -> Maybe EncryptionKey -> Int64 -> IndexPathType -> Maybe Text.Text -> IO IndexPathType
addArray conn encKey report path val = withTransaction conn $ do
  let reversedPath = reverse path
      endOfPath = take 2 $ reversedPath
  report' <- case endOfPath !! 0 of
               i@(IndexArr _) -> getParentReport conn i
               i@(IndexVal _) -> getParentReport conn i
               _ -> return $ Just report
  if Just report /= report'
    then throw $ VisibleError "The new array does not belong to the current report"
    else return ()
  (iv, val) <- case encKey of
                  Nothing -> return (Nothing, val)
                  Just key -> generateIv >>= \iv -> return (Just iv, case encryptData key iv val of Right r -> Just r ; Left _ -> Nothing)
  case (iv, val) of
    (Just _, Nothing) -> throw $ VisibleError "Encryption failed"
    _ -> return ()
  case endOfPath of
    IndexTempVars i:p -> do let parent = case p of
                                           [IndexArr p] -> p
                                           [IndexVal p] -> p
                                           [] -> report
                            executeNamed conn "INSERT INTO ReportVars (template, parent, data, iv) VALUES (:template, :parent, :data, :iv)" [":template" := i, ":parent" := parent, ":data" := val, ":iv" := iv]
                            newIndex <- lastInsertRowId conn
                            return $ reverse $ IndexArr newIndex:drop 1 reversedPath
    _ -> throw $ VisibleError "Not yet implemented"

-- Users

addUser :: Connection -> Text.Text -> Text.Text -> IO ()
addUser conn username password = withTransaction conn $ do
  salt <- flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))
  let salt' = Text.pack salt
  execute conn "INSERT INTO User (username, salt, passhash) VALUES (?, ?, ?)" (username, salt', show $ hashPasswordAndSalt password salt')
  id <- lastInsertRowId conn
  (pub, priv) <- generateKeyPair id password
  execute conn "UPDATE User SET publicKey = ?, privateKey = ? WHERE id = ?" (show pub, priv, id)

updateUserPassword :: Connection -> Text.Text -> Text.Text -> Text.Text -> IO Bool
updateUserPassword conn username oldpass newpass = withTransaction conn $ do
  u <- getUserWithPassword conn username oldpass
  case u of
    Nothing -> return False
    Just u -> do
      salt <- flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))
      let salt' = Text.pack salt
      case userKey u of
        Nothing -> return ()
        Just (_, privKey) -> let privDecrypted = decryptPrivateKey (userId u) oldpass privKey
                                 newKey = case privDecrypted of
                                            Just privDecrypted' -> Just $ encryptPrivateKey (userId u) newpass privDecrypted'
                                            Nothing -> Nothing
                               in case newKey of
                                    Just k -> execute conn "UPDATE User SET privateKey = ? WHERE id = ?" (newKey, userId u)
                                    Nothing -> throw $ VisibleError "Could not reencrypt the private key"
      execute conn "UPDATE User SET salt = ?, passhash = ? WHERE id = ?" (salt', show $ hashPasswordAndSalt newpass salt', userId u)
      c <- changes conn
      return $ c /= 1

-- Adding reports

addReport :: Connection -> Int64 -> Text.Text -> User -> Bool -> IO Int64
addReport conn template title owner encrypted = withTransaction conn $ do
  execute conn "INSERT INTO Report (template, name, owner) VALUES (?, ?, ?)" (template, title, userId owner)
  reportId <- lastInsertRowId conn
  case encrypted of
    False -> return reportId
    True -> do
      (pub, _) <- case userKey owner of
                    Just u -> return u
                    Nothing -> throw $ VisibleError "You can't create encrypted reports without a public key associated to you."
      key <- generateSharedKey
      key' <- encryptSharedKey key pub
      execute conn "INSERT INTO ReportKey (user, report, key) VALUES (?, ?, ?)" (userId owner, reportId, key')
      escrowKey <- query conn "SELECT User.id, User.publicKey FROM User, SettingEscrowAccount WHERE User.id = SettingEscrowAccount.user AND User.id != ?" (Only $ userId owner)
      case escrowKey of
        [(escrowId, Just escrowPublic)] -> do
            escKey <- encryptSharedKey key (read escrowPublic)
            execute conn "INSERT INTO ReportKey (user, report, key) VALUES (?, ?, ?)" (escrowId :: Int64, reportId, escKey)
        [(_, Nothing)] -> throw $ VisibleError "Escrow account not correctly set. Need a public key assigned."
        _ -> return ()
      return reportId

-- Autofill
editSavedTemplateVars :: Connection -> (Maybe SavedVars -> (Maybe SavedVars, a)) -> Int64 -> Int64 -> IO a
editSavedTemplateVars conn callback template savedVar = withTransaction conn $ do
  vars <- getSavedTemplateVars conn template (Just savedVar)
  case vars of
    [p] -> do let (res, ret) = callback (Just p)
              case res of
                Nothing -> return ret
                Just res' -> do
                  executeNamed conn "UPDATE SavedVar SET name = :name, description = :description, data = :data WHERE id = :id AND templateVars = :templateVars"
                                    [":name" := savedVarsName res'
                                    ,":description" := savedVarsDescription res'
                                    ,":data" := savedVarsData res'
                                    ,":id" := savedVar
                                    ,":templateVars" := template]
                  execute conn "DELETE FROM SavedTemplateVar WHERE savedVar = ?" (Only savedVar)
                  flip mapM (savedVarsVar res') $ \(tempVar, d) -> do
                    execute conn "INSERT INTO SavedTemplateVar (savedVar, templateVar, data) VALUES (?, ?, ?)" (savedVar, tempVar, d)
                  return ret
    [] -> let (_, a) = callback Nothing
            in return a

addSavedTemplateVars :: Connection -> Int64 -> SavedVars -> IO Int64
addSavedTemplateVars conn template saved = withTransaction conn $ do
  execute conn "INSERT INTO SavedVar (templateVars, name, description, data) VALUES (?, ?, ?, ?)"
               (savedVarsTemplate saved, savedVarsName saved, savedVarsDescription saved, savedVarsData saved)
  newId <- lastInsertRowId conn
  flip mapM (savedVarsVar saved) $ \(tempVar, d) -> do
    execute conn "INSERT INTO SavedTemplateVar (savedVar, templateVar, data) VALUES (?, ?, ?)" (newId, tempVar, d)
  return newId

copySavedVarsToReport :: Connection -> Maybe EncryptionKey -> Int64 -> IndexPathType -> Int64 -> IO (Maybe IndexPathType)
copySavedVarsToReport conn encKey report path savedVars = withTransaction conn $ do
  savedVarData <- query conn "SELECT data FROM SavedVar WHERE id = ?" (Only savedVars)
  case savedVarData of
    [(Only d)] -> do
        succ <- setVariable conn encKey report path d
        if not succ
          then return Nothing
          else do
            varId <- lastInsertRowId conn
            let varId' = (take (length path - 1) path) ++ [IndexArr varId]
            vars <- query conn "SELECT data, templateVar FROM SavedTemplateVar WHERE savedVar = ?" (Only savedVars)
            flip mapM vars $ \(d, idx) -> do
              let path' = varId' ++ [IndexTempVar idx]
              setVariable conn encKey report path' (d :: Maybe Text.Text)
            return $ Just varId'

    _ -> return Nothing
