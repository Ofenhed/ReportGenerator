{-# LANGUAGE OverloadedStrings #-}
module Database.Resolver where

import Database.Types
import Database.Encryption
import Types
import UserType

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import Control.Exception

import qualified Data.Text            as Text
import qualified Data.Map             as Map
import qualified Data.Text.Encoding   as Encoding

import Debug.Trace

data EncryptionException = EncryptionMissmatchException
                         | CouldNotDecryptException deriving Show
instance Exception EncryptionException

getAllChildVariable :: Connection -> TemplateVarParent -> Int64 -> Maybe EncryptionKey -> IO [(Int64, Maybe Int64, Text.Text, Maybe Text.Text)]
getAllChildVariable conn template parent key = do
  let (target, val) = templateParentName template
  variables <- query conn (Query $ Text.concat ["SELECT TemplateVar.id, ReportVar.id, TemplateVar.name, ReportVar.iv, ReportVar.data, TemplateVar.data FROM TemplateVar LEFT JOIN ReportVar ON ReportVar.template = TemplateVar.id AND ReportVar.parent = ? WHERE TemplateVar.", target, " = ?"]) (parent, val)
  flip mapM variables $ \(tid, rid, name, iv, rd, td) -> case traceShowId (key, iv, rd) of
    (Just key, Just iv, Just rd') -> case decryptData key iv rd' of
                                       Just dec -> return (tid, rid, name, dec)
                                       Nothing -> throw CouldNotDecryptException
    (Just key, Just iv, Nothing) -> return (tid, rid, name, Nothing)
    (Nothing, Nothing, _) -> case rid of
                              Just _ -> return (tid, rid, name, rd)
                              Nothing -> return (tid, rid, name, td)
    (Just _, Nothing, Nothing) -> return (tid, rid, name, td)
    _ -> throw EncryptionMissmatchException

getAllChildVariables :: Connection -> TemplateVarParent -> Int64 -> Maybe EncryptionKey -> IO [(Int64, Text.Text, [(Int64, Maybe Text.Text)])]
getAllChildVariables conn template parent key = do
  let (target, val) = case template of
                        TemplateVarParent i -> ("template", i)
                        TemplateVarParentVar i -> ("templateVar", i)
                        TemplateVarParentVars i -> ("templateVars", i)
  vars <- query conn (Query $ Text.concat ["SELECT TemplateVars.id, TemplateVars.name FROM TemplateVars WHERE ", target, " = ?"]) (Only val)
  flip mapM vars $ \(tId, tName) -> do
    values <- query conn "SELECT id, data, iv FROM ReportVars WHERE parent = ? AND template = ? ORDER BY weight ASC" (parent, tId)
    values' <- flip mapM values $ \(id, d, iv) -> case traceShowId (key, iv, d) of
                 (Just key, Just iv, Just d') -> case decryptData key iv d' of
                                          Just dec -> return (id, dec)
                                          Nothing -> throw CouldNotDecryptException
                 (Just key, Just iv, Nothing) -> return (id, d)
                 (Nothing, Nothing, _) -> return (id, d)
                 _ -> throw EncryptionMissmatchException
    return $ (tId, tName, values')

getReport :: Connection -> Int64 -> Maybe Int64 -> Maybe EncryptionKey -> IO (Maybe (Report, IOReportContext))
getReport conn reportId tempId encKey = do
  var <- case tempId of
           Nothing -> query conn "SELECT Report.id, Report.name, Template.* FROM Report LEFT JOIN Template ON Report.template = Template.id WHERE Report.id = ?" (Only reportId)
           Just i -> query conn "SELECT Report.id, Report.name, Template.* FROM Report INNER JOIN Template ON Template.id = ? WHERE Report.id = ?" (i, reportId)
  context <- newIORef $ ReportContext { reportContextId = reportId, reportContextVariable = Map.empty, reportContextCustomVariable = Map.empty }
  case var of
    [] -> return Nothing
    [r] -> do
      var <- query conn "SELECT CustVar.name, CustVar.data FROM CustVar WHERE CustVar.report = ?" (Only reportId)
      atomicModifyIORef' context $ \c -> (c { reportContextCustomVariable = Map.fromList var }, ())
      includeTemplateVariables conn context (templateIncludeName $ reportTemplate r) (templateId $ reportTemplate r) encKey
      return $ Just (r, context)

includeTemplateVariables conn context mapKey template encKey = do
  context' <- readIORef context
  let findVariablesRecursive from parent path = do
        var <- getAllChildVariable conn from parent encKey
        var' <- flip mapM var $ \(tempId, varId, name, d) -> do
            let path' = path ++ [case varId of Just varId' -> IndexVal varId' ; Nothing -> IndexTempVar tempId]
            otherVar <- case varId of Nothing -> return $ Map.empty ; Just varId' -> findVariablesRecursive (TemplateVarParentVar tempId) varId' path'
            return $ (name, ReportVar { reportVarVariables = otherVar, reportVarValue = Just (path', d), reportVarArray = Nothing })
        vars <- getAllChildVariables conn from parent encKey
        vars' <- flip mapM vars $ \(tempId, name, v) -> do
            v' <- flip mapM v $ \(rId, val) -> do
                let path' = path ++ [IndexArr rId]
                others <- findVariablesRecursive (TemplateVarParentVars tempId) rId path'
                return $ ReportVar { reportVarVariables = others, reportVarValue = Just (path', val), reportVarArray = Nothing }
            return (name, ReportVar { reportVarVariables = Map.empty, reportVarValue = Nothing, reportVarArray = Just (path ++ [IndexTempVars tempId], v') })
        let merged = Map.unionWith (\var vars -> var { reportVarArray = reportVarArray vars }) (Map.fromList var') (Map.fromList vars')
        return merged
  vars <- findVariablesRecursive (TemplateVarParent template) (reportContextId context') []
  atomicModifyIORef' context $ \c -> (c { reportContextVariable = Map.insert mapKey (ReportVar { reportVarVariables = vars, reportVarValue = Nothing, reportVarArray = Nothing}) (reportContextVariable c) }, ())

getTemplate :: Connection -> IOReportContext -> Maybe EncryptionKey -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplate conn context encKey template included = do
  var <- query conn "SELECT id, includeName, source FROM Template WHERE includeName = ? AND (? = 0 OR includable = 1)" (template, included)
  context' <- readIORef context
  case var of
    [] -> return $ Nothing
    [(tId, name, v)] -> do
      includeTemplateVariables conn context name tId encKey
      c <- readIORef context
      return $ Just v

-- Editor

getTemplateEditor :: Connection -> IOReportContext -> Maybe EncryptionKey -> Text.Text -> Bool -> IO (Maybe Text.Text)
getTemplateEditor conn context encKey template included = do
  var <- query conn "SELECT id, includeName, editor FROM Template WHERE includeName = ? AND (? = 0 OR includable = 1)" (template, included)
  context' <- readIORef context
  case var of
    [] -> return $ Nothing
    [(tId, name, v)] -> do
      includeTemplateVariables conn context name tId encKey
      c <- readIORef context
      return $ Just v

getTemplates :: Connection -> IO [Template]
getTemplates conn = query_ conn "SELECT * FROM Template" :: IO [Template]

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

getTemplateAndVariables :: Connection -> Int64 -> IO (Maybe (Template, TemplateVarTree))
getTemplateAndVariables conn id = do
  template <- query conn "SELECT * FROM Template WHERE Template.id = ?" (Only id) :: IO [Template]
  case template of
    [] -> return Nothing
    [template'] -> do
      let findVarsRecursive parent = do
            let (target, val) = templateParentName parent
            templateVars <- query conn (Query $ Text.concat ["SELECT id, name, description FROM TemplateVars WHERE ", target, " = ?"]) (Only val)
            templateVar <- query conn (Query $ Text.concat ["SELECT id, name, description, data FROM TemplateVar WHERE ", target, " = ?"]) (Only val)
            vars' <- flip mapM templateVars $ \(varsId, varsName, varsDesciption) ->
                                                findVarsRecursive (TemplateVarParentVars varsId) >>=
                                                  \children -> return $ TemplateVars { templateVarsId = varsId
                                                                                     , templateVarsName = varsName
                                                                                     , templateVarsDescription = varsDesciption
                                                                                     , templateVarsChildren = children }
                                                  
            var' <- flip mapM templateVar $ \(varId, varName, varDesciption, varDefault) ->
                                                findVarsRecursive (TemplateVarParentVar varId) >>=
                                                  \children -> return $ TemplateVar { templateVarId = varId
                                                                                    , templateVarName = varName
                                                                                    , templateVarDescription = varDesciption
                                                                                    , templateVarDefault = varDefault
                                                                                    , templateVarChildren = children }
            return (vars', var')
      vars <- findVarsRecursive $ TemplateVarParent $ templateId template'
      return $ Just (template', vars)
      
getReports :: Connection -> IO [Report]
getReports conn = query_ conn "SELECT Report.id, Report.name, Template.* FROM Report LEFT JOIN Template ON Template.id == Report.template"

getParentReport conn idx = do
  let (first_select, i) = case idx of
                     IndexVal i -> ("ReportVar.parent, TemplateVar.templateVar, TemplateVar.templateVars, NULL, NULL, Report.id FROM \
                                    \ReportVar, TemplateVar \
                                    \LEFT JOIN Report ON Report.id = ReportVar.parent AND TemplateVar.template IS NOT NULL \
                                    \WHERE ReportVar.id = ? AND TemplateVar.id = ReportVar.template", i)
                     IndexArr i -> ("ReportVars.parent, NULL, NULL, TemplateVars.templateVar, TemplateVars.templateVars, Report.id FROM \
                                    \ReportVars, TemplateVars \
                                    \LEFT JOIN Report ON Report.id = ReportVars.parent AND TemplateVars.template IS NOT NULL \
                                    \WHERE ReportVars.id = ? AND TemplateVars.id = ReportVars.template", i)
  parent_id <- query conn (Query $ Text.concat
               ["WITH RECURSIVE p_tree(p, vv, va, av, aa, parent) AS \
                \(SELECT ", first_select,
                " UNION SELECT \
                         \(CASE WHEN ReportVar.id IS NOT NULL THEN ReportVar.parent ELSE ReportVars.parent END) AS report_parent, \
                         \TemplateVar.templateVar, TemplateVar.templateVars, \
                         \TemplateVars.templateVar, TemplateVars.TemplateVars, \
                         \Report.id \
                       \FROM p_tree \
                         \LEFT JOIN TemplateVars ON TemplateVars.id = va OR TemplateVars.id = aa \
                         \LEFT JOIN TemplateVar ON TemplateVar.id = vv OR TemplateVar.id = av \
                         \LEFT JOIN ReportVars ON ReportVars.id = p_tree.p AND ReportVars.template = TemplateVars.id \
                         \LEFT JOIN ReportVar ON ReportVar.id = p_tree.p AND ReportVar.template = TemplateVar.id \
                         \LEFT JOIN Report ON Report.id = report_parent AND \
                                    \(TemplateVar.template IS NOT NULL OR TemplateVars.template IS NOT NULL) \
                       \WHERE report_parent IS NOT NULL) \
                \SELECT parent FROM p_tree WHERE parent IS NOT NULL"]) (Only i)
  case parent_id of
    [Only p] -> return $ Just p
    [] -> return Nothing

getParentTemplate conn idx = do
  let (first_select, i) = case idx of
                     IndexVal i -> ("TemplateVar.templateVar, TemplateVar.templateVars, NULL, NULL, Template.id FROM \
                                    \ReportVar, TemplateVar \
                                    \LEFT JOIN Template ON Template.id = TemplateVar.template \
                                    \WHERE ReportVar.id = ? AND TemplateVar.id = ReportVar.template", i)
                     IndexArr i -> ("NULL, NULL, TemplateVars.templateVar, TemplateVars.templateVars, Template.id FROM \
                                    \ReportVars, TemplateVars \
                                    \LEFT JOIN Template ON Template.id = TemplateVars.template \
                                    \WHERE ReportVars.id = ? AND TemplateVars.id = ReportVars.template", i)
                     IndexTempVar i -> ("?, NULL, NULL, NULL, NULL", i)
                     IndexTempVars i -> ("NULL, ?, NULL, NULL, NULL", i)
  template_id <- query conn (Query $ Text.concat
                 ["WITH RECURSIVE p_tree(vv, va, av, aa, parent) AS \
                  \(SELECT ", first_select,
                  " UNION SELECT \
                           \TemplateVar.templateVar, TemplateVar.templateVars, \
                           \TemplateVars.templateVar, TemplateVars.templateVars, \
                           \Template.id \
                         \FROM p_tree \
                           \LEFT JOIN TemplateVars ON TemplateVars.id = va OR TemplateVars.id = aa \
                           \LEFT JOIN TemplateVar ON TemplateVar.id = vv OR TemplateVar.id = av \
                           \LEFT JOIN Template ON Template.id = TemplateVars.template OR Template.id = TemplateVar.template) \
                  \SELECT parent FROM p_tree WHERE parent IS NOT NULL;"]) (Only i)
  case template_id of
    [Only p] -> return $ Just p
    [] -> return Nothing

-- User

getUserWithPassword :: Connection -> Text.Text -> Text.Text -> IO (Maybe User)
getUserWithPassword conn username password = do
  user <- query conn "SELECT id, salt from User where username = ?" (Only username) :: IO [(Int64, Text.Text)]
  case user of
    [] -> return Nothing
    [(uid, salt)] -> do
      let hash = hashPasswordAndSalt password salt
      verifiedUser <- query conn "SELECT id, username, passid, publicKey, privateKey FROM User WHERE id = ? AND passhash = ?" (uid, show hash)
      case verifiedUser of
        [] -> return Nothing
        [(id, name, pass, Just pub, Just priv)] -> return $ Just $ User { userId = id, userUsername = name, userPassId = pass, userKey = Just (read pub, priv) }
        [(id, name, pass, Nothing, Nothing)] -> return $ Just $ User { userId = id, userUsername = name, userPassId = pass, userKey = Nothing }

getUserFromId :: Connection -> Int64 -> Maybe Int64 -> IO (Maybe User)
getUserFromId conn uid passid = do
  user <- queryNamed conn "SELECT id, username, passid, publicKey, privateKey from User WHERE id = :id AND (:passid IS NULL OR passid = :passid)" [":id" := uid, ":passid" := passid]
  case user of
    [] -> return Nothing
    [(id, name, pass, Just pub, Just priv)] -> return $ Just $ User { userId = id, userUsername = name, userPassId = pass, userKey = Just (read pub, priv) }
    [(id, name, pass, Nothing, Nothing)] -> return $ Just $ User { userId = id, userUsername = name, userPassId = pass, userKey = Nothing }

getUserEncryptionKey conn user rid = do
  res <- query conn "SELECT key FROM ReportKey WHERE user = ? AND report = ?" (userId user, rid)
  case res of
    [(Only key)] -> return $ Just key
    [] -> return Nothing
