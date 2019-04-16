{-# LANGUAGE OverloadedStrings #-}
module Database.Resolver where

import Database.Types
import Database.Encryption
import Database.Extra
import Types
import UserType

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import Control.Exception
import Data.Maybe (mapMaybe)

import qualified Data.Text            as Text
import qualified Data.Map             as Map
import qualified Data.Text.Encoding   as Encoding

import Debug.Trace

data EncryptionException = EncryptionMissmatchException
                         | CouldNotDecryptException deriving Show
instance Exception EncryptionException

getAllChildVariable :: Connection -> TemplateVarParent -> Int64 -> Maybe EncryptionKey -> IO [(Int64, Maybe Int64, Text.Text, Maybe Text.Text)]
getAllChildVariable conn template parent key = do
  getFetcher <- prepareGetAllChildVariable conn key
  getFetcher $ \fetcher -> fetcher template parent

prepareGetAllChildVariable :: Connection -> Maybe EncryptionKey -> IO (((TemplateVarParent -> Int64 -> IO [(Int64, Maybe Int64, Text.Text, Maybe Text.Text)]) -> IO a) -> IO a)
prepareGetAllChildVariable conn key = do
  let statement' = openStatement conn "SELECT TemplateVar.id, ReportVar.id, TemplateVar.name, ReportVar.iv, ReportVar.data, TemplateVar.data \
                                         \FROM TemplateVar \
                                         \LEFT JOIN ReportVar \
                                           \ON ReportVar.template = TemplateVar.id AND ReportVar.parent = :parent \
                                         \WHERE CASE :target \
                                                  \WHEN 'template' THEN TemplateVar.template = :template \
                                                  \WHEN 'templateVar' THEN TemplateVar.templateVar = :template \
                                                  \WHEN 'templateVars' THEN TemplateVar.templateVars = :template \
                                                  \END"
      bracketeer statement template parent = do
        let (target, val) = templateParentName template :: (Text.Text, Int64)
        withBindNamed statement [":parent" := parent, ":target" := target, ":template" := val] $
          flip (handleRows statement) [] $ \(tid, rid, name, iv, rd, td) ->
                                              case (key, iv, rd) of
                                                (Just key, Just iv, Just rd') -> case decryptData key iv rd' of
                                                                                   Right dec -> return (tid, rid, name, dec)
                                                                                   Left _ -> throw CouldNotDecryptException
                                                (Just key, Just iv, Nothing) -> return (tid, rid, name, Nothing)
                                                (Nothing, Nothing, _) -> case rid of
                                                                           Just _ -> return (tid, rid, name, rd)
                                                                           Nothing -> return (tid, rid, name, td)
                                                (Just _, Nothing, Nothing) -> return (tid, rid, name, td)
                                                _ -> throw EncryptionMissmatchException
  return $ \func -> bracket statement' closeStatement $ \statement -> func $ bracketeer statement


getAllChildVariables :: Connection -> TemplateVarParent -> Int64 -> Maybe EncryptionKey -> IO [(Int64, Text.Text, [(Int64, Maybe Text.Text)])]
getAllChildVariables conn template parent key = do
  getFetcher <- prepareGetAllChildVariables conn key
  getFetcher $ \fetcher -> fetcher template parent

prepareGetAllChildVariables :: Connection -> Maybe EncryptionKey -> IO (((TemplateVarParent -> Int64 -> IO [(Int64, Text.Text, [(Int64, Maybe Text.Text)])]) -> IO a) -> IO a)
prepareGetAllChildVariables conn key = do
  let templateStatement' = openStatement conn "SELECT TemplateVars.id, TemplateVars.name FROM TemplateVars \
                                                 \WHERE CASE :target \
                                                          \WHEN 'template' THEN TemplateVars.template = :template \
                                                          \WHEN 'templateVar' THEN TemplateVars.templateVar = :template \
                                                          \WHEN 'templateVars' THEN TemplateVars.templateVars = :template \
                                                          \END"
      reportStatement' = openStatement conn "SELECT id, data, iv FROM ReportVars WHERE parent = ? AND template = ?"
      bracketeer templateStatement reportStatement template parent = do
        let (target, val) = templateParentName template :: (Text.Text, Int64)
        withBindNamed templateStatement [":target" := target, ":template" := val] $
          flip (handleRows templateStatement) [] $ \(tId, tName) -> do
            values <- withBind reportStatement (parent, tId) $
              flip (handleRows reportStatement) [] $ \(id, d, iv) ->
                case (key, iv, d) of
                  (Just key, Just iv, Just d') -> case decryptData key iv d' of
                                                    Right dec -> return (id, dec)
                                                    Left _ -> throw CouldNotDecryptException
                  (Just key, Just iv, Nothing) -> return (id, d)
                  (Nothing, Nothing, _) -> return (id, d)
                  _ -> throw EncryptionMissmatchException
            return (tId, tName, values)
  return $ \func -> bracket templateStatement' closeStatement $ \templateStatement ->
                    bracket reportStatement' closeStatement $ \reportStatement -> func $ bracketeer templateStatement reportStatement

getReportAndVariables :: Connection -> Int64 -> Maybe Text.Text -> Maybe EncryptionKey -> IO (Maybe (Report, IOReportContext))
getReportAndVariables conn reportId tempName encKey = do
  var <- case tempName of
           Nothing -> query conn "SELECT Report.id, Report.name, ReportKey.id IS NOT NULL, Template.* FROM Report LEFT JOIN Template ON Report.template = Template.id LEFT JOIN ReportKey ON Report.id = ReportKey.report WHERE Report.id = ?" (Only reportId)
           Just name -> query conn "SELECT Report.id, Report.name, ReportKey.id IS NOT NULL, Template.* FROM Report INNER JOIN Template ON Template.includeName = ? LEFT JOIN ReportKey ON Report.id = ReportKey.report WHERE Report.id = ?" (name, reportId)
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
  allChildVariableFetcher <- prepareGetAllChildVariable conn encKey
  allChildVariablesFetcher <- prepareGetAllChildVariables conn encKey
  let findVariablesRecursive funcs@(getAllChildVariable', getAllChildVariables') from parent path = do
        var <- getAllChildVariable' from parent
        var' <- flip mapM var $ \(tempId, varId, name, d) -> do
            let path' = path ++ [case varId of Just varId' -> IndexVal varId' ; Nothing -> IndexTempVar tempId]
            otherVar <- case varId of Nothing -> return $ Map.empty ; Just varId' -> findVariablesRecursive funcs (TemplateVarParentVar tempId) varId' path'
            return $ (name, ReportVar { reportVarVariables = otherVar, reportVarValue = Just (path', d), reportVarArray = Nothing })
        vars <- getAllChildVariables' from parent
        vars' <- flip mapM vars $ \(tempId, name, v) -> do
            v' <- flip mapM v $ \(rId, val) -> do
                let path' = path ++ [IndexArr rId]
                others <- findVariablesRecursive funcs (TemplateVarParentVars tempId) rId path'
                return $ ReportVar { reportVarVariables = others, reportVarValue = Just (path', val), reportVarArray = Nothing }
            return (name, ReportVar { reportVarVariables = Map.empty, reportVarValue = Nothing, reportVarArray = Just (path ++ [IndexTempVars tempId], v') })
        let merged = Map.unionWith (\var vars -> var { reportVarArray = reportVarArray vars }) (Map.fromList var') (Map.fromList vars')
        return merged
  vars <- allChildVariableFetcher $ \childVariableFetcher ->
          allChildVariablesFetcher $ \childVariablesFetcher ->
          findVariablesRecursive (childVariableFetcher, childVariablesFetcher) (TemplateVarParent template) (reportContextId context') []
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

getMainTemplates :: Connection -> IO [Template]
getMainTemplates conn = query_ conn "SELECT * FROM Template WHERE main_template = 1" :: IO [Template]

getTemplatesWithLists :: Connection -> IO [Template]
getTemplatesWithLists conn = query_ conn "WITH RECURSIVE \
                                            \has_arrays(ptemp, pvar, temp, has_vars) AS \
                                            \(SELECT Template.id, TemplateVar.id, Template.id, TemplateVars.id IS NOT NULL \
                                                \FROM Template \
                                                \LEFT JOIN TemplateVars \
                                                  \ON TemplateVars.template = Template.id \
                                                \LEFT JOIN TemplateVar \
                                                  \ON (TemplateVars.id IS NULL AND TemplateVar.template = Template.id) \
                                             \UNION SELECT NULL, TemplateVar.id, has_arrays.temp, TemplateVars.id IS NOT NULL \
                                                \FROM has_arrays \
                                                \LEFT JOIN TemplateVar \
                                                  \ON (has_arrays.has_vars = False AND (TemplateVar.template = has_arrays.ptemp OR TemplateVar.templateVar = has_arrays.pvar)) \
                                                \LEFT JOIN TemplateVars \
                                                  \ON (has_arrays.has_vars = False AND TemplateVars.templateVar = has_arrays.pvar)) \
                                          \SELECT Template.* FROM has_arrays, Template \
                                            \WHERE has_arrays.has_vars = True AND Template.id = has_arrays.temp GROUP BY has_arrays.temp;"

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
getReports conn = query_ conn "SELECT Report.id, Report.name, ReportKey.id IS NOT NULL, Template.* FROM Report LEFT JOIN Template ON Template.id = Report.template LEFT JOIN ReportKey ON Report.id = ReportKey.report"

getReport :: Connection -> Int64 -> IO (Maybe Report)
getReport conn id = do r <- query conn "SELECT Report.id, Report.name, ReportKey.id IS NOT NULL, Template.* FROM Report LEFT JOIN Template ON Template.id = Report.template LEFT JOIN ReportKey ON Report.id = ReportKey.report WHERE Report.id = ?" (Only id)
                       case r of
                         [r'] -> return $ Just r'
                         _ -> return Nothing


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

getUserEncryptionKeyFor conn user rid = do
  res <- query conn "SELECT key FROM ReportKey WHERE user = ? AND report = ?" (userId user, rid)
  case res of
    [(Only key)] -> return $ Just key
    [] -> return Nothing

-- Autofill
getTemplateVars :: Connection -> Int64 -> IO (Maybe (Template, TemplateVars))
getTemplateVars conn varsId = do
  templateId <- getParentTemplate conn (IndexTempVars varsId)
  case templateId of
    Nothing -> return Nothing
    Just templateId' -> do
      Just (template, vars) <- getTemplateAndVariables conn templateId'
      let findId (vars, var) = let vars' = foldr (\v state -> case state of
                                                                Nothing -> if templateVarsId v == varsId
                                                                             then Just v
                                                                             else findId $ templateVarsChildren v
                                                                x -> x)
                                                 Nothing
                                                 vars
                                   var' = foldr (\v state -> case state of Nothing -> findId $ templateVarChildren v ; x -> x)
                                                Nothing
                                                var
                                 in case (vars', var') of
                                      (x@(Just _), Nothing) -> x
                                      (Nothing, x@(Just _)) -> x
                                      (Nothing, Nothing) -> Nothing
                                      _ -> error "This should not happen with a well behaving database"
      case findId vars of
        Just x -> return $ Just (template, x)
        Nothing -> return Nothing

getSavedTemplateVars :: Connection -> Int64 -> Maybe Int64 -> IO [SavedVars]
getSavedTemplateVars conn tVars savedVarId = do
  parent <- queryNamed conn "SELECT id, name, description, data FROM SavedVar WHERE templateVars = :tempvar AND (:savedid IS NULL OR id = :savedid)"
                            [":tempvar" := tVars
                            ,":savedid" := savedVarId]
  flip mapM parent $ \(id, name, description, d) -> do
    children <- query conn "SELECT id, templateVar, data FROM SavedTemplateVar WHERE savedVar = ?" (Only id)
    return $ SavedVars { savedVarsId = id
                       , savedVarsTemplate = tVars
                       , savedVarsName = name
                       , savedVarsDescription = description
                       , savedVarsData = d
                       , savedVarsVar = Map.fromList $ flip map children $ \(id, templateVar, d) -> (templateVar, (id, d)) }
