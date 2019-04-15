{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ReportEditor where

import Common
import Database.Resolver
import Database.Writer
import Database.Types
import Database.Encryption
import Csrf
import Redirect
import SignedData
import TemplateFiles
import Encryption

import Data.Default.Class (Default(..), def)
import Text.Ginger.GVal (toGVal, ToGVal(..), dict, fromFunction)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', atomicWriteIORef, IORef())
import System.FilePath (isPathSeparator, normalise)
import System.Random (randomRIO)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Maybe (isJust, maybe, fromJust, mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8      as C8
import Safe (headMay)

import Debug.Trace

listReports :: CsrfFormApplication
listReports csrf context req f = do
  reports <- getReports (sessionDbConn context)
  templates <- getMainTemplates (sessionDbConn context)
  let lookup :: TemplateLookupType p
      lookup name = case name of
                      "reports" -> return $ toGVal reports
                      "templates" -> return $ toGVal templates
                      "csrf" -> return $ toGVal csrf
                      _ -> return $ def
  result <- runTemplate context Nothing "list_reports" lookup
  f $ responseText status200 [] result

listReports_ :: CsrfVerifiedApplication
listReports_ (params, _) context req f = do
  case (lookup "name" params, lookup "template" params, sessionUser context, isJust $ lookup "encrypted" params) of
    (Just name, Just template, Just user, encrypted) -> do
      reportId <- addReport (sessionDbConn context) (read $ Text.unpack template) name user encrypted
      redirect (Text.append "/report/" $ Text.pack $ show reportId) req f
    _ -> redirectSame req f

data TemplateDataFields = DataFieldBuilder { fieldCheckbox :: [Text.Text],
                                             fieldValue :: [Text.Text],
                                             fieldFile :: [Text.Text] }
                        | DataFieldAlreadyUsed deriving (Show, Read)
instance Default TemplateDataFields where
  def = DataFieldBuilder [] [] []

dataFieldModifier hmac ref func = do
  let modifyMaybe modifier key value = liftRun $ atomicModifyIORef' ref $ \ref ->
                                           (flip (Map.insert key)
                                                 ref
                                                 $ modifier (Map.findWithDefault def key ref)
                                                            $ asText value
                                           , value)
      modify modifier val = case val of
                              [(_, value)] -> modifyMaybe modifier Nothing value
                              [(_, value), (Just "form", form)] -> modifyMaybe modifier (Just $ asText form) value
                              _ -> throw $ VisibleError $ Text.concat ["Incorrect use of function ", func]
  case func of
    "add_value" -> return $ fromFunction $ modify $ \d v -> d { fieldValue = v:fieldValue d }
    "add_checkbox" -> return $ fromFunction $ modify $ \d v -> d { fieldCheckbox = v:fieldCheckbox d }
    "add_file" -> return $ fromFunction $ modify $ \d v -> d { fieldFile = v:fieldFile d }
    "new_form" -> return $ fromFunction $ \_ -> let doInsert = do name <- flip mapM [1..5] $ (\_ -> randomRIO ('a', 'z'))
                                                                  let key = Just $ Text.pack name
                                                                  succ <- atomicModifyIORef' ref $ \ref -> do
                                                                    case Map.lookup key ref of
                                                                      Nothing -> (Map.insert key def ref, Just name)
                                                                      Just _ -> (ref, Nothing)
                                                                  case succ of
                                                                    Just name -> return $ toGVal name
                                                                    Nothing -> doInsert
                                                  in liftRun doInsert

    "signed_fields" -> return $ fromFunction $ \val -> do
                                  key <- case val of
                                           [(_, form)] -> return $ Just $ asText form
                                           [] -> return Nothing
                                           _ -> throw $ VisibleError $ Text.concat ["Incorrect use of function ", func]
                                  newMap <- liftRun $ atomicModifyIORef' ref $
                                                        \ref -> case Map.lookup key ref of
                                                                  Just (var@DataFieldBuilder { fieldCheckbox = cb
                                                                                   , fieldValue = tb
                                                                                   , fieldFile = fs }) -> (Map.insert key DataFieldAlreadyUsed ref, Right $ show $ signData hmac var)
                                                                  Just DataFieldAlreadyUsed -> (ref, Left "This field has already been signed")
                                                                  Nothing -> (ref, Left "Tried to sign a field which does not exist")
                                  case newMap of
                                    Left err -> throw $ VisibleError err
                                    Right j -> return $ toGVal j
    _ -> throw $ VisibleError $ Text.concat ["Variable ", func, " does not exist"]

editReport :: Maybe Int64 -> [Text.Text] -> CsrfFormApplicationWithEncryptedKey
editReport template args id key csrf context req f = do
  encryptionKey <- getUserEncryptionKeyFor (sessionDbConn context) (fromJust $ sessionUser context) id
  reportAndVars <- getReportAndVariables (sessionDbConn context) id template encryptionKey
  toSaveMvar <- newIORef (Map.fromList [(Nothing, def)] :: Map.Map (Maybe Text.Text) (TemplateDataFields))
  let getSavedVars context' = do
        cachedSavedVars <- newIORef (Nothing, def :: GVal (Run p IO Html))
        return $ do
          reportVars <- liftRun $ readIORef context'
          (cachedReportVars, savedVars) <- liftRun $ readIORef cachedSavedVars
          let reportVars' = reportContextVariable reportVars
          if Just reportVars' == cachedReportVars
            then return savedVars
            else do
              let recursiveFindArrs var = let children = concat $ map recursiveFindArrs $ Map.elems $ reportVarVariables var
                                            in case reportVarArray var of
                                                                     Just (idx, vars) -> idx:((concat $ map recursiveFindArrs vars) ++ children)
                                                                     Nothing -> children
                  arrs = Map.map recursiveFindArrs reportVars'
              saved <- liftRun $ flip mapM arrs $ mapM $ \idx -> case last idx of
                                                  IndexTempVars i -> getSavedTemplateVars (sessionDbConn context) i Nothing >>= \saved -> return $ Just (Text.pack $ show $ idx, saved)
                                                  _ -> return Nothing
              let saved' = toGVal $ Map.map (Map.fromList . mapMaybe Prelude.id) saved
              liftRun $ atomicWriteIORef cachedSavedVars $ (Just reportVars', saved')
              return saved'
      (rpc, args') = case args of
                       "rpc":a -> (True, a)
                       a -> (False, a)
  case reportAndVars of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find report"
    Just (report, context') -> do
      let lookup :: (Run p IO Html (GVal (Run p IO Html))) -> VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup savedVarsFetcher name = case name of
                          "report" -> return $ toGVal report
                          "variables" -> liftRun $ readIORef context' >>= return . toGVal . (Map.map IndexedReportVar) . reportContextVariable
                          "custom_variables" -> return $ toGVal [("image_1", "no")]
                          "csrf" -> return $ toGVal csrf
                          "args" -> return $ toGVal args'
                          "rpc" -> return $ toGVal rpc
                          "saved_vars" -> savedVarsFetcher
                          -- "delete_template_var" -> \vars -> case vars of
                          --                                     ["delete_id", var] -> do
                          --                                       let var' = read $ asText var :: IndexPathType
                          --                                       case last var' of
                          --                                         IndexVal i -> throw $ VisibleError "Not yet implemented."
                          --                                         IndexArr i -> throw $ VisibleError "Not yet implemented."
                          --                                         _ -> throw $ VisibleError "Invalid report ID."
                          --                                     _ -> throw $ VisibleError "Function delete_template_var expects one named variable, (delete_id=varid)"
                          _ -> dataFieldModifier (sessionHasher context) toSaveMvar name
          includer parent name = case Text.split isPathSeparator $ Text.pack $ normalise name of
                                   ["default"] -> parent name
                                   ["template_curr"] -> return $ Just $ Text.unpack $ templateEditor $ reportTemplate report
                                   ["template", sub] -> getTemplateEditor (sessionDbConn context) context' encryptionKey sub True >>= return . (maybe Nothing $ Just . Text.unpack)
                                   _ -> return Nothing
      savedVarsFetcher <- getSavedVars context'
      result <- runTemplate context (Just includer) "edit_report" $ lookup savedVarsFetcher
      f $ responseText status200 [] result
    
saveReport :: Int64 -> CsrfVerifiedApplication
saveReport id (params, files) context req f = do
  encryptionKey <- getUserEncryptionKeyFor (sessionDbConn context) (fromJust $ sessionUser context) id
  variables <- case lookup "fields" params of
                 Just f -> case verifySignedData (sessionHasher context) (read $ Text.unpack f) of
                             Just v -> return $ getSignedData v
                             Nothing -> throw $ VisibleError "I don't think that's something I signed..."
                 Nothing -> throw $ VisibleError "No fields received"
  flip mapM (fieldValue variables) $ \variable -> do
    setVariable (sessionDbConn context) encryptionKey id (read $ Text.unpack variable) $ lookup variable params
  flip mapM (fieldCheckbox variables) $ \variable -> do
    setVariable (sessionDbConn context) encryptionKey id (read $ Text.unpack variable) $ if isJust $ lookup variable params
                                                                             then Just ("1" :: Text.Text)
                                                                             else Just "0"
  flip mapM (fieldFile variables) $ \file -> do
    case lookup (Encoding.encodeUtf8 file) files of
      Just f -> if (fileName f == C8.empty || fileName f == C8.pack "\"\"") && fileContent f == LC8.empty -- Bug in Wai/Warp makes filenames contain "" when really empty
                  then return True
                  else setVariable (sessionDbConn context) encryptionKey id (read $ Text.unpack file) $ Just $ Encoding.decodeLatin1 $ LC8.toStrict $ fileContent f
      Nothing -> return True
  redirectSame req f

reportAddList rid (params, _) context req f = do
  encryptionKey <- getUserEncryptionKeyFor (sessionDbConn context) (fromJust $ sessionUser context) rid
  variables <- case lookup "idx" params of
                 Just a -> return $ read $ Text.unpack a
                 Nothing -> throw $ VisibleError "No list to add"
  _ <- addArray (sessionDbConn context) encryptionKey rid variables Nothing
  redirectBack req f
