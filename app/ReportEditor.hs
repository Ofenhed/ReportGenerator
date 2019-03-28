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

import Text.Ginger.GVal (toGVal, ToGVal(..), dict, fromFunction)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Maybe (isJust, maybe, fromJust)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8      as C8
import Safe (headMay)

instance ToGVal m Report where
  toGVal t = dict $ [("id", toGVal $ reportId t)
                    ,("name", toGVal $ reportName t)
                    ,("templateId", toGVal $ templateId $ reportTemplate t)
                    ,("templateIncludeName", toGVal $ templateIncludeName $ reportTemplate t)
                    ,("templateLongName", toGVal $ templateLongName $ reportTemplate t)
                    ,("editor", toGVal $ templateEditor $ reportTemplate t)]

listReports :: CsrfFormApplication
listReports csrf context req f = do
  reports <- getReports (sessionDbConn context)
  templates <- getTemplates (sessionDbConn context)
  let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
      lookup name = case name of
                      "reports" -> return $ toGVal reports
                      "templates" -> return $ toGVal templates
                      "csrf" -> return $ toGVal csrf
                      _ -> return $ def
  result <- runTemplate context Nothing "list_reports" lookup
  f $ responseText status200 [(hContentType, "text/html")] result

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

dataFieldModifier hmac ref func = do
  let modify modifier val = case val of
                              [(_, value)] -> liftRun $ atomicModifyIORef' ref $ \ref -> (modifier ref $ asText value, value)
                              _ -> throw $ VisibleError $ Text.concat ["Incorrect use of function ", func]
  case func of
    "add_value" -> return $ fromFunction $ modify (\d v -> d { fieldValue = v:fieldValue d })
    "add_checkbox" -> return $ fromFunction $ modify (\d v -> d { fieldCheckbox = v:fieldCheckbox d })
    "add_file" -> return $ fromFunction $ modify (\d v -> d { fieldFile = v:fieldFile d })

    "signed_fields" -> return $ fromFunction $ \_ -> do
                                  key <- liftRun $ atomicModifyIORef' ref $
                                                     \ref -> case ref of
                                                               var@DataFieldBuilder { fieldCheckbox = cb
                                                                                , fieldValue = tb
                                                                                , fieldFile = fs } -> (DataFieldAlreadyUsed, Just $ show $ signData hmac var)
                                                               DataFieldAlreadyUsed -> (DataFieldAlreadyUsed, Nothing)
                                  case key of
                                    Nothing -> throw $ VisibleError "Fields has already been signed"
                                    Just j -> return $ toGVal j
    _ -> throw $ VisibleError $ Text.concat ["Variable ", func, " does not exist"]

editReport :: Int64 -> Maybe Int64 -> [Text.Text] -> CsrfFormApplication
editReport id template args csrf context req f = do
  encryptionKey <- getUserEncryptionKeyFor (sessionDbConn context) (fromJust $ sessionUser context) id
  reportAndVars <- getReport (sessionDbConn context) id template encryptionKey
  toSaveMvar <- newIORef $ DataFieldBuilder { fieldCheckbox = [], fieldValue = [], fieldFile = [] }
  let (rpc, args') = case args of
                       "rpc":a -> (True, a)
                       a -> (False, a)
  case reportAndVars of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find report"
    Just (report, context') -> do
      let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup name = case name of
                          "report" -> return $ toGVal report
                          "variables" -> liftRun $ readIORef context' >>= return . toGVal . (Map.map IndexedReportVar) . reportContextVariable
                          "custom_variables" -> return $ toGVal [("image_1", "no")]
                          "csrf" -> return $ toGVal csrf
                          "args" -> return $ toGVal args'
                          "rpc" -> return $ toGVal rpc
                          -- "delete_template_var" -> \vars -> case vars of
                          --                                     ["delete_id", var] -> do
                          --                                       let var' = read $ asText var :: IndexPathType
                          --                                       case last var' of
                          --                                         IndexVal i -> throw $ VisibleError "Not yet implemented."
                          --                                         IndexArr i -> throw $ VisibleError "Not yet implemented."
                          --                                         _ -> throw $ VisibleError "Invalid report ID."
                          --                                     _ -> throw $ VisibleError "Function delete_template_var expects one named variable, (delete_id=varid)"
                          _ -> dataFieldModifier (sessionHasher context) toSaveMvar name
          includer parent name = case Text.splitOn "/" $ Text.pack name of
                                   [".", "default"] -> parent name
                                   [".", "template_curr"] -> return $ Just $ Text.unpack $ templateEditor $ reportTemplate report
                                   [".", "template", sub] -> getTemplateEditor (sessionDbConn context) context' encryptionKey sub True >>= return . (maybe Nothing $ Just . Text.unpack)
                                   _ -> return Nothing
      result <- runTemplate context (Just includer) "edit_report" lookup
      f $ responseText status200 [(hContentType, "text/html")] result
    
saveReport :: Int64 -> CsrfVerifiedApplication
saveReport id (params, files) context req f = do
  encryptionKey <- getUserEncryptionKeyFor (sessionDbConn context) (fromJust $ sessionUser context) id
  variables <- case lookup "fields" params of
                 Just f -> case getSignedData (sessionHasher context) (read $ Text.unpack f) of
                             Just v -> return $ v
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
