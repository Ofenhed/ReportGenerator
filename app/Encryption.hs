{-# LANGUAGE OverloadedStrings #-}
module Encryption (CsrfFormApplicationWithEnctyptedKey, CsrfVerifiedApplicationWithEnctyptedKey, createTemporaryKey, getWithDecryptionKey, handleKeyDecryption, clearDecryptionKeys) where

-- import Types
import Common
import Redirect
import Csrf
import Database.Encryption
import Database.Resolver
import Database.Types
import TemplateFiles

import Network.Wai (vault)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, File, FileInfo(..))
import Data.Maybe (mapMaybe, isJust)
import Control.Exception (throw)
import Control.DeepSeq (deepseq)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as Encoding
import System.Random

type CsrfFormApplicationWithEnctyptedKey = Int64 -> Maybe EncryptionKey -> CsrfFormApplication
type CsrfVerifiedApplicationWithEnctyptedKey = Int64 -> Maybe EncryptionKey -> CsrfFormApplication
sessionKeyName = "encryption_keys"

clearDecryptionKeys context req = do
  let Just (_, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
  sessionInsert sessionKeyName ""

queryDecryptionKey :: Report -> CsrfFormApplication
queryDecryptionKey report csrf context req f = do
  let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
      lookup name = case name of
                      "report" -> return $ toGVal report
                      "csrf" -> return $ toGVal csrf
                      _ -> return def
  result <- runTemplate context Nothing "unlock_encryption" lookup
  f $ responseText status200 [(hContentType, "text/html")] result
  
createTemporaryKey user password temp_password =   
  case userKey user of
    Nothing -> Nothing
    Just (_, privKey) -> case decryptPrivateKey (userId user) password privKey of
                           Nothing -> Nothing
                           Just p -> Just $ encryptPrivateKey (userId user) temp_password p

getWithDecryptionKey :: Int64 -> CsrfFormApplicationWithEnctyptedKey -> CsrfFormApplication
getWithDecryptionKey rid other csrf context req f = do
  report <- getReport (sessionDbConn context) rid
  report' <- case report of
               Just r -> return r
               Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find report"
  if reportEncrypted report' == 0
    then other rid Nothing csrf context req f
    else do
      let Just (sessionLookup, _) = Vault.lookup (sessionSession context) (vault req)
      key <- sessionLookup sessionKeyName
      case key of
            Nothing -> queryDecryptionKey report' csrf context req f
            Just s -> case reads $ Text.unpack s of
              [((rid', key'), [])] -> if rid == rid'
                                        then other rid (Just key') csrf context req f
                                        else queryDecryptionKey report' csrf context req f
              _ -> throw $ VisibleError "Could not read decryption key"

handleKeyDecryption :: Int64 -> CsrfVerifiedApplication
handleKeyDecryption rid (params, _) context req f = do
  case (lookup "pass" params, isJust $ lookup "lock_other" params, sessionUser context) of
    (Just pass, lock_other, Just user) -> do
        case userKey user of
          Nothing -> throw $ VisibleError "This user does not have a private key"
          Just (_, priv) -> do
            let priv' = decryptPrivateKey (userId user) pass priv
                Just (sessionLookup, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
            encKey <- getUserEncryptionKeyFor (sessionDbConn context) user rid
            decrypted <- case (priv', encKey) of
                           (Just privKey, Just encKey') -> decryptSharedKey encKey' privKey
                           _ -> return $ Nothing
            case decrypted of
              Nothing -> throw $ VisibleError "Could not decrypt private key"
              Just d -> deepseq d $ sessionInsert sessionKeyName (Text.pack $ show (rid, d)) >> redirectBack req f
    _ -> throw $ VisibleError "Parameters missing"
-- postWithDecryptionKey :: Int64 -> CsrfVerifiedApplication -> CsrfVerifiedApplication 
-- postWithDecryptionKey rid other params context req f
--   let Just (sessionLookup, _) = Vault.lookup (sessionSession context) $ vault req
--   key <- sessionLookup sessionKeyNameKeys
--   case key of
--     Nothing -> raise $ VisibleErrorWithStatus status403 "This report is locked"
--     Just key' -> other rid key' 
-- verifyCsrf :: CsrfVerifiedApplication -> WebApplication
-- verifyCsrf target context req f = do
--   let Just (sessionLookup, _) = Vault.lookup (sessionSession context) (vault req)
--   csrf <- sessionLookup sessionKeyNameKeys
--   (params, files) <- parseRequestBody lbsBackEnd req
--   let params' = flip map params $ \(p1, p2) -> (Encoding.decodeUtf8 p1, Encoding.decodeUtf8 p2)
--       valid = case (csrf, lookup "csrf" params') of
--                 (Just c1, Just c2) -> c1 == c2
--                 _ -> False
--   if valid
--     then target (params', files) context req f
--     else throw $ VisibleError "Oh noes, a CSRF attack. I give up, change anything."

