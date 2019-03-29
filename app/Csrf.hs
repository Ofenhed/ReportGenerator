{-# LANGUAGE OverloadedStrings #-}
module Csrf (withCsrf, verifyCsrf, CsrfFormApplication, CsrfVerifiedApplication, FileInfo(..)) where

-- import Types
import Common

import Network.Wai (vault)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, File, FileInfo(..))
import Data.Maybe (mapMaybe)
import Control.Exception (throw)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as Encoding
import System.Random

type CsrfFormApplication = Text.Text -> WebApplication
type CsrfVerifiedApplication = ([(Text.Text, Text.Text)], [File LC8.ByteString]) -> WebApplication
sessionKeyName = "csrf"

withCsrf :: CsrfFormApplication -> WebApplication
withCsrf other context req f = do
  let Just (sessionLookup, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
  csrf <- sessionLookup sessionKeyName
  csrf' <- case csrf of
                Nothing -> do
                  str <- flip mapM [1..20] $ (\_ -> randomRIO ('a', 'z'))
                  sessionInsert sessionKeyName $ Text.pack str
                  return $ Text.pack str
                Just t -> return t
  other csrf' context req f


verifyCsrf :: CsrfVerifiedApplication -> WebApplication
verifyCsrf target context req f = do
  let Just (sessionLookup, _) = Vault.lookup (sessionSession context) (vault req)
  csrf <- sessionLookup sessionKeyName
  (params, files) <- parseRequestBody lbsBackEnd req
  let params' = flip map params $ \(p1, p2) -> (Encoding.decodeUtf8 p1, Encoding.decodeUtf8 p2)
      valid = case (csrf, lookup "csrf" params') of
                (Just c1, Just c2) -> c1 == c2
                _ -> False
  if valid
    then target (params', files) context req f
    else throw $ VisibleError "Oh noes, a CSRF attack. I give up, change anything."

