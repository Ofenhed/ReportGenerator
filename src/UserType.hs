{-# LANGUAGE TemplateHaskell #-}
module UserType where

import Types

import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA512)
import Data.FileEmbed (embedFile)

import qualified Data.Text.Encoding   as Encoding
import qualified Data.Text            as Text

serverSalt = Encoding.decodeUtf8 $(embedFile "server_secret.txt")

hashPasswordAndSalt :: Text.Text -> Text.Text -> Digest SHA512
hashPasswordAndSalt pass salt = hmacGetDigest $ hmac (Encoding.encodeUtf8 $ Text.append serverSalt salt) $ Encoding.encodeUtf8 pass
