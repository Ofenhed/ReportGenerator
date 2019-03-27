{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UserType where

import Types
import ServerSecret

import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA512)
import Data.FileEmbed (embedFile)

import qualified Data.Text.Encoding   as Encoding
import qualified Data.Text            as Text

hashPasswordAndSalt :: Text.Text -> Text.Text -> Digest SHA512
hashPasswordAndSalt pass salt = hmacGetDigest $ hmac (Encoding.encodeUtf8 $ Text.concat ["password", serverSecret, salt]) $ Encoding.encodeUtf8 pass
