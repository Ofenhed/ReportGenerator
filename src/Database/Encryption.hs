{-# LANGUAGE OverloadedStrings #-}
module Database.Encryption where

import ServerSecret
import Database.Types

import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA512, SHA3_224(SHA3_224))
import Crypto.Cipher.Types (nullIV, cbcEncrypt, cbcDecrypt, Cipher(cipherInit, cipherKeySize), KeySizeSpecifier(KeySizeFixed), BlockCipher (cbcEncrypt, cbcDecrypt, blockSize))
import Crypto.PubKey.RSA (PrivateKey, PublicKey, generate)
import Crypto.Cipher.AES (AES192)
import Crypto.Data.Padding (pad, unpad, Format(PKCS7))
import Crypto.Error (CryptoFailable(CryptoPassed))
import System.Random (randomIO)
import Data.Int (Int64)

import qualified Crypto.PubKey.RSA.OAEP   as OAEP
import qualified Data.ByteString.Base64   as B64
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Encoding
import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Char8    as C8

import Debug.Trace

type EncryptionKey = Text.Text
type EncryptionIv = Text.Text

decryptData :: Read a => EncryptionKey -> EncryptionIv -> Text.Text -> Maybe a
decryptData key iv d = let (key', d') = Text.splitAt (Text.length key + Text.length iv) d
                      in if (Text.append key iv) == key'
                           then case reads $ Text.unpack d' of
                                  [(res, "")] -> Just res
                                  _ -> Nothing
                           else Nothing

encryptData :: Show a => EncryptionKey -> EncryptionIv -> a -> Text.Text
encryptData key iv d = Text.concat [key, iv, Text.pack $ show d]

generateIv :: IO EncryptionIv
generateIv = (flip mapM [1..64] $ (\_ -> randomIO)) >>= return . Text.pack

generateSharedKey :: IO EncryptionKey
generateSharedKey = (flip mapM [1..64] $ (\_ -> randomIO)) >>= return . Text.pack

decryptionKey :: Int64 -> Text.Text -> Digest SHA512
decryptionKey id password = hmacGetDigest $ hmac (Encoding.encodeUtf8 $ Text.concat ["pubKeyEncryptionKey", serverSecret]) $ Encoding.encodeUtf8 $ Text.concat [Text.pack $ show id, ":", password]

generateKeyPair :: Int64 -> Text.Text -> IO (PublicKey, EncryptedPrivateKey)
generateKeyPair id password = do
  (pub, priv) <- generate 1024 0x10001
  return (pub, encryptPrivateKey id password priv)

encryptPrivateKey :: Int64 -> Text.Text -> PrivateKey -> EncryptedPrivateKey
encryptPrivateKey id password key = let KeySizeFixed aesSize = cipherKeySize (undefined :: AES192)
                                        (aesKey, rest) = BA.splitAt aesSize $ BA.convert $ decryptionKey id password :: (BA.Bytes, BA.Bytes)
                                        CryptoPassed aesCiph = cipherInit aesKey :: CryptoFailable AES192
                                      in Encoding.decodeLatin1 $ B64.encode $ cbcEncrypt aesCiph nullIV $ pad (PKCS7 $ blockSize aesCiph) $ Encoding.encodeUtf8 $ Text.pack $ show key

decryptPrivateKey :: Int64 -> Text.Text -> EncryptedPrivateKey -> Maybe PrivateKey
decryptPrivateKey id password key = let KeySizeFixed aesSize = cipherKeySize (undefined :: AES192)
                                        (aesKey, rest) = BA.splitAt aesSize $ BA.convert $ decryptionKey id password :: (BA.Bytes, BA.Bytes)
                                        CryptoPassed aesCiph = cipherInit aesKey :: CryptoFailable AES192
                                        decodedKey = B64.decodeLenient $ Encoding.encodeUtf8 key
                                      in case unpad (PKCS7 $ blockSize aesCiph) $ cbcDecrypt aesCiph nullIV decodedKey of
                                           Nothing -> Nothing
                                           Just k -> case reads $ C8.unpack k of
                                                       [(k, [])] -> Just k
                                                       _ -> Nothing

encryptSharedKey :: EncryptionKey -> PublicKey -> IO (Maybe Text.Text)
encryptSharedKey secret pub = do
  result <- OAEP.encrypt (OAEP.defaultOAEPParams SHA3_224) pub $ Encoding.encodeUtf8 $ Text.pack $ show secret
  case result of
    Left e -> traceShow e $ return Nothing
    Right d -> return $ Just $ Encoding.decodeUtf8 $ B64.encode d

decryptSharedKey :: Text.Text -> PrivateKey -> IO (Maybe EncryptionKey)
decryptSharedKey enc key = do
  let decoded = B64.decodeLenient $ Encoding.encodeUtf8 enc
  result <- OAEP.decryptSafer (OAEP.defaultOAEPParams SHA3_224) key decoded
  case result of
    Left e -> traceShow e $ return Nothing
    Right d -> case reads $ C8.unpack d of
                 [(r, [])] -> return $ Just r
                 _ -> return Nothing
