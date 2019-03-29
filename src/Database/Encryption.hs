{-# LANGUAGE OverloadedStrings #-}
module Database.Encryption where

import ServerSecret
import Database.Types

import Crypto.MAC.HMAC (hmac, HMAC(hmacGetDigest))
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA512, SHA3_224(SHA3_224))
import Crypto.Cipher.Types (nullIV, makeIV, cbcEncrypt, cbcDecrypt, Cipher(cipherInit, cipherKeySize), KeySizeSpecifier(KeySizeFixed), BlockCipher (cbcEncrypt, cbcDecrypt, blockSize), IV())
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

type EncryptionType = AES192

decryptData_ :: (Read a, BA.ByteArrayAccess b) => b -> IV EncryptionType -> Text.Text -> Maybe a
decryptData_ key iv d = let KeySizeFixed keySize = cipherKeySize (undefined :: EncryptionType)
                            (firstKey, rest) = BA.splitAt keySize $ BA.convert key :: (BA.Bytes, BA.Bytes)
                            CryptoPassed firstCiph = cipherInit firstKey :: CryptoFailable EncryptionType
                          in case unpad (PKCS7 $ blockSize firstCiph) $ cbcDecrypt firstCiph iv $ B64.decodeLenient $ Encoding.encodeUtf8 d of
                               Nothing -> Nothing
                               Just k -> case reads $ C8.unpack k of
                                           [(k', [])] -> Just k'
                                           _ -> Nothing

encryptData_ :: (Show a, BA.ByteArrayAccess b) => b -> IV EncryptionType -> a -> Text.Text
encryptData_ key iv d = let KeySizeFixed keySize = cipherKeySize (undefined :: EncryptionType)
                            (firstKey, rest) = BA.splitAt keySize $ BA.convert key :: (BA.Bytes, BA.Bytes)
                            CryptoPassed firstCiph = cipherInit firstKey :: CryptoFailable EncryptionType
                          in Encoding.decodeLatin1 $ B64.encode $ cbcEncrypt firstCiph iv $ pad (PKCS7 $ blockSize firstCiph) $ Encoding.encodeUtf8 $ Text.pack $ show d

decryptData :: Read a => EncryptionKey -> EncryptionIv -> Text.Text -> Either Text.Text a
decryptData key iv d = case makeIV $ B64.decodeLenient $ C8.pack $ Text.unpack iv of
                         Nothing -> Left "Could not create IV"
                         Just iv' -> case decryptData_ (Encoding.encodeUtf8 key) iv' d of
                                       Nothing -> Left "Could not decrypt data"
                                       Just j -> Right j

encryptData :: Show a => EncryptionKey -> EncryptionIv -> a -> Either Text.Text Text.Text
encryptData key iv d = case makeIV $ B64.decodeLenient $ C8.pack $ Text.unpack $ iv of
                         Nothing -> Left "Could not create IV"
                         Just iv' -> Right $ encryptData_ (Encoding.encodeUtf8 key) iv' d



generateIv :: IO EncryptionIv
generateIv = (flip mapM [1..blockSize (undefined :: EncryptionType)] $ (\_ -> randomIO)) >>= return . Text.pack . C8.unpack . B64.encode . C8.pack

generateSharedKey :: IO EncryptionKey
generateSharedKey = let KeySizeFixed keySize = cipherKeySize (undefined :: EncryptionType)
                      in (flip mapM [1..keySize] $ (\_ -> randomIO)) >>= return . Text.pack

decryptionKey :: Int64 -> Text.Text -> Digest SHA512
decryptionKey id password = hmacGetDigest $ hmac (Encoding.encodeUtf8 $ Text.concat ["pubKeyEncryptionKey", serverSecret]) $ Encoding.encodeUtf8 $ Text.concat [Text.pack $ show id, ":", password]

generateKeyPair :: Int64 -> Text.Text -> IO (PublicKey, EncryptedPrivateKey)
generateKeyPair id password = do
  (pub, priv) <- generate 1024 0x10001
  return (pub, encryptPrivateKey id password priv)

encryptPrivateKey :: Int64 -> Text.Text -> PrivateKey -> EncryptedPrivateKey
encryptPrivateKey id password key = encryptData_ (decryptionKey id password) nullIV key

decryptPrivateKey :: Int64 -> Text.Text -> EncryptedPrivateKey -> Maybe PrivateKey
decryptPrivateKey id password key = decryptData_ (decryptionKey id password) nullIV key

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
