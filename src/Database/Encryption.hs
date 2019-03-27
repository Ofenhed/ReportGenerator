{-# LANGUAGE OverloadedStrings #-}
module Database.Encryption where

import Database.SQLite.Simple
import System.Random (randomRIO)

import qualified Data.Text as Text

type EncryptionKey = Text.Text

decryptData :: Read a => EncryptionKey -> Text.Text -> Text.Text -> Maybe a
decryptData key iv d = let (key', d') = Text.splitAt (Text.length key + Text.length iv) d
                      in if (Text.append key iv) == key'
                           then case reads $ Text.unpack d' of
                                  [(res, "")] -> Just res
                                  _ -> Nothing
                           else Nothing

encryptData :: Show a => EncryptionKey -> Text.Text -> a -> Text.Text
encryptData key iv d = Text.concat [key, iv, Text.pack $ show d]

generateIv :: IO Text.Text
generateIv = (flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))) >>= return . Text.pack

getUserEncryptionKey conn uid rid = do
  res <- query conn "SELECT key FROM ReportKey WHERE user = ? AND report = ?" (uid, rid)
  case res of
    [(Only key)] -> return $ Just key
    [] -> return Nothing
