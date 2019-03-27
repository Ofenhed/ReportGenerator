module Database.Encryption where

import System.Random (randomRIO)

import qualified Data.Text as Text

type EncryptionKey = Text.Text

decryptData key iv d = let (key', d') = Text.splitAt (Text.length key + Text.length iv) d
                      in if (Text.append key iv) == key'
                           then Just d'
                           else Nothing

encryptData key iv d = Text.concat [key, iv, d]

generateIv :: IO Text.Text
generateIv = (flip mapM [1..64] $ (\_ -> randomRIO ('0', 'z'))) >>= return . Text.pack
