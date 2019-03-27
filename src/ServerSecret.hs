{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ServerSecret where

import Data.Text.Encoding (decodeLatin1)
import Data.FileEmbed (embedFile)

serverSecret = decodeLatin1 $(embedFile "server_secret.txt")
