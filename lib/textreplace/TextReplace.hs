module TextReplace (CaseSensitivity(CaseSensitive), build, run) where

import qualified Data.Text as Text

data CaseSensitivity = CaseSensitive

build :: CaseSensitivity -> [(Text.Text, Text.Text)] -> [(Text.Text, Text.Text)]
build CaseSensitive list = list

run :: [(Text.Text, Text.Text)] -> Text.Text -> Text.Text
run = flip $ foldl $ \t (k, r) -> Text.replace k r t
