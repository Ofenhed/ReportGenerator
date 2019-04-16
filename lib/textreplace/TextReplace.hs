module TextReplace (CaseSensitivity(CaseSensitive), build, run) where

import qualified Data.Text as Text
import Data.List (sortOn)

data CaseSensitivity = CaseSensitive

build :: CaseSensitivity -> [(Text.Text, Text.Text)] -> [(Text.Text, Text.Text)]
build CaseSensitive list = sortOn (((-)0) . Text.length . fst) list

run :: [(Text.Text, Text.Text)] -> Text.Text -> Text.Text
run = flip $ foldl $ \t (k, r) -> Text.replace k r t
