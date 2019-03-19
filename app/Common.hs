module Common (module Types
              , module Common
              , module TemplateFiles
              , module Network.HTTP.Types
              , module Text.Ginger.GVal
              , module Text.Ginger.Html
              , module Text.Ginger
              , def, throw) where

import TemplateFiles
import Types

import Network.Wai (Application, responseLBS, responseFile, requestMethod, pathInfo, Response)
import Network.Wai.Session (withSession)
import Data.Default.Class (def)
import Network.HTTP.Types (status200, status404, status500)
import Text.Ginger.GVal (toGVal, GVal(..))
import Text.Ginger (toGVal, VarName, Run)
import Text.Ginger.Html (Html)
import Control.Exception (throw)

import qualified Data.Text                      as Text
import qualified Data.Text.Lazy                 as LazyText
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Text.Lazy.Encoding        as LazyEncoding
import qualified Data.ByteString.Char8          as C8

responseText code headers = (responseLBS code $ map (\(x, y) -> (x, Encoding.encodeUtf8 y)) headers) . LazyEncoding.encodeUtf8 . LazyText.fromStrict
responseTextLazy code headers = (responseLBS code $ map (\(x, y) -> (x, Encoding.encodeUtf8 y)) headers) . LazyEncoding.encodeUtf8

