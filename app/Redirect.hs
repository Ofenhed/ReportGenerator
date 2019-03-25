{-# LANGUAGE OverloadedStrings #-}
module Redirect (redirect, redirectSame, redirectBack) where
import Types
import Network.HTTP.Types (status302, hLocation, hReferer)
import Network.Wai (Application, responseLBS, rawPathInfo, requestHeaders)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Text                      as Text

redirect_ :: C8.ByteString -> Application
redirect_ location _ f = f $ responseLBS status302 [(hLocation, location)] LC8.empty

redirect :: Text.Text -> Application
redirect = redirect_ . Encoding.encodeUtf8

redirectSame :: Application
redirectSame req = redirect_ (rawPathInfo req) req

redirectBack :: Application
redirectBack req f = do
  let referer = lookup hReferer $ requestHeaders req
  case referer of
    Just r -> redirect_ r req f
    Nothing -> throw $ VisibleError "Could not redirect"

