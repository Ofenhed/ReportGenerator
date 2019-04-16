{-# LANGUAGE OverloadedStrings #-}
module Redirect (redirect, redirectSame, redirectBack, redirectBackOverridable) where
import Types
import SignedData
import TextReplacer
import Network.HTTP.Types (status302, hLocation, hReferer, hContentType, status200)
import Network.Wai (Application, responseLBS, rawPathInfo, requestHeaders, queryString)
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

redirectBackOverridable hmac replace req f = do
  case lookup ("RedirectUrl" :: C8.ByteString) $ queryString req of
    Just Nothing -> f $ responseLBS status200 [(hContentType, "plain/text")] $ LC8.pack $ show replace
    Just (Just v) -> let unverified = read $ C8.unpack v
                         verified = verifySignedData hmac unverified
                       in case verified of
                            Nothing -> redirectBack req f
                            Just v -> let replacer = build CaseSensitive $ flip map replace $ \(x, y) -> (Text.append "$" x, y)
                                        in redirect (run replacer $ getSignedData v) req f
    _ -> redirectBack req f
