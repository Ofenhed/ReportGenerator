{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common (module Types
              , module Common
              , module Network.HTTP.Types
              , module Text.Ginger.GVal
              , module Text.Ginger.Html
              , module Text.Ginger
              , def) where

import Types
import Database.Types
import Database.Resolver

import Network.Wai (Application, responseLBS, responseFile, requestMethod, pathInfo, Response)
import Network.Wai.Session (withSession)
import Data.Default.Class (def)
import Network.HTTP.Types (status200, status404, status500)
import Text.Ginger.GVal (toGVal, GVal(..), ToGVal(..))
import Text.Ginger (toGVal, VarName, Run, dict)
import Text.Ginger.Html (Html)
import Crypto.MAC.HMAC(Context(..))
import Crypto.Hash.Algorithms (SHA256)

import qualified Data.Text                      as Text
import qualified Data.Map                       as Map
import qualified Data.Text.Lazy                 as LazyText
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Text.Lazy.Encoding        as LazyEncoding
import qualified Data.ByteString.Char8          as C8
import qualified Data.Vault.Lazy                as Vault
import qualified Network.Wai.Session as S

data SessionType = Session { sessionDbConn :: Connection
                           , sessionSession :: Vault.Key (S.Session IO Text.Text Text.Text)
                           , sessionHasher :: Context SHA256
                           , sessionUser :: Maybe User }

type WebApplication = SessionType -> Application

type TemplateLookupType p = VarName -> Run p IO Html (GVal (Run p IO Html))

responseText code headers = (responseLBS code $ map (\(x, y) -> (x, Encoding.encodeUtf8 y)) headers) . LazyEncoding.encodeUtf8 . LazyText.fromStrict
responseTextLazy code headers = (responseLBS code $ map (\(x, y) -> (x, Encoding.encodeUtf8 y)) headers) . LazyEncoding.encodeUtf8

instance ToGVal m Template where
  toGVal t = dict $ [("id", toGVal $ templateId t)
                    ,("includeName", toGVal $ templateIncludeName t)
                    ,("longName", toGVal $ templateLongName t)
                    ,("description", toGVal $ templateDescription t)
                    ,("source", toGVal $ templateSource t)
                    ,("editor", toGVal $ templateEditor t)
                    ,("includable", toGVal $ templateIncludable t)]

instance ToGVal m TemplateVars where
  toGVal t = dict $ [("id", toGVal $ templateVarsId t)
                    ,("type", toGVal ("arr" :: Text.Text))
                    ,("name", toGVal $ templateVarsName t)
                    ,("description", toGVal $ templateVarsDescription t)
                    ,("children", toGVal $ templateVarsChildren t)]
instance ToGVal m TemplateVar where
  toGVal t = dict $ [("id", toGVal $ templateVarId t)
                    ,("type", toGVal ("var" :: Text.Text))
                    ,("name", toGVal $ templateVarName t)
                    ,("description", toGVal $ templateVarDescription t)
                    ,("default", toGVal $ templateVarDefault t)
                    ,("children", toGVal $ templateVarChildren t)]
                                         
instance ToGVal m Report where
  toGVal t = dict $ [("id", toGVal $ reportId t)
                    ,("name", toGVal $ reportName t)
                    ,("templateId", toGVal $ templateId $ reportTemplate t)
                    ,("templateIncludeName", toGVal $ templateIncludeName $ reportTemplate t)
                    ,("templateLongName", toGVal $ templateLongName $ reportTemplate t)
                    ,("editor", toGVal $ templateEditor $ reportTemplate t)
                    ,("encrypted", toGVal $ reportEncrypted t)]

instance ToGVal m SavedVars where
  toGVal xs = gvalMap [("id", toGVal $ savedVarsId xs)
                      ,("name", toGVal $ savedVarsName xs)
                      ,("description", toGVal $ savedVarsDescription xs)
                      ,("data", toGVal $ savedVarsData xs)
                      ,("var", gvalMap $ flip map (Map.toList $ savedVarsVar xs) $
                                                  \(template, (id, d)) ->
                                                   (Text.pack $ show template
                                                   ,gvalMap [("id", toGVal id)
                                                            ,("value", toGVal d)]))]

