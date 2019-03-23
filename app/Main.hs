{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Database
import ReportGenerator
import ReportEditor
import Types
import Templates
import Common
import Csrf

import Network.Wai (Application, responseLBS, responseFile, requestMethod, pathInfo, Response, Middleware)
import Network.Wai.Session (withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (defaultSettings, setPort, setOnExceptionResponse)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Web.Cookie (SetCookie(setCookieSecure, setCookieHttpOnly))
import Data.Default.Class
import Network.HTTP.Types (status200, status404, status500, Status(statusCode))
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.ST (runST)
import System.IO (openFile, IOMode(ReadMode), hGetContents)

import System.Environment (getEnvironment)
-- import System.FilePath ((</>), takeFileName)
import Control.Exception (fromException, SomeException, try)

import qualified Data.Vault.Lazy                as Vault
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as TextIO
import qualified Database.SQLite.Simple         as DB
import qualified Data.ByteString.Lazy.Char8     as LC8
import qualified Data.ByteString.Char8          as C8

import Debug.Trace

staticDir = "static"
serverDir = "static/server"
clientDir = "static/client"

app sess req f = do
  let call x = x sess req f
  let toTemplateParent "var" i = return $ TemplateVarParentVar $ read $ Text.unpack i
      toTemplateParent "arr" i = return $ TemplateVarParentVars $ read $ Text.unpack i
      toTemplateParent _ _ = throw $ VisibleErrorWithStatus status404 "Now that wasn't very nice, now was it?"
  case (requestMethod req, pathInfo req, Just 1) of
    -- ("GET", ["static", file]) -> f $ responseFile status200 [] (staticDir </> (takeFileName $ Text.unpack file)) Nothing
    -- -- ("GET", ["server", file]) -> f $ responseFile status200 [] (serverDir </> (takeFileName $ Text.unpack file)) Nothing
    -- -- ("GET", ["client", file]) -> f $ responseFile status200 [] (clientDir </> (takeFileName $ Text.unpack file)) Nothing
    -- ("GET", []) -> call indexPage


    -- List and edit reports
    ("GET", ["report"], Just _) -> call $ listReports
    ("GET", ["report", id], Just _) -> call $ withCsrf $ editReport (read $ Text.unpack id :: Int)
    ("POST", ["report", id], Just _) -> call $ verifyCsrf $ saveReport (read $ Text.unpack id :: Int)
    
    -- List and edit templates
    ("GET", ["template"], Just _) -> call $ listTemplates
    ("GET", ["template", id], Just _) -> call $ withCsrf $ editTemplate (read $ Text.unpack id :: Int)
    ("POST", ["template", id], Just _) -> call $ verifyCsrf $ editTemplate_ (read $ Text.unpack id :: Int)

    -- Add subvariables
    ("GET", ["template", id, parent_type, varid, "add", add_type], Just _) -> do newType <- toTemplateParent add_type "0"
                                                                                 parentType <- toTemplateParent parent_type varid
                                                                                 call $ withCsrf $ addTemplateVar (read $ Text.unpack id :: Int) parentType newType
    ("POST", ["template", id, parent_type, varid, "add", add_type], Just _) -> do newType <- toTemplateParent add_type "0" 
                                                                                  parentType <- toTemplateParent parent_type varid
                                                                                  call $ verifyCsrf $ addTemplateVar_ (read $ Text.unpack id :: Int) parentType newType
    -- Add top variables
    ("GET", ["template", id, "add", add_type], Just _) -> do newType <- toTemplateParent add_type "0"
                                                             let id' = read $ Text.unpack id
                                                             call $ withCsrf $ addTemplateVar id' (TemplateVarParent id') newType
    ("POST", ["template", id, "add", add_type], Just _) -> do newType <- toTemplateParent add_type "0" 
                                                              let id' = read $ Text.unpack id
                                                              call $ verifyCsrf $ addTemplateVar_ id' (TemplateVarParent id') newType
    -- Delete template variable
    ("GET", ["template", id, _type, varid, "delete"], Just _) -> toTemplateParent _type varid >>= call . withCsrf . (promptDeleteTemplateVariable (read $ Text.unpack id :: Int))
    ("POST", ["template", id, _type, varid, "delete"], Just _) -> toTemplateParent _type varid >>= call . verifyCsrf . (promptDeleteTemplateVariable_ (read $ Text.unpack id :: Int))

    -- Generate report
    ("GET", ["report", "generate", id], Just _) -> render (sessionDbConn sess) 1 >>= \rep -> f $ responseText status200 [("Content-Type", "text/html")] rep

    _ -> throw $ VisibleErrorWithStatus status404 "Could not find this site."

sockServer _ = return ()

showErrors :: Middleware
showErrors other req f = do
  resp <- try (other req f)
  case resp of
    Right res -> return res
    Left err -> do
      let (status, db) = case fromException err of
                       Just (VisibleError msg) -> (status500, [("exception", toGVal msg), ("status", toGVal (500 :: Int))])
                       Just (VisibleErrorWithStatus status msg) -> (status, [("exception", toGVal msg), ("status", toGVal $ statusCode status)])
                       _ -> throw err
      result <- try (runTemplate "exception" $ \k -> return $ fromMaybe def $ lookup k db) :: IO (Either VisibleError Text.Text)
      case result of
        Right result' -> f $ responseText status [("Content-Type", "text/html")] result'
        Left _ -> f $ responseText status500 [] "Something went screwy, and before you knew he was trying to kill everyone."

main = do
  session <- Vault.newKey
  store <- mapStore_
  db <- openDatabase
  port <- lookup "PORT" <$> getEnvironment
  let settings = maybe defaultSettings (\p -> setPort (read p) defaultSettings) port
  runTLS (tlsSettings "new.cert.cert" "new.cert.key") settings $ showErrors
                                                               $ withSession store "sess" (def { setCookieHttpOnly = True, setCookieSecure = True }) session
                                                               $ websocketsOr defaultConnectionOptions sockServer
                                                               $ app $ Session { sessionDbConn = db, sessionSession = session }
