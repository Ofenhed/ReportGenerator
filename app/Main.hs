{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Database
import Heading

import Network.Wai (Application, responseLBS, responseFile, requestMethod, pathInfo, Response)
import Network.Wai.Session (withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnExceptionResponse)
import Data.Default.Class
import Network.HTTP.Types (status200, status404, status500)
import Data.Maybe (fromJust)
import Control.Monad.ST (runST)

import System.Environment (getEnvironment)
-- import System.FilePath ((</>), takeFileName)
import Control.Exception (fromException, SomeException)

import Text.Ginger.Run (makeContextHtml, runGinger)
import Text.Ginger.Parse (parseGinger)
import Text.Ginger (toGVal)
import Text.Ginger.GVal (fromFunction, GVal(..))
import Text.Ginger.Html (htmlSource, unsafeRawHtml, htmlSource, html)
import Text.Ginger.AST (Template)

import qualified Data.Vault.Lazy                as Vault
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as TextIO
import qualified Database.SQLite.Simple         as DB
import qualified Data.ByteString.Lazy.Char8     as LC8

staticDir = "static"
serverDir = "static/server"
clientDir = "static/client"

app db req f = do
  let call x = x db req f
  case (requestMethod req, pathInfo req) of
    -- ("GET", ["static", file]) -> f $ responseFile status200 [] (staticDir </> (takeFileName $ Text.unpack file)) Nothing
    -- -- ("GET", ["server", file]) -> f $ responseFile status200 [] (serverDir </> (takeFileName $ Text.unpack file)) Nothing
    -- -- ("GET", ["client", file]) -> f $ responseFile status200 [] (clientDir </> (takeFileName $ Text.unpack file)) Nothing
    -- ("GET", []) -> call indexPage

    -- ("GET", ["findings"]) -> call showFindings
    -- ("GET", ["findings", "new"]) -> call $ withCsrf $ showFinding Nothing
    -- ("GET", ["findings", id]) -> call $ withCsrf $ showFinding $ Just (read (Text.unpack id) :: Int)
    -- ("POST", ["findings", "new"]) -> call $ verifyCsrf $ saveFinding Nothing
    -- ("POST", ["findings", id]) -> call $ verifyCsrf $ saveFinding $ Just (read (Text.unpack id) :: Int)

    -- ("GET", ["findings", "delete", id]) -> call $ withCsrf $ removeFinding (read (Text.unpack id) :: Int)
    -- ("POST", ["findings", "delete", id]) -> call $ verifyCsrf $ doRemoveFinding (read (Text.unpack id) :: Int)

    -- ("GET", ["tests"]) -> call showTests

    -- ("GET", ["tests", "new"]) -> call $ withCsrf $ showTest Nothing
    -- ("GET", ["tests", id]) -> call $ withCsrf $ showTest $ Just (read (Text.unpack id) :: Int)
    -- ("POST", ["tests", "new"]) -> call $ verifyCsrf $ saveTest Nothing
    -- ("POST", ["tests", id]) -> call $ verifyCsrf $ saveTest $ Just (read (Text.unpack id) :: Int)

    -- ("GET", ["tests", "delete", id]) -> call $ withCsrf $ removeTest (read (Text.unpack id) :: Int)
    -- ("POST", ["tests", "delete", id]) -> call $ verifyCsrf $ doRemoveTest (read (Text.unpack id) :: Int)

    -- ("GET", ["report"]) -> call $ showReport

    _ -> f $ responseLBS status404 [("Content-Type", "text/plain")] "Oh, sorry, I could not find this site"

-- showError' :: SomeException -> Response
-- showError' e = case fromException e
--                  of (Just (VisibleError msg)) -> responseLBS status500 [("Content-Type", "text/plain")] $ LC8.concat ["Error: ", LC8.pack $ Text.unpack msg]
--                     _ -> responseLBS status500 [("Content-Type", "text/plain")] "Something went screwy, and before you knew he was trying to kill everyone."

render = do
  headerState <- createHeaderState
  Right parsed <- parseGinger (const $ return Nothing) Nothing "Hello {{username}} {{   header(2, '<script>alert(1);</script>') }} {{ header(3, 'hejsan', id=\"<dåligt>id<'\") }} {{ header(1, 'Big stuff') }} {{ header(2, 'smaller stuff', id='hejsan') }} {{ ref('hejsan') }} "
  let contextLookup varName =
        case varName of
          "username" -> toGVal $ Text.pack "haxxor"
          "header" -> fromFunction $ createHeading headerState
          "ref" -> fromFunction $ createRef headerState
          _ -> def
      context = makeContextHtml contextLookup
      source = htmlSource $ runGinger context parsed
  finalized <- finalizeRefs headerState source
  return $ case finalized of
    Right h -> h
    Left err -> Text.concat ["Error: ", err]

main = do
  -- session <- Vault.newKey
  -- store <- mapStore_
  rendered <- render
  TextIO.putStrLn rendered
  -- db <- openDatabase
  -- port <- lookup "PORT" <$> getEnvironment
  -- let settings = -- setOnExceptionResponse showError'
  --              maybe defaultSettings (\p -> setPort (read p) defaultSettings) port
  -- runSettings settings $ -- withSession store "sess" def session $ app db session
  --                        app db
