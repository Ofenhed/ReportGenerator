{-# LANGUAGE OverloadedStrings #-}
module Login where

import Common
import Redirect
import TemplateFiles
import Database.Types
import Database.Resolver
import Database.Writer
import Csrf
import Network.Wai (vault)

import qualified Data.Vault.Lazy                as Vault
import qualified Data.Text                      as Text

doLogOut context req = do
  let Just (_, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
  sessionInsert "user" ""
  return ()

showLogOut context req f = do
  doLogOut context req
  redirect "/" req f

loggedInUser context req = do
  let Just (sessionLookup, _) = Vault.lookup (sessionSession context) (vault req)
  user <- sessionLookup "user"
  case user of
    Nothing -> return Nothing
    Just t -> case reads $ Text.unpack t of
                [((uid, passid), "")] -> do
                  user' <- getUserFromId (sessionDbConn context) uid (Just passid)
                  case user' of
                    Nothing -> doLogOut context req
                    _ -> return ()
                  return user'
                _ -> return Nothing

showLogin :: CsrfFormApplication
showLogin csrf context req f = do
  let lookup name = case name of
                      "csrf" -> return $ toGVal csrf
                      _ -> return $ def
  login <- runTemplate context Nothing "login" lookup
  f $ responseText status200 [(hContentType, "text/html")] login

showLogin_ :: CsrfVerifiedApplication
showLogin_ (params, _) context req f = do
  case (lookup "username" params, lookup "password" params) of
    (Just u, Just p) -> do user <- getUserWithPassword (sessionDbConn context) u p
                           case user of
                             Just u -> do
                               let Just (_, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
                               sessionInsert "user" $ Text.pack $ show (userId u, userPassId u)
                               redirect "/" req f
                             Nothing -> redirectSame req f
    _ -> throw $ VisibleError "Now, what am I supposed to do with that?"

userPage :: CsrfFormApplication
userPage csrf context req f = do
  let lookup name = case name of
                      "csrf" -> return $ toGVal csrf
                      _ -> return def
  page <- runTemplate context Nothing "user" lookup
  f $ responseText status200 [(hContentType, "text/html")] page

userPage_ :: CsrfVerifiedApplication
userPage_ (params, _) context req f = do
  case (lookup "oldPass" params, lookup "newPass" params, lookup "newPassAgain" params, sessionUser context) of
    (Just old, Just new, Just new2, Just user) -> do
      if Text.null old || Text.null new || new /= new2
        then redirectSame req f
        else updateUserPassword (sessionDbConn context) (userUsername user) old new >>= \success -> redirect "/" req f
    _ -> redirectSame req f
