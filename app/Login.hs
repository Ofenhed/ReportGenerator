{-# LANGUAGE OverloadedStrings #-}
module Login (doLogOut, showLogOut, loggedInUser, showLogin, showLogin_, userPage, userPage_) where

import Common
import Redirect
import TemplateFiles
import Database.Types
import Database.Resolver
import Database.Writer
import Csrf
import Network.Wai (vault)
import Encryption

import qualified Data.Vault.Lazy                as Vault
import qualified Data.Text                      as Text

sessionKeyName = "user"

doLogOut context req = do
  let Just (_, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
  sessionInsert sessionKeyName ""
  clearDecryptionKeys context req
  return ()

showLogOut context req f = do
  doLogOut context req
  redirect "/" req f

loggedInUser context req = do
  let Just (sessionLookup, _) = Vault.lookup (sessionSession context) (vault req)
  user <- sessionLookup sessionKeyName
  case user of
    Nothing -> return Nothing
    Just t -> case reads $ Text.unpack t of
                [((uid, passid, privKey), "")] -> do
                  user' <- getUserFromId (sessionDbConn context) uid (Just passid)
                  user'' <- case (user', privKey) of
                              (Nothing, _) -> doLogOut context req >> return Nothing
                              (Just u, Nothing) -> return $ Just u
                              (Just u, Just privKey') -> return $ Just $
                                                           case userKey u of
                                                             Nothing -> u
                                                             Just (pub, _) -> u { userKey = Just (pub, privKey') }
                  return user''
                _ -> return Nothing

showLogin :: CsrfFormApplication
showLogin csrf context req f = do
  let lookup name = case name of
                      "csrf" -> return $ toGVal csrf
                      _ -> return $ def
  login <- runTemplate context Nothing "login" lookup
  f $ responseText status200 [] login

showLogin_ :: CsrfVerifiedApplication
showLogin_ (params, _) context req f = do
  case (lookup "username" params, lookup "password" params, lookup "use_temp_password" params, lookup "temp_password" params) of
    (Just u, Just p, useTempPassword, temp) ->
                              do user <- getUserWithPassword (sessionDbConn context) u p
                                 case user of
                                   Just u -> do
                                     let Just (_, sessionInsert) = Vault.lookup (sessionSession context) (vault req)
                                     priv <- case (userKey u, useTempPassword, temp) of
                                               (Just (_, priv), Nothing, _) -> return $ Just priv
                                               (Nothing, Nothing, _) -> return Nothing
                                               (Just _, Just _, Just tempPass) -> case createTemporaryKey u p tempPass of
                                                                            Just newKey -> return $ Just newKey
                                                                            Nothing -> throw $ VisibleError "Could not create a new key"
                                               (Nothing, _, Just _) -> throw $ VisibleError "You don't have a private key"
                                     sessionInsert sessionKeyName $ Text.pack $ show (userId u, userPassId u, priv)
                                     redirect "/" req f
                                   Nothing -> redirectSame req f
    _ -> throw $ VisibleError "Now, what am I supposed to do with that?"

userPage :: CsrfFormApplication
userPage csrf context req f = do
  let lookup name = case name of
                      "csrf" -> return $ toGVal csrf
                      _ -> return def
  page <- runTemplate context Nothing "user" lookup
  f $ responseText status200 [] page

userPage_ :: CsrfVerifiedApplication
userPage_ (params, _) context req f = do
  case (lookup "oldPass" params, lookup "newPass" params, lookup "newPassAgain" params, sessionUser context) of
    (Just old, Just new, Just new2, Just user) -> do
      if Text.null old || Text.null new || new /= new2
        then redirectSame req f
        else updateUserPassword (sessionDbConn context) (userUsername user) old new >>= \success -> redirect "/" req f
    _ -> redirectSame req f
