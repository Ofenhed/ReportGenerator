{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Templates where

import Common
import Database.Resolver
import Database.Writer
import Database.Types
import Csrf
import Redirect
import TemplateFiles

import Text.Ginger.GVal (toGVal, ToGVal(..), dict)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe (isJust)

listTemplates context req f = do
  templates <- getTemplates (sessionDbConn context)
  let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
      lookup name = case name of
                      "templates" -> return $ toGVal templates
                      _ -> return def
  result <- runTemplate context Nothing "list_templates" lookup
  f $ responseText status200 [] result
    
editTemplate :: Int64 -> CsrfFormApplication
editTemplate id csrf context req f = do
  tempAndVars <- getTemplateAndVariables (sessionDbConn context) id
  case tempAndVars of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find template"
    Just (template, variables) -> do
      let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup name = case name of
                          "template" -> return $ toGVal template
                          "variables" -> return $ toGVal variables
                          "csrf" -> return $ toGVal csrf
                          _ -> return def
      result <- runTemplate context Nothing "edit_template" lookup
      f $ responseText status200 [] result
    
editTemplate_ :: Int64 -> CsrfVerifiedApplication
editTemplate_ id (params, _) context req f = do
  _ <- flip (changeTemplate $ sessionDbConn context) id $ \t ->
               case t of
                 Nothing -> (Nothing, False)
                 Just t -> (Just $ t { templateIncludable = case lookup "includable" params of
                                                              Just s -> read (Text.unpack s) :: Int
                                                              Nothing -> templateIncludable t
                                     , templateSource = case lookup "source" params of
                                                          Just s -> s
                                                          Nothing -> templateSource t
                                     , templateEditor = case lookup "editor" params of
                                                          Just s -> s
                                                          Nothing -> templateEditor t }, True)
  redirectSame req f

promptDeleteTemplateVariable tid varid csrf context req f = do
  let lookup name = case name of
                      "csrf" -> return $ toGVal csrf
                      "template_id" -> return $ toGVal tid
                      _ -> return def
  result <- runTemplate context Nothing "delete_template_var" lookup
  f $ responseText status200 [] result

promptDeleteTemplateVariable_ tid varid _ context req f = do
  Database.Writer.deleteTemplateVariable (sessionDbConn context) varid
  redirect (Text.concat ["/template/", Text.pack $ show tid]) req f

addTemplateVar tid parent new csrf context req f = do
  let lookup name = case (name, new) of
                      ("csrf", _) -> return $ toGVal csrf
                      ("type", TemplateVarParentVars _) -> return $ toGVal ("list" :: Text.Text)
                      ("type", TemplateVarParentVar _) -> return $ toGVal ("val" :: Text.Text)
                      _ -> return def
  result <- runTemplate context Nothing "add_template_variable" lookup
  f $ responseText status200 [] result

addTemplateVar_ tid parent new (params, _) context req f = do
  case (new, lookup "name" params, lookup "value" params) of
    (TemplateVarParentVar _, Just name, Just value) -> Database.Writer.addTemplateVariable (sessionDbConn context) parent name value
    (TemplateVarParentVars _, Just name, Nothing) -> Database.Writer.addTemplateArray (sessionDbConn context) parent name
    _ -> throw $ VisibleError "I though we were over this?"
  redirect (Text.concat ["/template/", Text.pack $ show tid]) req f
