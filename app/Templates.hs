{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Templates where

import Common
import Database.Resolver
import Database.Writer
import Database.Types
import Csrf
import Redirect

import Text.Ginger.GVal (toGVal, ToGVal(..), dict)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe (isJust)

instance ToGVal m (Map.Map Text.Text (GVal m)) where
  toGVal xs = def { asLookup = Just $ flip Map.lookup xs
                  , isNull = Map.null xs
                  }

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
                    ,("name", toGVal $ templateVarsName t)
                    ,("description", toGVal $ templateVarsDescription t)
                    ,("children", toGVal $ templateVarsChildren t)]
instance ToGVal m TemplateVar where
  toGVal t = dict $ [("id", toGVal $ templateVarId t)
                    ,("name", toGVal $ templateVarName t)
                    ,("description", toGVal $ templateVarDescription t)
                    ,("default", toGVal $ templateVarDefault t)
                    ,("children", toGVal $ templateVarChildren t)]
                                         

listTemplates context req f = do
  templates <- getTemplates (sessionDbConn context)
  let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
      lookup name = case name of
                      "templates" -> return $ toGVal templates
                      _ -> return $ def
  result <- runTemplate "list_templates" lookup
  case result of
    Left _ -> throw $ VisibleError "Could not generate webpage"
    Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
editTemplate :: Int -> CsrfFormApplication
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
                          _ -> return $ def
      result <- runTemplate "edit_template" lookup
      case result of
        Left e -> throw $ VisibleError $ Text.concat ["Could not generate webpage: ", Text.pack e]
        Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
saveTemplate :: Int -> CsrfVerifiedApplication
saveTemplate id (params, _) context req f = do
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
