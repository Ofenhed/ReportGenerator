{-# LANGUAGE OverloadedStrings #-}
module Autofill where

import Common
import Csrf
import Database.Types
import Redirect
import Encryption

import Database.Resolver
import Database.Writer
import TemplateFiles
import Data.Maybe (mapMaybe)
import qualified Data.Map  as Map
import qualified Data.Text as Text

import Debug.Trace

listAutofill :: WebApplication
listAutofill context req f = do
  templates <- getTemplatesWithLists (sessionDbConn context)
  let lookup :: TemplateLookupType p
      lookup name = case name of
                      "templates" -> return $ toGVal templates
                      _ -> return def
  result <- runTemplate context Nothing "list_autofill" lookup
  f $ responseText status200 [] result

findTemplateVars :: TemplateVarTree -> [TemplateVars]
findTemplateVars (tVars, tVar) = let tVars' = concat $ flip map tVars $ \v -> (v { templateVarsChildren = ([], []) }) : (findTemplateVars $ templateVarsChildren v)
                                     tVar' = concat $ map (findTemplateVars . templateVarChildren) tVar
                                   in tVars' ++ tVar'

flattenTemplateVars :: TemplateVars -> [TemplateVar]
flattenTemplateVars = snd . templateVarsChildren
  -- where
  -- flattenTemplateVars' (tVars, tVar) = let tVars' = concat $ map flattenTemplateVars tVars
  --                                          tVar' = concat $ flip map tVar $ \v -> (v { templateVarChildren = ([], []) }) : (flattenTemplateVars' $ templateVarChildren v)
  --                                        in tVars' ++ tVar'

listAutofillFor :: Int64 -> CsrfFormApplication
listAutofillFor template csrf context req f = do
  tempAndVars <- getTemplateAndVariables (sessionDbConn context) template
  case tempAndVars of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find template"
    Just (template, variables) -> do
      let templateVars = findTemplateVars variables
      savedVars <- flip mapM templateVars $ \tVar -> getSavedTemplateVars (sessionDbConn context) (templateVarsId tVar) Nothing >>= \saved -> return (Text.pack $ show $ templateVarsId tVar, saved)
      let lookup :: TemplateLookupType p
          lookup name = case name of
                          "template" -> return $ toGVal template
                          "arrs" -> return $ toGVal templateVars
                          "saved_vars" -> return $ toGVal $ Map.fromList savedVars
                          "csrf" -> return $ toGVal csrf
                          _ -> return def
      result <- runTemplate context Nothing "list_autofill_for" lookup
      f $ responseText status200 [] result

editAutofill :: Int64 -> Maybe Int64 -> CsrfFormApplication
editAutofill templateId savedVar csrf context req f = do
  t <- getTemplateVars (sessionDbConn context) templateId
  case t of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Can't find template"
    Just (template, vars) -> do
      savedVars <- case savedVar of
                     Just _ -> getSavedTemplateVars (sessionDbConn context) templateId savedVar >>= \x -> case x of
                       [] -> throw $ VisibleErrorWithStatus status404 "Could not find autofill"
                       [j] -> return $ Just j
                     Nothing -> return Nothing
      let lookup :: TemplateLookupType p
          lookup name = case name of
                          "template" -> return $ toGVal template
                          "template_vars" -> return $ toGVal vars
                          "flat_template_vars" -> return $ toGVal $ flattenTemplateVars vars
                          "saved_vars" -> return $ toGVal savedVars
                          "csrf" -> return $ toGVal csrf
                          _ -> return def
      result <- runTemplate context Nothing "edit_autofill" lookup
      f $ responseText status200 [] result

editAutofill_ :: Int64 -> Maybe Int64 -> CsrfVerifiedApplication
editAutofill_ templateId savedVar (params, _) context req f = do
  t <- getTemplateVars (sessionDbConn context) templateId
  case (t, lookup "name" params) of
    (Nothing, _) -> throw $ VisibleErrorWithStatus status404 "Can't find template"
    (Just (_, vars), Just savedName) -> do
      let flat_vars = traceShowId $ flattenTemplateVars vars
          getVars = flip mapMaybe flat_vars $ \var -> let idx = templateVarId var
                                                          idx' = Text.pack $ show idx
                                                        in case (flip lookup params $ Text.append "set_" idx'
                                                                ,flip lookup params $ Text.append "text_" idx') of
                                                             (Just _, Just val) -> Just (idx, (idx, val))
                                                             _ -> Nothing
          changeVar x = x { savedVarsName = savedName
                          , savedVarsDescription = lookup "description" params
                          , savedVarsData = case (lookup "set_main_val" params, lookup "main_val" params) of
                                              (Just _, val) -> val
                                              _ -> Nothing
                          , savedVarsTemplate = templateId
                          , savedVarsVar = Map.fromList getVars }
      newId <- case savedVar of
                 Nothing -> addSavedTemplateVars (sessionDbConn context) templateId $ changeVar def
                 Just idx -> editSavedTemplateVars (sessionDbConn context) (\(Just x) -> (Just $ changeVar x, savedVarsId x)) templateId idx
      redirect (Text.concat ["/autofill/", Text.pack $ show templateId, "/", Text.pack $ show newId]) req f
    _ -> throw $ VisibleError "Missing parameters"

addSavedToReport_ :: CsrfVerifiedApplicationWithEncryptedKey
addSavedToReport_ report encKey (params, _) context req f = do
  case (lookup "path" params, lookup "savedVars" params) of
    (Just path, Just saved) -> do
        newIdx <- copySavedVarsToReport (sessionDbConn context) encKey report (read $ Text.unpack path) (read $ Text.unpack saved)
        case newIdx of
          Just newIdx' -> redirectBackOverridable (sessionHasher context) [("idx", Text.pack $ show newIdx')
                                                                          ,("report_id", Text.pack $ show report)]
                                                                          req f
          Nothing -> throw $ VisibleError "Could not create variables from saved data"
