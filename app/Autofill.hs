{-# LANGUAGE OverloadedStrings #-}
module Autofill where

import Common
import Database.Resolver
import TemplateFiles

listAutofill :: WebApplication
listAutofill context req f = do
  templates <- getTemplatesWithLists (sessionDbConn context)
  let lookup :: TemplateLookupType p
      lookup name = case name of
                      "templates" -> return $ toGVal templates
                      _ -> return def
  result <- runTemplate context Nothing "list_autofill" lookup
  f $ responseText status200 [] result

