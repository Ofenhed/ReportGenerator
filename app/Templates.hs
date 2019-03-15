{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Common
import Database.Resolver

listTemplates context req f = do
  let lookup name = case name of
                      _ -> return $ def
  result <- runTemplate "list_templates" lookup
  case result of
    Left _ -> throw $ VisibleError "Could not generate webpage"
    Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
