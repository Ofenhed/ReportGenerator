{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ReportEditor where

import Common
import Database.Resolver
import Database.Writer
import Database.Types
import Csrf
import Redirect

import Text.Ginger.GVal (toGVal, ToGVal(..), dict)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import Data.IORef (readIORef)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe (isJust)

instance ToGVal m (Map.Map Text.Text (GVal m)) where
  toGVal xs = def { asLookup = Just $ flip Map.lookup xs
                  , isNull = Map.null xs
                  }

instance ToGVal m Report where
  toGVal t = dict $ [("id", toGVal $ reportId t)
                    ,("name", toGVal $ reportName t)
                    ,("reportIncludeName", toGVal $ templateIncludeName $ reportTemplate t)
                    ,("reportLongName", toGVal $ templateLongName $ reportTemplate t)
                    ,("editor", toGVal $ templateEditor $ reportTemplate t)]

listReports context req f = do
  reports <- getReports (sessionDbConn context)
  let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
      lookup name = case name of
                      "reports" -> return $ toGVal reports
                      _ -> return $ def
  result <- runTemplate "list_reports" lookup
  case result of
    Left _ -> throw $ VisibleError "Could not generate webpage"
    Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
editReport :: Int -> CsrfFormApplication
editReport id csrf context req f = do
  tempAndVars <- getReport (sessionDbConn context) id
  case tempAndVars of
    Nothing -> throw $ VisibleError "Could not find template"
    Just (template, context) -> do
      let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup name = case name of
                          "template" -> return $ toGVal template
                          "variables" -> liftRun $ readIORef context >>= return . toGVal . reportContextVariable
                          "custom_variables" -> return $ toGVal [("image_1", "no")]
                          "csrf" -> return $ toGVal csrf
                          _ -> return $ def
      result <- runTemplate "edit_report" lookup
      case result of
        Left e -> throw $ VisibleError $ Text.concat ["Could not generate webpage: ", Text.pack e]
        Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
-- saveTemplate :: Int -> CsrfVerifiedApplication
-- saveTemplate id (params, _) context req f = do
--   _ <- flip (changeTemplate $ sessionDbConn context) id $ \t ->
--                case t of
--                  Nothing -> (Nothing, False)
--                  Just t -> (Just $ t { templateIncludable = case lookup "includable" params of
--                                                               Just s -> read (Text.unpack s) :: Int
--                                                               Nothing -> templateIncludable t
--                                      , templateSource = case lookup "source" params of
--                                                           Just s -> s
--                                                           Nothing -> templateSource t
--                                      , templateEditor = case lookup "editor" params of
--                                                           Just s -> s
--                                                           Nothing -> templateEditor t }, True)
--   redirectSame req f
