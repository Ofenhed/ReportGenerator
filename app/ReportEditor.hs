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

import Text.Ginger.GVal (toGVal, ToGVal(..), dict, fromFunction)
import Text.Ginger.Run (liftRun, runtimeErrorMessage)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe (isJust)

import Debug.Trace

instance ToGVal m (Map.Map Text.Text (GVal m)) where
  toGVal xs = def { asLookup = Just $ flip Map.lookup xs
                  , isNull = Map.null xs
                  }

instance ToGVal m Report where
  toGVal t = dict $ [("id", toGVal $ reportId t)
                    ,("name", toGVal $ reportName t)
                    ,("templateIncludeName", toGVal $ templateIncludeName $ reportTemplate t)
                    ,("templateLongName", toGVal $ templateLongName $ reportTemplate t)
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

data TemplateDataFields = DataFieldBuilder { fieldCheckbox :: [Text.Text],
                                             fieldText :: [Text.Text],
                                             fieldFile :: [Text.Text] }
                        | DataFieldAlreadyUsed deriving (Show, Read)

dataFieldModifier ref func = do
  let modify modifier val = case val of
                              [(_, value)] -> liftRun $ atomicModifyIORef' ref $ \ref -> (modifier ref $ asText value, value)
                              _ -> throw $ VisibleError $ Text.concat ["Incorrect use of function ", func]
  case func of
    "add_text" -> return $ fromFunction $ modify (\d v -> d { fieldText = v:fieldText d })
    "add_checkbox" -> return $ fromFunction $ modify (\d v -> d { fieldCheckbox = v:fieldCheckbox d })
    "add_file" -> return $ fromFunction $ modify (\d v -> d { fieldFile = v:fieldFile d })

    "signed_fields" -> return $ fromFunction $ \_ -> do
                                  key <- liftRun $ atomicModifyIORef' ref $
                                                     \ref -> case ref of
                                                               var@DataFieldBuilder { fieldCheckbox = cb
                                                                                , fieldText = tb
                                                                                , fieldFile = fs } -> (DataFieldAlreadyUsed, Just $ show var)
                                                               DataFieldAlreadyUsed -> (DataFieldAlreadyUsed, Nothing)
                                  case key of
                                    Nothing -> throw $ VisibleError "Fields has already been signed"
                                    Just j -> return $ toGVal j
    _ -> throw $ VisibleError $ Text.concat ["Variable ", func, " does not exist"]
    
editReport :: Int -> CsrfFormApplication
editReport id csrf context req f = do
  reportAndVars <- getReport (sessionDbConn context) id
  toSaveMvar <- newIORef $ DataFieldBuilder { fieldCheckbox = [], fieldText = [], fieldFile = [] }
  case reportAndVars of
    Nothing -> throw $ VisibleError "Could not find template"
    Just (report, context) -> do
      let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup name = case name of
                          "report" -> return $ toGVal report
                          "variables" -> liftRun $ readIORef context >>= return . toGVal . (Map.map IndexedReportVar) . reportContextVariable
                          "custom_variables" -> return $ toGVal [("image_1", "no")]
                          "csrf" -> return $ toGVal csrf
                          _ -> dataFieldModifier toSaveMvar name
      result <- runTemplate "edit_report" lookup
      case result of
        Left e -> throw $ VisibleError $ Text.concat ["Could not generate webpage: ", Text.pack e]
        Right t -> f $ responseText status200 [("Content-Type", "text/html")] $ t
    
saveReport :: Int -> CsrfVerifiedApplication
saveReport id (params, _) context req f = do
  case lookup "fields" params of
    Just f -> throw $ VisibleError $ Text.pack $ show $ fieldText $ read $ Text.unpack f
    Nothing -> throw $ VisibleError "No fields received"
  -- _ <- flip (changeTemplate $ sessionDbConn context) id $ \t ->
  --              case t of
  --                Nothing -> (Nothing, False)
  --                Just t -> (Just $ t { templateIncludable = case lookup "includable" params of
  --                                                             Just s -> read (Text.unpack s) :: Int
  --                                                             Nothing -> templateIncludable t
  --                                    , templateSource = case lookup "source" params of
  --                                                         Just s -> s
  --                                                         Nothing -> templateSource t
  --                                    , templateEditor = case lookup "editor" params of
  --                                                         Just s -> s
  --                                                         Nothing -> templateEditor t }, True)
  redirectSame req f