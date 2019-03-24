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
import qualified Data.Text.Encoding as Encoding
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8      as C8

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
  f $ responseText status200 [("Content-Type", "text/html")] result

data TemplateDataFields = DataFieldBuilder { fieldCheckbox :: [Text.Text],
                                             fieldValue :: [Text.Text],
                                             fieldFile :: [Text.Text] }
                        | DataFieldAlreadyUsed deriving (Show, Read)

dataFieldModifier ref func = do
  let modify modifier val = case val of
                              [(_, value)] -> liftRun $ atomicModifyIORef' ref $ \ref -> (modifier ref $ asText value, value)
                              _ -> throw $ VisibleError $ Text.concat ["Incorrect use of function ", func]
  case func of
    "add_value" -> return $ fromFunction $ modify (\d v -> d { fieldValue = v:fieldValue d })
    "add_checkbox" -> return $ fromFunction $ modify (\d v -> d { fieldCheckbox = v:fieldCheckbox d })
    "add_file" -> return $ fromFunction $ modify (\d v -> d { fieldFile = v:fieldFile d })

    "signed_fields" -> return $ fromFunction $ \_ -> do
                                  key <- liftRun $ atomicModifyIORef' ref $
                                                     \ref -> case ref of
                                                               var@DataFieldBuilder { fieldCheckbox = cb
                                                                                , fieldValue = tb
                                                                                , fieldFile = fs } -> (DataFieldAlreadyUsed, Just $ show var)
                                                               DataFieldAlreadyUsed -> (DataFieldAlreadyUsed, Nothing)
                                  case key of
                                    Nothing -> throw $ VisibleError "Fields has already been signed"
                                    Just j -> return $ toGVal j
    _ -> throw $ VisibleError $ Text.concat ["Variable ", func, " does not exist"]
    
editReport :: Int -> CsrfFormApplication
editReport id csrf context req f = do
  reportAndVars <- getReport (sessionDbConn context) id
  toSaveMvar <- newIORef $ DataFieldBuilder { fieldCheckbox = [], fieldValue = [], fieldFile = [] }
  case reportAndVars of
    Nothing -> throw $ VisibleErrorWithStatus status404 "Could not find report"
    Just (report, context) -> do
      let lookup :: VarName -> Run p IO Html (GVal (Run p IO Html))
          lookup name = case name of
                          "report" -> return $ toGVal report
                          "variables" -> liftRun $ readIORef context >>= return . toGVal . (Map.map IndexedReportVar) . reportContextVariable
                          "custom_variables" -> return $ toGVal [("image_1", "no")]
                          "csrf" -> return $ toGVal csrf
                          -- "delete_template_var" -> \vars -> case vars of
                          --                                     ["delete_id", var] -> do
                          --                                       let var' = read $ asText var :: IndexPathType
                          --                                       case last var' of
                          --                                         IndexVal i -> throw $ VisibleError "Not yet implemented."
                          --                                         IndexArr i -> throw $ VisibleError "Not yet implemented."
                          --                                         _ -> throw $ VisibleError "Invalid report ID."
                          --                                     _ -> throw $ VisibleError "Function delete_template_var expects one named variable, (delete_id=varid)"
                          _ -> dataFieldModifier toSaveMvar name
      result <- runTemplate "edit_report" lookup
      f $ responseText status200 [("Content-Type", "text/html")] result
    
saveReport :: Int -> CsrfVerifiedApplication
saveReport id (params, files) context req f = do
  variables <- case lookup "fields" params of
                 Just f -> return $ (read $ Text.unpack f :: TemplateDataFields)
                 Nothing -> throw $ VisibleError "No fields received"
  flip mapM (fieldValue variables) $ \variable -> do
    setVariable (sessionDbConn context) id (read $ Text.unpack variable) $ lookup variable params
  flip mapM (fieldCheckbox variables) $ \variable -> do
    setVariable (sessionDbConn context) id (read $ Text.unpack variable) $ if isJust $ lookup variable params
                                                                             then (1 :: Int)
                                                                             else 0
  flip mapM (fieldFile variables) $ \file -> do
    case lookup (Encoding.encodeUtf8 file) files of
      Just f -> if (fileName f == C8.empty || fileName f == C8.pack "\"\"") && fileContent f == LC8.empty -- Bug in Wai/Warp makes filenames contain "" when really empty
                  then return True
                  else setVariable (sessionDbConn context) id (read $ Text.unpack file) $ Encoding.decodeUtf8 $ LC8.toStrict $ fileContent f
      Nothing -> return True
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
