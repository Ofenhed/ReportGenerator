{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Types where

import qualified Network.Wai.Session as S
import Database.SQLite.Simple (Connection)
import qualified Data.Text as Text
import qualified Data.Vault.Lazy                as Vault
import Control.Exception (Exception(..))
import Data.Maybe (isNothing, fromMaybe)
import Text.Ginger.GVal (ToGVal(..), asHtml, asText, isNull, asList, asLookup)
import qualified Data.Map as Map
import Data.IORef (IORef)
import Data.Default.Class (def)

data ReportVar = ReportVar { reportVarValue :: Maybe Text.Text
                           , reportVarVariables :: Map.Map Text.Text ReportVar
                           , reportVarArray :: [ReportVar] } deriving Show

instance ToGVal m ReportVar where
  toGVal xs = def { asHtml = asHtml $ toGVal $ fromMaybe "" $ reportVarValue xs
                  , asText = asText $ toGVal $ fromMaybe "" $ reportVarValue xs
                  , isNull = (isNothing $ reportVarValue xs) && (null $ reportVarArray xs)
                  , asList = if null $ reportVarArray xs
                               then Nothing
                               else Just $ map toGVal $ reportVarArray xs
                  , asLookup = Just $ flip Map.lookup $ Map.map toGVal $ reportVarVariables xs
                  }

instance ToGVal m a => ToGVal m (Map.Map Text.Text a) where
  toGVal xs = def { asLookup = Just $ flip Map.lookup $ Map.map toGVal xs
                  , isNull = Map.null xs
                  }

data ReportContext = ReportContext { reportContextId :: Int
                                   , reportContextVariable :: Map.Map Text.Text ReportVar
                                   , reportContextCustomVariable :: Map.Map Text.Text Text.Text } deriving Show
type IOReportContext = IORef ReportContext

data TemplateVarParent = TemplateVarParent Int
                       | TemplateVarParentVar Int
                       | TemplateVarParentVars Int deriving Show

data SessionType = Session { sessionDbConn :: Connection
                           , sessionSession :: Vault.Key (S.Session IO Text.Text Text.Text) }

data VisibleError = VisibleError Text.Text deriving Show
instance Exception VisibleError
