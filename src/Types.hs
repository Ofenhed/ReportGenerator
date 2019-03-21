{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes #-}
module Types (ReportVar(..), ReportContext(..), IOReportContext(..), TemplateVarParent(..), VisibleError(..), Connection, throw, IndexType(..), IndexedReportVar(..)) where 
import Database.SQLite.Simple (Connection)
import qualified Data.Text as Text
import Control.Exception (Exception(..), throw)
import Data.Maybe (isNothing, fromMaybe)
import Text.Ginger.GVal (ToGVal(..), asHtml, asText, isNull, asList, asLookup, GVal(..))
import qualified Data.Map as Map
import Data.IORef (IORef)
import Data.Default.Class (def)

import Debug.Trace

data IndexType = IndexVal Int
               | IndexArr Int deriving Show

data ReportVar = ReportVar { reportVarValue :: Maybe ([IndexType], Maybe Text.Text)
                           , reportVarVariables :: Map.Map Text.Text ReportVar
                           , reportVarArray :: Maybe ([IndexType], [ReportVar]) } deriving Show

data IndexedReportVar = IndexedReportVar ReportVar deriving Show

instance {-# OVERLAPPING #-} ToGVal m [IndexType] where
  toGVal xs = let val = foldl (\x i -> let before = if x == [] then x else x ++ "."
                                         in before ++ case i of
                                                        IndexVal i -> show i
                                                        IndexArr i -> "[" ++ show i)
                              [] xs
                in toGVal val 

lookupGVal lookup stack = def { isNull = False
                              , asLookup = Just $ flip lookup stack }

instance ToGVal m IndexedReportVar where
  toGVal (IndexedReportVar xs) = def { asText = asText $ toGVal ("Still in" :: Text.Text)
                                     , isNull = (isNothing $ reportVarValue xs) && (null $ snd $ fromMaybe ([], []) $ reportVarArray xs)
                                     -- , asList = if null $ snd $ fromMaybe ([], []) $ reportVarArray xs
                                     --              then Nothing
                                     --              else Just $ map (toGVal . IndexedReportVar) $ snd $ fromMaybe ([], []) $ reportVarArray xs
                                     , asLookup = Just $ flip lookup [("idx", toGVal $ fst $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                                     ,("val", toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                                     ,("arr", lookupGVal lookup $ [("idx", toGVal $ fst $ fromMaybe ([], []) $ reportVarArray xs)
                                                                                                  ,("list", toGVal $ map IndexedReportVar $ snd $ fromMaybe ([], []) $ reportVarArray xs)])
                                                                                                  -- ])
                                                                     ,("children", toGVal $ Map.map IndexedReportVar $ reportVarVariables xs)]}

instance ToGVal m ReportVar where
  toGVal xs = def { asHtml = asHtml $ toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs
                  , asText = asText $ toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs
                  , isNull = (isNothing $ reportVarValue xs) && (null $ snd $ fromMaybe ([], []) $ reportVarArray xs)
                  , asList = if null $ snd $ fromMaybe ([], []) $ reportVarArray xs
                               then Nothing
                               else Just $ map toGVal $ snd $ fromMaybe ([], []) $ reportVarArray xs
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

data VisibleError = VisibleError Text.Text deriving Show
instance Exception VisibleError
