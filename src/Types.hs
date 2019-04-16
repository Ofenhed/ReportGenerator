{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes #-}
module Types (ReportVar(..), ReportContext(..), IOReportContext(..), TemplateVarParent(..), VisibleError(..), Connection, throw, IndexType(..), IndexedReportVar(..), IndexPathType(..), Int64, gvalMap) where 
import Database.SQLite.Simple (Connection)
import qualified Data.Text as Text
import Control.Exception (Exception(..), throw)
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import Text.Ginger.GVal (ToGVal(..), asHtml, asText, isNull, asList, asLookup, GVal(..))
import qualified Data.Map as Map
import Data.IORef (IORef)
import Data.Default.Class (def)
import Network.HTTP.Types (Status)
import Data.Int (Int64)
import Text.XML (Document(documentRoot), Element(..), Node(..), Name(nameLocalName))

import Debug.Trace

instance ToGVal m a => ToGVal m (Map.Map Text.Text a) where
  toGVal = helper . (Map.map toGVal)
    where
      helper :: Map.Map Text.Text (GVal m) -> GVal m
      helper xs = def { asLookup = Just $ flip Map.lookup xs
                      , asDictItems = Just $ Map.toList xs
                      , asBoolean = not $ Map.null xs
                      , isNull = False
                      }

gvalMap :: [(Text.Text, GVal m)] -> GVal m
gvalMap = toGVal . Map.fromList

data IndexType = IndexVal Int64
               | IndexArr Int64
               | IndexTempVar Int64
               | IndexTempVars Int64 deriving Eq
type IndexPathType = [IndexType]

instance {-# OVERLAPPING #-} Show IndexPathType where
  show = flip foldl [] $ \x i -> x ++ case i of
                                        IndexVal i -> "$" ++ show i
                                        IndexArr i -> "[" ++ show i
                                        IndexTempVar i -> "!" ++ show i
                                        IndexTempVars i -> "{" ++ show i

instance {-# OVERLAPPING #-} Read IndexPathType where
  readsPrec l [] = [([], [])]
  readsPrec l (t:d) = concat $ flip map (readsPrec l d) $
                        \(val, rest) -> flip mapMaybe (readsPrec l rest) $
                                          \(otherVals, rest') -> let idx = createIndex t val
                                                                   in case idx of
                                                                        Just idx' -> Just ((idx':otherVals), rest')
                                                                        Nothing -> Nothing
    where
    createIndex '$' i = Just $ IndexVal i
    createIndex '[' i = Just $ IndexArr i
    createIndex '!' i = Just $ IndexTempVar i
    createIndex '{' i = Just $ IndexTempVars i
    createIndex _ _ = Nothing

data ReportVar = ReportVar { reportVarValue :: Maybe (IndexPathType, Maybe Text.Text)
                           , reportVarVariables :: Map.Map Text.Text ReportVar
                           , reportVarArray :: Maybe (IndexPathType, [ReportVar]) } deriving (Show, Eq)

data IndexedReportVar = IndexedReportVar ReportVar deriving (Show, Eq)

instance {-# OVERLAPPING #-} ToGVal m [IndexType] where
  toGVal = toGVal . show

instance ToGVal m Int64 where
  toGVal = toGVal . toInteger

instance ToGVal m IndexedReportVar where
  toGVal (IndexedReportVar xs) = let mapped = gvalMap [("idx" :: Text.Text, toGVal $ Text.pack $ show $ fst $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                      ,("val", toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                      ,("arr", gvalMap [("idx" :: Text.Text, toGVal $ Text.pack $ show $ fst $ fromMaybe ([], []) $ reportVarArray xs)
                                                                       ,("list", toGVal $ map IndexedReportVar $ snd $ fromMaybe ([], []) $ reportVarArray xs)])
                                                      ,("children", toGVal $ Map.map IndexedReportVar $ reportVarVariables xs)]
                                   in mapped { isNull = (isNothing $ reportVarValue xs) && (null $ snd $ fromMaybe ([], []) $ reportVarArray xs) }

instance ToGVal m ReportVar where
  toGVal xs = let mapped = toGVal $ reportVarVariables xs
                in mapped { asHtml = asHtml $ toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs
                          , asText = asText $ toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs
                          , isNull = (isNothing $ reportVarValue xs) && (null $ snd $ fromMaybe ([], []) $ reportVarArray xs)
                          , asList = if null $ snd $ fromMaybe ([], []) $ reportVarArray xs
                                       then Nothing
                                       else Just $ map toGVal $ snd $ fromMaybe ([], []) $ reportVarArray xs
                          , Text.Ginger.GVal.length = Just $ maybe 0 (Prelude.length . snd)  $ reportVarArray xs
                          , asDictItems = Nothing
                          }

data ReportContext = ReportContext { reportContextId :: Int64
                                   , reportContextVariable :: Map.Map Text.Text ReportVar
                                   , reportContextCustomVariable :: Map.Map Text.Text Text.Text } deriving Show
type IOReportContext = IORef ReportContext

data TemplateVarParent = TemplateVarParent Int64
                       | TemplateVarParentVar Int64
                       | TemplateVarParentVars Int64 deriving Show

data VisibleError = VisibleError Text.Text
                  | VisibleErrorWithStatus Status Text.Text deriving Show
instance Exception VisibleError

-- XML

instance ToGVal m Document where
  toGVal = toGVal . documentRoot

instance ToGVal m Element where
  toGVal elem = let mapped = gvalMap [("attr", toGVal $ Map.mapKeys nameLocalName $ elementAttributes elem)
                                     ,("type", toGVal $ nameLocalName $ elementName elem)
                                     ,("children", toGVal $ elementNodes elem)]
                  in mapped { isNull = False
                            , asList = Just $ map toGVal $ elementNodes elem
                            , Text.Ginger.GVal.length = Just $ Prelude.length $ elementNodes elem
                            , asDictItems = Nothing }

instance ToGVal m Node where
  toGVal (NodeElement e) = toGVal e
  toGVal (NodeContent t) = toGVal t
  toGVal _ = def { isNull = True }

