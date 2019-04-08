{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes #-}
module Types (ReportVar(..), ReportContext(..), IOReportContext(..), TemplateVarParent(..), VisibleError(..), Connection, throw, IndexType(..), IndexedReportVar(..), IndexPathType(..), Int64) where 
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

data IndexType = IndexVal Int64
               | IndexArr Int64
               | IndexTempVar Int64
               | IndexTempVars Int64
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
                           , reportVarArray :: Maybe (IndexPathType, [ReportVar]) } deriving Show

data IndexedReportVar = IndexedReportVar ReportVar deriving Show

instance {-# OVERLAPPING #-} ToGVal m [IndexType] where
  toGVal = toGVal . show

instance ToGVal m Int64 where
  toGVal = toGVal . toInteger

lookupGVal lookup stack = def { isNull = False
                              , asLookup = Just $ flip lookup stack }

instance ToGVal m IndexedReportVar where
  toGVal (IndexedReportVar xs) = def { asText = asText $ toGVal ("Still in" :: Text.Text)
                                     , isNull = (isNothing $ reportVarValue xs) && (null $ snd $ fromMaybe ([], []) $ reportVarArray xs)
                                     , asLookup = Just $ flip lookup [("idx", toGVal $ Text.pack $ show $ fst $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                                     ,("val", toGVal $ snd $ fromMaybe ([], Nothing) $ reportVarValue xs)
                                                                     ,("arr", lookupGVal lookup $ [("idx", toGVal $ Text.pack $ show $ fst $ fromMaybe ([], []) $ reportVarArray xs)
                                                                                                  ,("list", toGVal $ map IndexedReportVar $ snd $ fromMaybe ([], []) $ reportVarArray xs)])
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
  toGVal xs = def { asLookup = Just $ \x -> case Map.lookup x xs of
                                              Just res -> Just $ toGVal res
                                              Nothing -> Nothing
                  , isNull = Map.null xs
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

instance ToGVal m Document where
  toGVal = toGVal . documentRoot

instance ToGVal m Element where
  toGVal elem = def { isNull = False
                    , asLookup = Just $ flip lookup [("attr", def { isNull = False
                                                                  , asLookup = Just $ flip Map.lookup $ 
                                                                      Map.mapKeys nameLocalName $
                                                                      Map.map toGVal $ elementAttributes elem})
                                                    ,("type", toGVal $ nameLocalName $ elementName elem)
                                                    ,("children", def { isNull = null $ elementNodes elem
                                                                      , asList = Just $ map toGVal $ elementNodes elem })]
                    , asList = Just $ map toGVal $ elementNodes elem }

instance ToGVal m Node where
  toGVal (NodeElement e) = toGVal e
  toGVal (NodeContent t) = toGVal t
  toGVal _ = def { isNull = True }
