{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GingerExtra where
import Types

import Text.Ginger.GVal (fromFunction, Function(..), GVal(..), toGVal)
import Text.Ginger.Run (Run(..))
import Text.XML (parseText)
import Text.Ginger (VarName(..))
import Data.Default.Class
import Data.Cvss.V3 (calculateEnvironmentalScore)
import Data.Scientific (fromRationalRepetend)
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy                 as LText

parseXml [(Nothing, d)] = case parseText def $ LText.fromStrict $ asText d of 
                            Left err -> throw $ VisibleError $ Text.append "XML Parsing error: " $ Text.pack $ show err
                            Right p -> return $ toGVal $ p
 
parseCvss [(Nothing, cvss)] = return $ toGVal $ case filter (null . snd) $ reads $ Text.unpack $ asText cvss of
                                                  [(cvss, _)] -> let Right (num, _) = fromRationalRepetend Nothing $ calculateEnvironmentalScore cvss
                                                                   in Just num
                                                  _ -> Nothing

gingerFunctions :: Monad m => [(VarName, Function (Run p m a))]
gingerFunctions = [("merge", \args -> return $ def { isNull = False
                                                   , asLookup = Just $ flip foldl (\_ -> Nothing)
                                                                                  (\state (_, new) -> (\key -> case asLookup new of
                                                                                                                 Just l -> case l key of Nothing -> state key ; v@(Just _) -> v
                                                                                                                 Nothing -> state key))
                                                                                  args })
                  ,("parse_xml", parseXml)
                  ,("parse_cvss3", parseCvss)
                  ,("none", (\[(Nothing, x)] -> return $ toGVal $ isNull x))]
