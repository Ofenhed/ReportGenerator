{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GingerExtra where
import Types

import Text.Ginger.GVal (fromFunction, Function(..), GVal(..), toGVal, keys)
import Text.Ginger.Run (Run(..), liftRun)
import Text.XML (parseText)
import Text.Ginger (VarName(..))
import Data.Default.Class
import Data.Cvss.V3 (calculateEnvironmentalScore)
import Data.Scientific (fromRationalRepetend)
import Data.Time.LocalTime (utcToZonedTime, hoursToTimeZone)
import Data.Time.Clock.System (SystemTime(MkSystemTime), systemToUTCTime)
import Data.Scientific (toBoundedInteger)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy                 as LText

parseXml [(Nothing, d)] = case parseText def $ LText.fromStrict $ asText d of 
                            Left err -> throw $ VisibleError $ Text.append "XML Parsing error: " $ Text.pack $ show err
                            Right p -> return $ toGVal $ p
 
parseCvss [(Nothing, cvss)] = return $ toGVal $ case filter (null . snd) $ reads $ Text.unpack $ asText cvss of
                                                  [(cvss, _)] -> let Right (num, _) = fromRationalRepetend Nothing $ calculateEnvironmentalScore cvss
                                                                   in Just num
                                                  _ -> Nothing

parsePosixTime [one_arg] = parsePosixTime [one_arg, (Nothing, toGVal 0)]
parsePosixTime [(Nothing, time), (Nothing, offset)] =
  return $ toGVal $ case (asNumber time, asNumber offset) of
    (Just time', Just offset') -> case (toBoundedInteger time', toBoundedInteger offset') of
                                    (Just time'', Just offset'') -> Just $ utcToZonedTime (hoursToTimeZone offset'') $ systemToUTCTime $ MkSystemTime time'' 0
                                    _ -> Nothing
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
                  ,("none", \[(Nothing, x)] -> return $ toGVal $ isNull x)
                  ,("raise_visible", \[(Nothing, x)] -> throw $ VisibleError $ asText x)
                  ,("keys", \[(Nothing, x)] -> return $ toGVal $ keys x)
                  ,("parse_posix_time", parsePosixTime)]
