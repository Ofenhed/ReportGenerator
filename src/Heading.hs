{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Heading where

import qualified Data.Text as Text
import Text.Ginger.Html (htmlSource, unsafeRawHtml, htmlSource, html)
import Data.Maybe (fromJust)
import Text.Ginger (toGVal, Function, RuntimeError(ArgumentsError))
import Text.Ginger.Run.Type (liftRun, Run, throwHere)
import Text.Ginger.GVal (fromFunction, GVal(..))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef, IORef())
import Control.Monad.ST (ST(..))
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms (SHA3_512)
import Data.ByteString.Char8 as C8

import Debug.Trace

chapterRefPrefix = "Chapter-ref-"
refFinder = Text.pack . show . (hash . C8.pack . Text.unpack :: Text.Text -> Digest SHA3_512)

type Chapter = (Maybe Text.Text, (Text.Text, Text.Text)) -- (ref, chapter, title)

data Heading = Heading { headingCounter :: V.Vector Integer,
                         headingKnown :: V.Vector Chapter,
                         headingRefsUsed :: V.Vector Text.Text } deriving (Show)

createHeaderState = do
  ref <- newIORef $ Heading { headingCounter = V.empty, headingKnown = V.empty, headingRefsUsed = V.empty }
  return ref

createHeadingRef :: Chapter -> Text.Text
createHeadingRef (Just name, _) = name
createHeadingRef (Nothing, (chp, _)) = Text.append chapterRefPrefix chp

addHeading state level title name = atomicModifyIORef' state $ \heading ->
    let currHeading = V.length $ headingCounter heading
        level' = level currHeading
        newCounter = case compare level' currHeading of
                       GT -> (V.++) (headingCounter heading)
                                    (V.replicate (level' - currHeading) 1)
                       _ -> let (same, changed) = V.splitAt (level'-1) $ headingCounter heading
                              in V.snoc same ((V.head changed) + 1)
        newChapter = Text.drop 1 $ V.foldl' (\acc new -> Text.append (Text.append acc ".") $ Text.pack $ show new) Text.empty newCounter
        newHeading = (name, (newChapter, title))
        newKnown = V.snoc (headingKnown heading) newHeading
      in (Heading { headingCounter = newCounter, headingKnown = newKnown,  headingRefsUsed = headingRefsUsed heading}, newHeading)
  

createRef :: Monad m => IORef Heading -> Function (Run p m h)
createRef state [(Nothing, name)] = do
  let name' = asText name
      name'' = unsafePerformIO $ atomicModifyIORef' state $ \heading ->
                 (if V.elem name' $ headingRefsUsed heading
                       then heading
                       else Heading { headingCounter = headingCounter heading
                                    , headingKnown = headingKnown heading
                                    , headingRefsUsed = V.snoc (headingRefsUsed heading) name'}
                    , name')
  return $ toGVal $ unsafeRawHtml $ Text.concat $ ["<a href='#", htmlSource $ asHtml name, "'>", refFinder name'', "</a>"]

finalizeRefs state !t = do
  let heading = unsafePerformIO $ readIORef state
      modifier =  \text ref ->
        case (text, V.find (\(other_ref, (chp, _)) -> case other_ref of Just other_ref' -> other_ref' == ref ; _ -> False) $ headingKnown heading) of
           (Left err, _) -> Left err
           (Right text', Just (_, (chp, _))) -> Right $ Text.replace (refFinder ref) chp text'
           (Right _, Nothing) -> Left $ Text.concat ["Could not find reference ", ref]
  traceShowM heading
           
  return $ V.foldl modifier (Right t) $ headingRefsUsed heading

createHeading :: Monad m => IORef Heading -> Function (Run p m h)
createHeading state ((Nothing, level):(Nothing, title):rest) = do
  let level' = floor $ fromJust $ asNumber level
      level'' = Text.pack $ show level'
      name = case lookup (Just "id") rest of 
               Just val -> Just $ asText val
               Nothing -> Nothing
      heading@(_, (chp, _)) = unsafePerformIO $ addHeading state (\_ -> level') (asText title) name
  let hid = createHeadingRef heading
  if level' < 1
    then throwHere $ ArgumentsError (Just "createHeading") "expected: (level, title, id=auto)"
    else return $ toGVal $ unsafeRawHtml $ Text.concat $ ["<h", level'', " class='heading' id='", htmlSource $ html hid, "'>",
                                                   htmlSource $ html chp, " ", htmlSource $ asHtml title, "</h", level'', ">"]
