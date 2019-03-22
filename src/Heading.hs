{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Heading where

import Types
import qualified Data.Text as Text
import Text.Ginger.Html (htmlSource, unsafeRawHtml, htmlSource, html, Html)
import Data.Maybe (fromJust, maybe)
import Text.Ginger (toGVal, Function, RuntimeError(ArgumentsError))
import Text.Ginger.Run.Type (liftRun, Run, throwHere)
import Text.Ginger.GVal (fromFunction, GVal(..))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef, IORef())
import Control.Monad.ST (ST(..))
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms (SHA3_512)
import qualified Data.ByteString.Char8 as C8
import Data.Default.Class (def)

chapterRefPrefix = "Chapter-ref-"
refFinder = Text.pack . show . (hash . C8.pack . Text.unpack . (Text.append "Chapter-reference-hashed") :: Text.Text -> Digest SHA3_512)
tableOfContentPlaceholder = Text.pack $ show (hash $ C8.pack $ Text.unpack $ "This is the table of contents, or the reference to it" :: Digest SHA3_512)

type Chapter = (Maybe Text.Text, (Text.Text, Text.Text)) -- (ref, chapter, title)

type IOHeading = IORef Heading

data Heading = Heading { headingCounter :: V.Vector Integer,
                         headingKnown :: V.Vector Chapter,
                         headingRefsUsed :: V.Vector Text.Text,
                         headingCounterStack :: V.Vector Int } deriving (Show)

createHeaderState = do
  ref <- newIORef $ Heading { headingCounter = V.empty, headingKnown = V.empty, headingRefsUsed = V.empty, headingCounterStack = V.empty }
  return ref

pushHeading state [] = do
  liftRun $ atomicModifyIORef' state $ \heading -> (heading { headingCounterStack = V.snoc (headingCounterStack heading) $ V.length $ headingCounter heading }, ())
  return $ toGVal ()

popHeading state [] = do
  _ <- liftRun $ atomicModifyIORef' state $ \heading -> let newLen = (V.length $ headingCounterStack heading) - 1
                                                                in (heading { headingCounterStack = V.take (max 0 newLen) $ headingCounterStack heading}, newLen >= 0)
  return $ toGVal ()

createHeadingRef :: Chapter -> Text.Text
createHeadingRef (Just name, _) = name
createHeadingRef (Nothing, (chp, _)) = Text.append chapterRefPrefix chp

addHeading state level title name = atomicModifyIORef' state $ \heading ->
    let currHeading = V.length $ headingCounter heading
        level' = if V.null (headingCounterStack heading)
                   then level
                   else level + (V.last $ headingCounterStack heading)
        newCounter = case compare level' currHeading of
                       GT -> (V.++) (headingCounter heading)
                                    (V.replicate (level' - currHeading) 1)
                       _ -> let (same, changed) = V.splitAt (level'-1) $ headingCounter heading
                              in V.snoc same ((V.head changed) + 1)
        newChapter = Text.drop 1 $ V.foldl' (\acc new -> Text.append (Text.append acc ".") $ Text.pack $ show new) Text.empty newCounter
        newHeading = (name, (newChapter, title))
        newKnown = V.snoc (headingKnown heading) newHeading
      in (heading { headingCounter = newCounter, headingKnown = newKnown }, (newHeading, level'))
  

createRef :: IORef Heading -> Function (Run p IO h)
createRef state [(Nothing, name)] = do
  let name' = asText name
  name'' <- liftRun $ atomicModifyIORef' state $ \heading ->
                 (if V.elem name' $ headingRefsUsed heading
                       then heading
                       else heading { headingRefsUsed = V.snoc (headingRefsUsed heading) name' }
                    , name')
  return $ toGVal $ unsafeRawHtml $ Text.concat $ ["<a href='#", htmlSource $ asHtml name, "'>", refFinder name'', "</a>"]

finalizeRefs state = do
  heading <- readIORef state
  return $ V.toList $ flip V.map (headingRefsUsed heading) $ \x -> case V.find (\(otherRef, _) -> maybe False ((==)x) otherRef) $ headingKnown heading of
                                                                     Just (_, (chp, _)) -> (refFinder x, chp)
                                                                     Nothing -> throw $ VisibleError $ Text.concat ["Could not find reference '", x, "'"]

finalizeTableOfContents state = do
  heading <- readIORef state
  let toc_line ref@(_, (chp, title)) = Text.concat ["<a href='#", htmlSource $ html $ createHeadingRef ref, "' "
                                                   ,"class='level_", Text.pack $ show $ 1 + Text.count "." chp, "'>"
                                                   , htmlSource $ html chp, " ", htmlSource $ html title
                                                   , "</a>"]
      table_of_contents = V.foldl (\text line -> Text.append text $ toc_line line)
                                  "<div class='table_of_contents'><h1>Table of Contents</h1>"
                                  $ headingKnown heading
      table_of_contents' = Text.append table_of_contents "</div>"
      needle = tableOfContentPlaceholder
  return (needle, table_of_contents')
  -- return $ if Text.length at_needle > 0
  --             then Text.concat [before, table_of_contents', Text.drop (Text.length needle) at_needle]
  --             else before

createHeading :: IORef Heading -> Function (Run p IO h)
createHeading state ((Nothing, level):(Nothing, title):rest) = do
  let level' = floor $ fromJust $ asNumber level
      name = case lookup (Just "id") rest of 
               Just val -> Just $ asText val
               Nothing -> Nothing
  (heading@(_, (chp, _)), real_level) <- liftRun $ addHeading state level' (asText title) name
  let hid = createHeadingRef heading
  if real_level < 1
    then throwHere $ ArgumentsError (Just "createHeading") "expected: (level, title, id=auto)"
    else return $ toGVal $ unsafeRawHtml $ Text.concat $ ["<h", Text.pack $ show real_level, " class='heading' id='", htmlSource $ html hid, "' counter='",
                                                   htmlSource $ html chp, "'>", htmlSource $ asHtml title, "</h", Text.pack $ show real_level, ">"]
