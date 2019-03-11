{-# LANGUAGE OverloadedStrings #-}
module ReportGenerator where

import Database
import Heading

import qualified Data.Text as Text

import Data.Default.Class
import Text.Ginger.Run (makeContextHtml, runGinger)
import Text.Ginger.Parse (parseGinger)
import Text.Ginger (toGVal, ParserError(..))
import Text.Ginger.GVal (fromFunction, GVal(..))
import Text.Ginger.Html (htmlSource, unsafeRawHtml, htmlSource, html)
import Text.Ginger.AST (Template)
import Data.Maybe (maybe)

includeResolver conn file = do
  case Text.splitOn "/" $ Text.pack file of
    [".", "template", t] -> getTemplate conn t True >>= return . (maybe Nothing (Just . Text.unpack))
    _ -> return $ Just $ "This should not have been included, " ++ file ++ " should have been!"
    -- _ -> return $ Nothing


render conn template = do
  headerState <- createHeaderState
  Right parsed <- parseGinger (includeResolver conn) Nothing template
  let contextLookup varName = do
           case varName of
             "username" -> toGVal $ Text.pack "haxxor"
             "customer" -> toGVal $ Text.pack "Some customer"
             "header" -> fromFunction $ createHeading headerState
             "confidential" -> toGVal True
             "ref" -> fromFunction $ createRef headerState
             "table_of_contents" -> toGVal $ tableOfContentPlaceholder
             _ -> def
      context = makeContextHtml contextLookup
      source = htmlSource $ runGinger context parsed
  finalized <- finalizeRefs headerState source
  case finalized of
    Right h -> finalizeTableOfContents headerState h >>= return
    Left err -> return $ Text.concat ["Error: ", err]
