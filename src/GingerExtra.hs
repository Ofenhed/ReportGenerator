{-# LANGUAGE OverloadedStrings #-}
module GingerExtra where
import Text.Ginger.GVal (fromFunction, Function(..), GVal(..))
import Text.Ginger.Run (Run(..))
import Text.Ginger (VarName(..))
import Data.Default.Class
import qualified Data.Text as Text

gingerFunctions :: Monad m => [(VarName, Function (Run p m a))]
gingerFunctions = [("merge", \args -> return $ def { isNull = False
                                                   , asLookup = Just $ flip foldl (\_ -> Nothing)
                                                                                  (\state (_, new) -> (\key -> case asLookup new of
                                                                                                                 Just l -> case l key of Nothing -> state key ; v@(Just _) -> v
                                                                                                                 Nothing -> state key))
                                                                                  args })]
