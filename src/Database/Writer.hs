{-# LANGUAGE OverloadedStrings #-}
module Database.Writer where

import Database.Types
import Types

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import qualified Data.Text as Text
import qualified Data.Map as Map

changeTemplate :: Connection -> (Maybe Template -> (Maybe Template, a)) -> Int -> IO a
changeTemplate conn f id = do
  template <- query conn "SELECT * FROM Template WHERE id == ?" (Only id)
  let var = case template of
              [t] -> Just t
              _ -> Nothing
      (newTemplate, ret) = f var
  case (var, newTemplate) of
    (Nothing, Just _) -> throw $ VisibleError "Tried to create a new template in a change function"
    (Just _, Just newTemplate') ->
      if templateId newTemplate' /= id
        then throw $ VisibleError "Tried to change id of a template"
        else do
          executeNamed conn "UPDATE Template SET includeName = :includeName\
                                              \, longName = :longName \
                                              \, description = :description \
                                              \, source = :source \
                                              \, includable = :includable \
                                              \ WHERE id== :id"
                            [":includeName" := templateIncludeName newTemplate'
                            ,":longName" := templateLongName newTemplate'
                            ,":description" := templateDescription newTemplate'
                            ,":source" := templateSource newTemplate'
                            ,":includable" := templateIncludable newTemplate'
                            ,":id" := id]
          return ret
        
    _ -> return ret
