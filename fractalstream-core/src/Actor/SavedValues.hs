{-# language OverloadedStrings #-}

-- | A project file's @values:@ key: the configuration values the project
-- was left with, where they differ from the defaults in its script --
-- both its own (under @config:@, by variable name) and its tools' (under
-- @tool:@, by viewer title, then tool name, then variable name). Each
-- value is kept as the text it was entered as (see 'Actor.Layout.Field').
--
-- Like a frontend's window layout, these are where a project was left,
-- not part of what it is: the script's own defaults are what it is. So a
-- frontend for exploring (the wx app) restores them, while one for
-- reproducible runs (fsrun) ignores them, and the core just keeps the key
-- with the project's other frontend keys
-- ('Actor.Ensemble.ensembleFrontendProperties').
module Actor.SavedValues
  ( SavedValues(..)
  , noSavedValues
  , readSavedValues
  , writeSavedValues
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

data SavedValues = SavedValues
  { svConfig :: Map String String
  , svTools  :: Map String (Map String (Map String String))
    -- ^ By viewer title, then tool name, then variable name: a tool that
    -- isn't tied to one viewer has a separate set of values in each.
  }
  deriving (Eq, Show)

noSavedValues :: SavedValues
noSavedValues = SavedValues Map.empty Map.empty

instance FromJSON SavedValues where
  parseJSON = withObject "values" $ \o -> SavedValues
    <$> o .:? "config" .!= Map.empty
    <*> o .:? "tool" .!= Map.empty

instance ToJSON SavedValues where
  toJSON SavedValues{..} = object $ catMaybes
    [ if Map.null svConfig then Nothing else Just ("config" .= svConfig)
    , if Map.null tools then Nothing else Just ("tool" .= tools)
    ]
    where tools = Map.filter (not . Map.null) (Map.map (Map.filter (not . Map.null)) svTools)

-- | Read the @values:@ key out of a project file's frontend keys. A
-- project without one has no saved values.
readSavedValues :: KM.KeyMap Value -> Either String SavedValues
readSavedValues = maybe (Right noSavedValues) (parseEither parseJSON) . KM.lookup "values"

-- | Replace the @values:@ key (or remove it, when there are no values to
-- save), leaving the project's other keys alone.
writeSavedValues :: SavedValues -> KM.KeyMap Value -> KM.KeyMap Value
writeSavedValues sv
  | toJSON sv == object [] = KM.delete "values"
  | otherwise = KM.insert "values" (toJSON sv)
