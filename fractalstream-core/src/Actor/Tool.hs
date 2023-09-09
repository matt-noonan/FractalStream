{-# language OverloadedStrings #-}
module Actor.Tool
  ( Tool(..)
  , ParsedTool(..)
  , ToolInfo(..)
  , RealTool(..)
  , ComplexTool(..)
  ) where

import Actor.Configuration
import Actor.Event
import Actor.Layout

import Data.Aeson
import Data.List (foldl')

data ParsedTool = ParsedTool
  { ptoolInfo :: ToolInfo
  , ptoolDrawLayer :: Int
  , ptoolConfig :: Maybe Configuration
  , ptoolEventHandlers :: ParsedEventHandlers
  }

data Tool = Tool
  { toolInfo :: ToolInfo
  , toolDrawLayer :: Int
  , toolConfig :: Maybe (Layout ConstantExpression)
  , toolEventHandler :: Event -> IO ()
  }

data ToolInfo = ToolInfo
  { tiName :: String
  , tiShortcut :: Maybe Char
  , tiShortHelp :: String
  , tiHelp :: String
  }

newtype RealTool = RealTool ParsedTool
newtype ComplexTool = ComplexTool ParsedTool

instance FromJSON (String -> String -> Either String RealTool) where
  parseJSON = withObject "tool" $ \o -> do
    tiName <- o .: "name"
    tiShortcut <- o .:? "shortcut"
    tiShortHelp <- o .:? "short-help" .!= ""
    tiHelp <- o .:? "help" .!= ""
    let ptoolInfo = ToolInfo{..}
    ptoolConfig <- o .:? "configuration"
    ptoolDrawLayer <- o .:? "draw-to-layer" .!= 100
    handlers <- o .:? "actions" .!= []
    pure $ \x y -> do
      let handlers' = map (($ y) . ($ x)) handlers
      ptoolEventHandlers <-
        foldl' combineEventHandlers (Right noEventHandlers) handlers'
      pure (RealTool ParsedTool{..})

instance FromJSON (String -> Either String ComplexTool) where
  parseJSON = withObject "tool" $ \o -> do
    tiName <- o .: "name"
    tiShortcut <- o .:? "shortcut"
    tiShortHelp <- o .:? "short-help" .!= ""
    tiHelp <- o .:? "help" .!= ""
    let ptoolInfo = ToolInfo{..}
    ptoolConfig <- o .:? "configuration"
    ptoolDrawLayer <- o .:? "draw-to-layer" .!= 100
    handlers <- o .:? "actions" .!= []
    pure $ \z -> do
      let handlers' = map (convertComplexToRealEventHandlers . ($ z)) handlers
      ptoolEventHandlers <-
        foldl' combineEventHandlers (Right noEventHandlers) handlers'
      pure (ComplexTool ParsedTool{..})
