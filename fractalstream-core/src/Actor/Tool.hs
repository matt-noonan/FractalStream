{-# language OverloadedStrings #-}
module Actor.Tool
  ( Tool(..)
--  , ParsedTool(..)
  , ToolInfo(..)
--  , RealTool(..)
--  , ComplexTool(..)
--  , defaultComplexSelectionTool
  ) where

import FractalStream.Prelude

import Actor.Configuration
import Actor.Event
import Actor.Layout

import Data.Codec
import Data.DynamicValue
import Language.Value.Typecheck (Splices)
import Language.Environment (SomeContext)
import qualified Data.Set as Set

data Tool = Tool
  { toolInfo :: ToolInfo
  , toolDrawLayer :: Variable Int
  , toolRefreshOnActivate :: Variable Bool
  , toolRefreshCanUpdate  :: Variable Bool
  , toolConfig :: Variable (Maybe Configuration)
  , toolEventHandlers :: Variable [XEventHandler]
  , toolEventHandler :: Dynamic (Event -> Maybe (IO ()))
  , toolVars :: Dynamic (Set String)
  }

data ToolInfo = ToolInfo
  { tiName      :: Parsed String
  , tiShortcut  :: Variable String
  , tiShortHelp :: Variable String
  , tiHelp      :: Variable String
  }

instance Codec ToolInfo where
  codec = do
    name      <-tiName-<      mapped (key "name") $ \_ -> pure nonEmptyString
    shortcut  <-tiShortcut-<  keyWithDefaultValue "" "shortcut"
    shortHelp <-tiShortHelp-< keyWithDefaultValue "" "short-help"
    help      <-tiHelp-<      keyWithDefaultValue "" "help"
    build ToolInfo name shortcut shortHelp help

instance CodecWith ScriptDependencies Tool where
  codecWith_ ctx = do
    ti <-toolInfo-< codec
    layer <-toolDrawLayer-< keyWithDefaultValue 100 "draw-to-layer"
    refreshOnActivate <-toolRefreshOnActivate-< keyWithDefaultValue True  "refresh-on-activation"
    refreshCanUpdate  <-toolRefreshCanUpdate-<  keyWithDefaultValue False "refresh-can-update"
    config <-toolConfig-< optionalField "configuration"
      (newVariable "" Nothing) (fmap isNothing . getDynamic) (codecWith (fmap (fmap snd) <$> ctx))
    handlers <-toolEventHandlers-< optionalField "actions"
      (newVariable "" []) (fmap null . getDynamic) (codecWith ctx)
    thandlers <-toolEventHandler-< purely $ \_ -> pure (const Nothing) -- FIXME TODO
    tvars <-toolVars-< purely $ \use -> Set.fromList <$> do
      cfg <- use config
      maybe (pure []) (fmap (either (const []) (map fst)) . layoutBindings . coContents) cfg
    build Tool ti layer refreshOnActivate refreshCanUpdate config handlers thandlers tvars

{-
data ParsedTool = ParsedTool
  { ptoolInfo :: ToolInfo
  , ptoolDrawLayer :: Variable Int
  , ptoolRefreshOnActivate :: Variable Bool
  , ptoolRefreshCanUpdate :: Variable Bool
  , ptoolConfig :: Variable (Maybe Configuration)
  , ptoolEventHandlers :: ParsedEventHandlers
  }


newtype RealTool = RealTool ParsedTool
  deriving Show

newtype ComplexTool = ComplexTool ParsedTool
  deriving Show

instance FromJSON (String -> String -> Either String RealTool) where
  parseJSON = withObject "tool" $ \o -> do
    tiName <- o .: "name"
    tiShortcut <- o .:? "shortcut"
    tiShortHelp <- o .:? "short-help" .!= ""
    tiHelp <- o .:? "help" .!= ""
    let ptoolInfo = ToolInfo{..}
    ptoolRefreshOnActivate <- o .:? "refresh-on-activation" .!= True
    ptoolRefreshCanUpdate <- o .:? "refresh-can-update" .!= False
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
    ptoolRefreshOnActivate <- o .:? "refresh-on-activation" .!= True
    ptoolRefreshCanUpdate <- o .:? "refresh-can-update" .!= False
    ptoolConfig <- o .:? "configuration"
    ptoolDrawLayer <- o .:? "draw-to-layer" .!= 100
    handlers <- o .:? "actions" .!= []
    pure $ \z -> do
      let handlers' = map (convertComplexToRealEventHandlers . ($ z)) handlers
      ptoolEventHandlers <-
        foldl' combineEventHandlers (Right noEventHandlers) handlers'
      pure (ComplexTool ParsedTool{..})

defaultComplexSelectionTool :: String -> ParsedTool
defaultComplexSelectionTool name = ParsedTool{..}
  where
    ptoolInfo = ToolInfo
      { tiName = "Select " ++ name
      , tiShortcut = Just 's'
      , tiShortHelp = ""
      , tiHelp = ""
      }
    ptoolDrawLayer = 100
    ptoolRefreshOnActivate = False
    ptoolRefreshCanUpdate = False
    ptoolConfig = Nothing
    ptoolEventHandlers = convertComplexToRealEventHandlers $ noComplexEventHandlers
      { cpehOnClick = Just (Left name, True, "pass")
      , cpehOnDrag = Just (Left name, "INTERNAL__drag_start", True, "pass")
      , cpehOnDragDone = Just (Left name, "INTERNAL__drag_start", True, "pass")
      }
-}
