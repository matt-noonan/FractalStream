{-# language OverloadedStrings #-}
module Actor.Tool
  ( Tool(..)
  , ToolInfo(..)
  ) where

import FractalStream.Prelude

import Actor.Configuration
import Actor.Event
import Actor.Layout

import Data.Codec
import Data.DynamicValue
import qualified Data.Set as Set

data Tool = Tool
  { toolInfo :: ToolInfo
  , toolDrawLayer :: Variable Int
  , toolRefreshOnActivate :: Variable Bool
  , toolRefreshCanUpdate  :: Variable Bool
  , toolConfig :: Variable (Maybe Configuration)
  , toolShowConfig :: Variable (Bool -> IO ())
  , toolEventHandlers :: Variable [SingleEventHandler]
  , toolEventHandler :: Variable (Double -> Event -> Maybe (IO ()))
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

instance CodecWith EventDependencies Tool where
  codecWith_ ctx = do
    ti <-toolInfo-< codec
    layer <-toolDrawLayer-< keyWithDefaultValue 100 "draw-to-layer"
    refreshOnActivate <-toolRefreshOnActivate-< keyWithDefaultValue True  "refresh-on-activation"
    refreshCanUpdate  <-toolRefreshCanUpdate-<  keyWithDefaultValue False "refresh-can-update"
    config <-toolConfig-< optionalField "configuration"
      (newVariable Nothing) (fmap isNothing . getDynamic) (codecWith ((\(x,_,_,_) -> x) <$> ctx))
    showConfig <-toolShowConfig-< newOf (pure $ pure $ const (pure ()))
    handlers <-toolEventHandlers-< optionalField "actions"
      (newVariable []) (fmap null . getDynamic) (codecWith ctx)
    tvars <-toolVars-< purely $ \use -> Set.fromList <$> do
      cfg <- dyn (use config)
      maybe (pure []) (fmap (either (const []) (map fst)) . layoutBindings . coContents) cfg
    handler <-toolEventHandler-< newOf (pure $ pure (\_ _ -> Nothing))
    build Tool ti layer refreshOnActivate refreshCanUpdate config showConfig handlers handler tvars
