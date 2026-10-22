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
  , toolScript :: Mapped CodeString ParsedToolScript
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
    script <-toolScript-< optionalField "script"
      newEmptyToolScript
      (\m -> (== "") . unCodeString <$> getDynamic (dyn (source m)))
      (mapped codec $ \use -> do
        let (dsplices, dvc, dlimits, dmpx) = use ctx
        ((\x1 x2 x3 x4 -> (,,,) <$> x1 <*> x2 <*> x3 <*> x4)
          <$> dvc <*> dsplices <*> dlimits <*> dmpx) >>= \case
          Left err -> pure (\_ -> ParsedToolScript $ \_ -> Left err)
          Right (vc, splices, limits, mpx) ->
            pure $ \(CodeString src) ->
              either (\(_, e) -> ParsedToolScript $ \_ -> Left e) id
                (parseToolScript splices vc limits mpx src))
    tvars <-toolVars-< purely $ \use -> Set.fromList <$> do
      cfg <- dyn (use config)
      maybe (pure []) (fmap (either (const []) (map fst)) . layoutBindings . coContents) cfg
    handler <-toolEventHandler-< newOf (pure $ pure (\_ _ -> Nothing))
    build Tool ti layer refreshOnActivate refreshCanUpdate config showConfig script handler tvars

-- | A @script:@ field whose default (when the field is absent) is an empty
-- script with no event handlers.
newEmptyToolScript :: IO (Mapped CodeString ParsedToolScript)
newEmptyToolScript = newVariable (CodeString "") >>= newMapped (pure (const emptyToolScript))
