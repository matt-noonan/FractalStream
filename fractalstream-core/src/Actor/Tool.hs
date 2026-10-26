{-# language OverloadedStrings #-}
module Actor.Tool
  ( Tool(..)
  , ToolInfo(..)
  ) where

import FractalStream.Prelude

import Actor.Configuration
import Actor.Event
import Actor.Layout
import Language.Parser.SourceRange (SourceRange(..))

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
  , tiIcon      :: Variable (Maybe String)
    -- ^ An icon name, as given by a unified-script @\@tool@ stanza's
    -- @icon=@ attribute. Not yet rendered anywhere -- threaded through
    -- for forward compatibility until icon display is implemented.
  }

instance Codec ToolInfo where
  codec = do
    name      <-tiName-<      mapped (key "name") $ \_ -> pure nonEmptyString
    shortcut  <-tiShortcut-<  keyWithDefaultValue "" "shortcut"
    shortHelp <-tiShortHelp-< keyWithDefaultValue "" "short-help"
    help      <-tiHelp-<      keyWithDefaultValue "" "help"
    icon      <-tiIcon-<      keyWithDefaultValue Nothing "icon"
    build ToolInfo name shortcut shortHelp help icon

instance CodecWith EventDependencies Tool where
  codecWith_ ctx = do
    ti <-toolInfo-< codec
    layer <-toolDrawLayer-< keyWithDefaultValue 100 "draw-to-layer"
    refreshOnActivate <-toolRefreshOnActivate-< keyWithDefaultValue True  "refresh-on-activation"
    refreshCanUpdate  <-toolRefreshCanUpdate-<  keyWithDefaultValue False "refresh-can-update"
    -- No codec for `Configuration` anymore (the classic project-file
    -- format is gone) -- this field is always `Nothing` now. The only
    -- surviving caller of this instance ('Actor.Ensemble.makeSelectTool',
    -- building the built-in "Select" tool from a small fixed JSON
    -- string) never supplied a `configuration:` key anyway.
    config <-toolConfig-< newOf (pure (pure Nothing))
    showConfig <-toolShowConfig-< newOf (pure $ pure $ const (pure ()))
    script <-toolScript-< optionalField "script"
      newEmptyToolScript
      (\m -> (== "") . unCodeString <$> getDynamic (dyn (source m)))
      (mapped codec $ \use -> do
        let (dsplices, dvc, dlimits, dmpx) = use ctx
        ((\x1 x2 x3 x4 -> (,,,) <$> x1 <*> x2 <*> x3 <*> x4)
          <$> dvc <*> dsplices <*> dlimits <*> dmpx) >>= \case
          Left err -> pure (\_ -> ParsedToolScript $ \_ -> Left (NoSourceRange, err))
          Right (vc, splices, limits, mpx) ->
            pure $ \(CodeString src) ->
              -- Keep the whole (SourceRange, String) pair, not just the
              -- message: 'parseToolScript' already locates this error
              -- precisely, and 'runParsedToolScript's error type carries
              -- a 'SourceRange' now specifically so a caller doing live
              -- editor highlighting (the same way a viewer's own code
              -- already does) doesn't lose it here.
              either (\se -> ParsedToolScript $ \_ -> Left se) id
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
