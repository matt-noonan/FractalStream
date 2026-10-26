{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
--  , listenForChanges
  ) where

import FractalStream.Prelude

import Actor.Viewer
import Actor.Layout
import Actor.Tool

import Data.DynamicValue

import Language.Value.Parser
import Language.Parser.SourceRange

-- | Placeholder for the classic @preparation:@ YAML key's raw payload,
-- which no codec produces anymore -- 'cvPrep' is always 'Nothing' now,
-- kept only because removing the field would mean updating every
-- 'ComplexViewer' construction site for no present benefit.
data PrepRaw

data ComplexViewer = ComplexViewer
  { cvTitle :: Parsed String
  , cvSize :: Variable Dimensions
  , cvPosition :: Variable Dimensions
  , cvCanResize :: Variable Bool
  , cvCenter :: Parsed (Complex Double)
  , cvPixelSize :: Parsed Double
  , cvCoord :: Parsed String
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  , cvEscapeRadius :: Parsed (Maybe ParsedValue)
  , cvVanishRadius :: Parsed (Maybe ParsedValue)
  , cvIterationLimit :: Parsed (Maybe ParsedValue)
  , cvPrep :: Variable (Maybe PrepRaw)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerWithContext)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }

