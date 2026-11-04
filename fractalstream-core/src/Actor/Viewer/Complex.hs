{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
--  , listenForChanges
  ) where

import FractalStream.Prelude ()

import Actor.Viewer
import Actor.Layout
import Actor.Tool
import Actor.Event (Coordinate(..))

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
  , cvCenter :: (Double, Double)
  , cvPixelSize :: Double
  , cvAspect :: Double
    -- ^ The height of a pixel over its width (always 1 for a complex viewer).
  , cvCoord :: Coordinate
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  , cvEscapeRadius :: Parsed (Maybe ParsedValue)
  , cvVanishRadius :: Parsed (Maybe ParsedValue)
  , cvIterationLimit :: Parsed (Maybe ParsedValue)
  , cvPrep :: Variable (Maybe PrepRaw)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerWithContext)
  --, cvOverlay :: Variable (Maybe String)
  , cvTools :: Variable [Tool]
  }

