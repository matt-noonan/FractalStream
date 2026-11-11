{-# language OverloadedStrings, RequiredTypeArguments #-}
module Actor.Viewer.Complex
  ( ComplexViewer(..)
--  , listenForChanges
  ) where

import FractalStream.Prelude ()

import Actor.Viewer
import Actor.Layout
import Actor.Tool
import Actor.Event (Coordinate(..), ViewKind(..))
import Actor.Viewer.Sphere (Rotation)

import Data.DynamicValue

import Language.Value.Parser
import Language.Parser.SourceRange

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
  , cvView :: ViewKind
    -- ^ How the viewer starts out showing its points (@view=@).
  , cvViewV :: Variable ViewKind
    -- ^ How it shows them now: a complex viewer can switch between the
    -- plane and the sphere.
  , cvRotationV :: Variable Rotation
    -- ^ The globe's orientation, when it shows the sphere.
  , cvCenterV :: Variable (Double, Double)
    -- ^ The current view's center (the 'Actor.Viewer.vCenter' of the
    -- viewer built from this), which its code and tools can read.
  , cvOriginalRotation :: Rotation
  , cvSpherePixelSize :: Double
    -- ^ The pixel size its sphere view starts with (in units of the
    -- sphere's radius); 'cvPixelSize' is its plane view's.
  , cvPixel :: Mapped (Maybe String) (Either String (Maybe String))
  , cvEscapeRadius :: Parsed (Maybe ParsedValue)
  , cvVanishRadius :: Parsed (Maybe ParsedValue)
  , cvIterationLimit :: Parsed (Maybe ParsedValue)
  , cvCode :: Mapped CodeString (Either (SourceRange, String) SomeViewerWithContext)
  , cvTools :: Variable [Tool]
  , cvOverlay :: Maybe Tool
    -- ^ Its overlay (@\@tool overlay@), if it has one: built like a tool,
    -- but never offered for selection (see 'Actor.Viewer.vRefreshOverlay').
  }

