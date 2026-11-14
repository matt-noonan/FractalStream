-- | What part of the model a viewer is looking at, and the history of
-- its views that "Last view" and "Next view" go back and forth through.
-- (Its own module, so that a viewer's window can carry its history
-- across a rebuild of the project: see the desktop's
-- 'UI.WxProperties.KeptFrame'; the web app's engine keeps each viewer's.)
module Actor.Viewer.ViewModel
  ( Model(..)
  , ModelShape(..)
  , modelRotation
  , modelIsometry
  , modelIsFlat
  , History(..)
  , historyBack
  , historyForward
  , historyAppend
  ) where

import Actor.Viewer.Sphere (Rotation)
import Actor.Viewer.Disk (Isometry, HyperbolicModel, HyperbolicDisplay)

-- | A model of the view, in terms of the underlying coordinate system.
data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  , modelShape    :: ModelShape
    -- ^ How the model's points reach the screen plane. Except on the
    -- plane, the center and pixel size are on the screen plane, in units
    -- of the sphere's (or the disk's) radius.
  }
  deriving Eq

-- | The plane (the screen plane is the model's), the globe with its
-- orientation, or the hyperbolic plane: the script's model, the display
-- (the disk or the half-plane), and the view (see "Actor.Viewer.Disk").
data ModelShape = FlatShape | GlobeShape Rotation | HyperbolicShape HyperbolicModel HyperbolicDisplay Isometry
  deriving Eq

-- | The globe's orientation, when the viewer shows the sphere.
modelRotation :: Model -> Maybe Rotation
modelRotation m = case modelShape m of { GlobeShape r -> Just r; _ -> Nothing }

-- | The view of the hyperbolic plane, when the viewer shows it.
modelIsometry :: Model -> Maybe Isometry
modelIsometry m = case modelShape m of { HyperbolicShape _ _ v -> Just v; _ -> Nothing }

-- | Whether the screen plane is the model's (on the plane).
modelIsFlat :: Model -> Bool
modelIsFlat m = modelShape m == FlatShape

-- | The views before the current one (most recent first), the current
-- one, and the ones after it (gone back from, nearest first).
data History = History [Model] Model [Model]

historyBack :: History -> Maybe (History, Model)
historyBack (History olds m news) = case olds of
  (m':olds') -> Just (History olds' m' (m:news), m')
  _ -> Nothing

historyForward :: History -> Maybe (History, Model)
historyForward (History olds m news) = case news of
  (m':news') -> Just (History (m:olds) m' news', m')
  _ -> Nothing

historyAppend :: Model -> History -> History
historyAppend m h@(History olds m' _)
  | m == m'   = h
  | otherwise = History (m' : olds) m []
