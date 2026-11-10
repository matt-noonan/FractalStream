-- | What part of the model a viewer is looking at, and the history of
-- its views that "Last view" and "Next view" go back and forth through.
-- (Its own module, so that a viewer's window can carry its history
-- across a rebuild of the project: see 'UI.WxProperties.KeptFrame'.)
module UI.ViewModel
  ( Model(..)
  , History(..)
  , historyBack
  , historyForward
  , historyAppend
  ) where

import Actor.Viewer.Sphere (Rotation)

-- | A model of the view, in terms of the underlying coordinate system.
data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  , modelRotation :: Maybe Rotation
    -- ^ The globe's orientation, when the viewer shows the sphere (then
    -- the center and pixel size are on the screen plane, in units of
    -- the sphere's radius); 'Nothing' on the plane.
  }
  deriving Eq

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
