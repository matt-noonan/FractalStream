-- | What part of the model a viewer is looking at, and the history of
-- its views that "Last view" and "Next view" go back and forth through.
-- (Its own module, so that a viewer's window can carry its history
-- across a rebuild of the project: see the desktop's
-- 'UI.WxProperties.KeptFrame'; the web app's engine keeps each viewer's.)
module Actor.Viewer.ViewModel
  ( Model(..)
  , ModelShape(..)
  , modelGlobe
  , modelIsometry
  , modelIsFlat
  , ViewSettings(..)
  , viewSettings
  , settingsView
  , fitGlobe
  , History(..)
  , historyBack
  , historyForward
  , historyAppend
  ) where

import Actor.Viewer.Sphere (Globe(..), globeCenter, rollOf, globeFacingRolled, clampGlobeZoom, globeScreenPixelSize)
import Actor.Viewer.Disk ( Isometry, HyperbolicModel, HyperbolicDisplay(..), screenToModelH, viewShowingTurned
                         , viewTurn, inModel, axisAtBottom )

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
data ModelShape = FlatShape | GlobeShape Globe | HyperbolicShape HyperbolicModel HyperbolicDisplay Isometry
  deriving Eq

-- | The globe's orientation and zoom, when the viewer shows the sphere.
modelGlobe :: Model -> Maybe Globe
modelGlobe m = case modelShape m of { GlobeShape g -> Just g; _ -> Nothing }

-- | The view of the hyperbolic plane, when the viewer shows it.
modelIsometry :: Model -> Maybe Isometry
modelIsometry m = case modelShape m of { HyperbolicShape _ _ v -> Just v; _ -> Nothing }

-- | Whether the screen plane is the model's (on the plane).
modelIsFlat :: Model -> Bool
modelIsFlat m = modelShape m == FlatShape

-- | A globe's view fitted to a window (the globe fills it: see
-- 'globeScreenPixelSize'); any other view as it is.
fitGlobe :: (Int, Int) -> Model -> Model
fitGlobe wh m = case modelShape m of
  GlobeShape _ -> let px = globeScreenPixelSize wh in m { modelCenter = (0, 0), modelPixelDim = (px, px) }
  _ -> m

-- | A view, as "Jump to view" shows it and sets it, whatever the kind of
-- viewer: the point in the middle (on the sphere, the point the globe
-- faces, which can be ∞: infinite), how far the picture is turned about
-- it (in degrees, counterclockwise; not on the plane), and the size of a
-- pixel there (on the sphere, its size on the sphere; on the plane, a
-- pixel's width).
data ViewSettings = ViewSettings
  { vsCenter :: (Double, Double)
  , vsTurn :: Maybe Double
  , vsPixelSize :: Double
  } deriving (Eq, Show)

-- | A view's settings, in a window of the given size.
viewSettings :: (Int, Int) -> Model -> ViewSettings
viewSettings (_, h) m = case modelShape m of
  FlatShape -> ViewSettings (modelCenter m) Nothing (fst (modelPixelDim m))
  GlobeShape g -> ViewSettings (globeCenter g) (Just (negate (degrees (rollOf (globeRotation g)))))
                               (globeZoom g * fst (modelPixelDim m))
  HyperbolicShape hm display v ->
    let s = middleOf display
    in ViewSettings (maybe (0, 0) id (screenToModelH hm display v s)) (Just (degrees (viewTurn hm display s v)))
                    (fst (modelPixelDim m))
  where
    middleOf display = case display of
      DiskDisplay -> (0, 0)
      HalfPlaneDisplay -> axisAtBottom h (fst (modelPixelDim m)) (0, 0)

-- | The view with some settings (in a window of the given size), keeping
-- what they don't say (a real viewer's aspect ratio), or why there's no
-- such view.
settingsView :: (Int, Int) -> Model -> ViewSettings -> Either String Model
settingsView wh@(_, h) m (ViewSettings c turn px)
  | not (px > 0) = Left "The pixel size should be positive."
  | otherwise = case modelShape m of
      FlatShape
        | isInfinite (fst c) || isInfinite (snd c) -> Left "The plane has no point at ∞."
        | otherwise -> let (opx, opy) = modelPixelDim m
                       in Right (Model c (px, px * opy / opx) FlatShape)
      GlobeShape _ ->
        let fitted = fitGlobe wh m
            screenPx = fst (modelPixelDim fitted)
            angle = negate (radians (maybe 0 id turn))
        in Right fitted { modelShape = GlobeShape (globeFacingRolled c angle (clampGlobeZoom (px / screenPx))) }
      HyperbolicShape hm display _
        | not (inModel hm c) -> Left "That point isn't in the hyperbolic plane's model."
        | otherwise ->
            let middle = case display of
                  DiskDisplay -> (0, 0)
                  HalfPlaneDisplay -> axisAtBottom h px (0, 0)
            in Right (Model middle (px, px)
                        (HyperbolicShape hm display (viewShowingTurned hm display c middle (radians (maybe 0 id turn)))))

degrees, radians :: Double -> Double
degrees r = r * 180 / pi
radians d = d * pi / 180

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
