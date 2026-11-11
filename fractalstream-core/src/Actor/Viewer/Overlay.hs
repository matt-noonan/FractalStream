{- |
Module       : Actor.Viewer.Overlay
Description  : Rasterize a tool's recorded draw commands onto a rendered viewer image

Composites the draw commands a tool's event handlers accumulate (the
layers 'Actor.Viewer.vDrawCmds' returns) onto a viewer's rendered base
image, using Rasterific (<https://hackage.haskell.org/package/Rasterific>)
for the actual rasterization rather than hand-rolling line\/circle drawing.
Both Rasterific and its JuicyPixels dependency are pure Haskell, adding no
new system-library dependency -- the point of a headless frontend is to
avoid exactly that kind of weight. For the same reason, text is drawn
with a copy of DejaVu Sans (@fonts/@) embedded in the library, rather
than with a font located at run time.
-}
module Actor.Viewer.Overlay
  ( compositeDrawLayers
  , compositeDrawLayersToFile
  ) where

import FractalStream.Prelude

import Language.Draw
import Data.Color (colorToRGB)
import qualified Data.Planar as Planar

import Actor.Viewer.Overlay.Font (overlayFont)
import Actor.Viewer.Sphere (Projection(..), ScreenView(..), projectToScreen, visibleRuns, sampleSegment, sampleCircle)
import Graphics.Text.TrueType (Font, BoundingBox(..), stringBoundingBox)

import Codec.Picture (Image(..), PixelRGBA8(..), writePng, generateImage, pixelAt)
import Codec.Picture.Types (newMutableImage, writePixel, unsafeFreezeImage)
import Graphics.Rasterific
import Graphics.Rasterific.Texture (uniformTexture)

import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)
import Data.Bits (shiftR)

-- | Composite a viewer's rendered base image (an RGB8 buffer, as produced
-- by 'Actor.Viewer.withRenderedViewerBuffer') with its tools' drawn
-- annotations, producing the resulting image in memory -- e.g. so a caller
-- can inspect a pixel directly (as a scenario's @assert@ command does)
-- without a round trip through a file.
--
-- @center@ and @pixelSize@ fix the model-space\/pixel-space mapping; pass
-- the same values used to produce the base render (the center and pixel
-- size 'Actor.Viewer.withRenderedViewerBuffer' computes @vaPoint@ from).
-- @layers@ is one list of draw commands per draw-order layer, as returned
-- by a tool's combined 'Actor.Viewer.vDrawCmds'.
compositeDrawLayers
  :: ScreenView         -- ^ where the view is, and how model points get there
  -> [[DrawCommand]]    -- ^ draw layers, in draw order
  -> (Word32, Word32)   -- ^ buffer dimensions, in pixels
  -> Ptr Word8          -- ^ the rendered base image, as RGB8
  -> IO (Image PixelRGBA8)
compositeDrawLayers ScreenView{ svCenter = center, svPixelSize = pixelSize, svProjection = proj }
                    layers (w32, h32) buf = do
  let w = fromIntegral w32
      h = fromIntegral h32
  baseImage <- bufferToImage w h buf
  let toPx = modelToPixel center pixelSize (w, h)
      image = Box 0 0 (fromIntegral w) (fromIntegral h)
  -- The drawings are rasterized onto a transparent canvas of their own
  -- and then laid over the base image, rather than drawn on top of it
  -- with Rasterific's 'drawImage': that samples the image half a pixel
  -- off, blending each pixel with its neighbors. This way, a pixel no
  -- drawing touches is exactly as rendered. With nothing to draw, the
  -- base image is the result, and Rasterific isn't needed at all.
  pure $ if all null layers then baseImage
    else overImage (renderDrawing w h (PixelRGBA8 0 0 0 0) $ case proj of
           FlatProjection -> drawLayers image toPx pixelSize layers
           GlobeProjection _ -> mapM_ (drawGlobeLayer image proj toPx) layers)
                   baseImage

-- | Lay a (non-premultiplied, as Rasterific makes them) RGBA image over
-- an opaque one of the same size, with Rasterific's own arithmetic for
-- drawing a color of that opacity over a pixel.
overImage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
overImage top bottom = generateImage at (imageWidth bottom) (imageHeight bottom)
  where
    at x y = case (pixelAt top x y, pixelAt bottom x y) of
      (PixelRGBA8 _ _ _ 0, b) -> b
      (PixelRGBA8 r g b 255, _) -> PixelRGBA8 r g b 255
      (PixelRGBA8 r g b a, PixelRGBA8 br bg bb _) ->
        PixelRGBA8 (mix a r br) (mix a g bg) (mix a b bb) 255
    mix :: Word8 -> Word8 -> Word8 -> Word8
    mix a c d =
      let v = fromIntegral a * fromIntegral c + fromIntegral d * (255 - fromIntegral a) + 128 :: Word32
      in fromIntegral ((v + (v `shiftR` 8)) `shiftR` 8)

-- | 'compositeDrawLayers', written out to a PNG file.
compositeDrawLayersToFile
  :: ScreenView -> [[DrawCommand]] -> (Word32, Word32) -> Ptr Word8
  -> FilePath
  -> IO ()
compositeDrawLayersToFile view layers dims buf path =
  writePng path =<< compositeDrawLayers view layers dims buf

-- | Copy an RGB8 'Ptr Word8' buffer into a JuicyPixels 'Image' (as opaque
-- RGBA8 -- Rasterific's 'renderDrawing' needs a 'PackeablePixel' instance
-- for its canvas pixel type, which plain RGB8 doesn't have but RGBA8 does).
bufferToImage :: Int -> Int -> Ptr Word8 -> IO (Image PixelRGBA8)
bufferToImage w h buf = do
  mimg <- newMutableImage w h
  forM_ [0 .. h - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
    let i = 3 * (y * w + x)
    r <- peekElemOff buf i
    g <- peekElemOff buf (i + 1)
    b <- peekElemOff buf (i + 2)
    writePixel mimg x y (PixelRGBA8 r g b 255)
  unsafeFreezeImage mimg

-- | The same model-space\/pixel-space mapping 'Actor.Viewer.withRenderedViewerBuffer'
-- uses (center +\/- half the view size, at one pixel per @pixelSize@), via
-- 'Data.Planar's generic affine rectangle conversion -- the same machinery
-- the interactive wx viewer uses to place its own tool overlays
-- (@UI.ProjectViewer.modelToView@), so this can't independently drift from
-- how the live app draws the same annotations.
modelToPixel :: (Double, Double) -> (Double, Double) -> (Int, Int) -> (Double, Double) -> Px
modelToPixel (cx, cy) (pxW, pxH) (w, h) pt =
  let halfW = pxW * fromIntegral w / 2
      halfH = pxH * fromIntegral h / 2
      modelRect :: Planar.Rectangle (Double, Double)
      modelRect = Planar.flippedRectangle (cx - halfW, cy - halfH) (cx + halfW, cy + halfH)
      pixelRect :: Planar.Rectangle (Double, Double)
      pixelRect = Planar.rectangle (0, 0) (fromIntegral w, fromIntegral h)
  in Planar.convertRect modelRect pixelRect pt

-- | Roughly the size of the live app's default GUI font: 13 pixels.
textSize :: PointSize
textSize = PointSize 10

textDpi :: Int
textDpi = 96

-- | Draw text centered on a point, like @UI.ProjectViewer@ does: centered
-- horizontally on the string's own width, and vertically on the font's
-- line height (here, the extent of a tall-and-deep reference string), so
-- that a string's position doesn't depend on which letters it contains.
drawCenteredText :: Font -> Point -> String -> Drawing PixelRGBA8 ()
drawCenteredText font (V2 x y) txt =
  let width = _xMax (stringBoundingBox font textDpi textSize txt)
      lineBox = stringBoundingBox font textDpi textSize "Ág"
      baseline = y + (_yMax lineBox + _yMin lineBox) / 2
  in printTextAt font textSize (V2 (x - width / 2) baseline) txt

------------------------------------------------------------
-- Keeping Rasterific near the image
------------------------------------------------------------

-- Rasterific works in single precision. Far from the image (a line out to
-- an escaping orbit's next point, say), its coordinates get too coarse
-- for it: from about 10^7 pixels away, stroking a line or an outline can
-- run for minutes and take gigabytes, and further out it draws the wrong
-- pixels. So shapes are worked out in double precision first, and one
-- that reaches far from the image is cut down to the part near it before
-- Rasterific sees it. A shape that stays near the image is drawn just as
-- it is.

-- | A point of the image, in pixels (in double precision).
type Px = (Double, Double)

-- | A rectangle of pixel space: left, top, right, bottom.
data Box = Box !Double !Double !Double !Double

grow :: Double -> Box -> Box
grow m (Box x0 y0 x1 y1) = Box (x0 - m) (y0 - m) (x1 + m) (y1 + m)

inBox :: Box -> Px -> Bool
inBox (Box x0 y0 x1 y1) (x, y) = x >= x0 && x <= x1 && y >= y0 && y <= y1

-- | How far outside the image a shape can reach and still be handed to
-- Rasterific as it is.
nearDistance :: Double
nearDistance = 1e4

-- | Whether points are near enough to the image to draw as they are.
near :: Box -> [Px] -> Bool
near image = all (inBox (grow nearDistance image))

-- | Where a shape is cut off: far enough outside the image that a 2-pixel
-- stroke (and its round caps, and its antialiasing) along the cut doesn't
-- show.
cutBox :: Box -> Box
cutBox = grow 4

finitePx :: Px -> Bool
finitePx (x, y) = not (isNaN x || isInfinite x || isNaN y || isInfinite y)

toPoint :: Px -> Point
toPoint (x, y) = V2 (realToFrac x) (realToFrac y)

-- | The part of a segment in a box, if any (by Cohen and Sutherland's
-- method). An end outside the box is moved onto the edge it's past, worked
-- out from the segment's other end: so a segment from a point in the box
-- out to a very distant one keeps its direction, and one along a row or
-- column of pixels stays on it, however far away its ends are.
clipSegment :: Box -> Px -> Px -> Maybe (Px, Px)
clipSegment (Box x0 y0 x1 y1) = go (8 :: Int)
  where
    outside (x, y) = (x < x0, x > x1, y < y0, y > y1)
    none (a, b, c, d) = not (a || b || c || d)
    shared (a, b, c, d) (a', b', c', d') = (a && a') || (b && b') || (c && c') || (d && d')
    go n p q
      | none op && none oq = Just (p, q)
      | shared op oq || n == 0 = Nothing
      | not (none op) = go (n - 1) (onEdge p q) q
      | otherwise = go (n - 1) p (onEdge q p)
      where
        op = outside p
        oq = outside q
    -- (p is outside the box, and q isn't past the same edge)
    onEdge (px, py) (qx, qy)
      | px < x0 = (x0, alongX x0)
      | px > x1 = (x1, alongX x1)
      | py < y0 = (alongY y0, y0)
      | otherwise = (alongY y1, y1)
      where
        alongX c = qy + (c - qx) * (py - qy) / (px - qx)
        alongY c = qx + (c - qy) * (px - qx) / (py - qy)

-- | The parts of a polyline in a box, each a polyline of its own.
clipPolyline :: Box -> [Px] -> [[Px]]
clipPolyline box pts =
  filter ((>= 2) . length) (map reverse (go [] (zip pts (drop 1 pts))))
  where
    -- (the run being built, backwards, then the finished ones)
    go run [] = [run]
    go run ((p, q) : rest) = case clipSegment box p q of
      Nothing -> run : go [] rest
      Just (p', q')
        | p' == p, (end : _) <- run, end == p -> go (q' : run) rest
        | otherwise -> run : go [q', p'] rest

-- | The part of a polygon in a box (by Sutherland and Hodgman's method).
clipPolygon :: Box -> [Px] -> [Px]
clipPolygon (Box x0 y0 x1 y1) =
    against (\(x, _) -> x >= x0) (atX x0)
  . against (\(x, _) -> x <= x1) (atX x1)
  . against (\(_, y) -> y >= y0) (atY y0)
  . against (\(_, y) -> y <= y1) (atY y1)
  where
    against inside cross poly = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
      where
        edge a b
          | inside b = [ cross a b | not (inside a) ] ++ [b]
          | inside a = [cross a b]
          | otherwise = []
    atX c (ax, ay) (bx, by) = (c, ay + (c - ax) * (by - ay) / (bx - ax))
    atY c (ax, ay) (bx, by) = (ax + (c - ay) * (bx - ax) / (by - ay), c)

-- | An ellipse (center, and radii in pixels) far from the image, or too
-- big for it: what of it to draw in a box. 'Nothing' if it can't reach
-- the box; @Just []@ if the box is all inside it; otherwise, points along
-- the part of it facing the box (all of it, if its center is in the box),
-- close enough together (within a tenth of a pixel of the curve) to join
-- up. Cutting those down to the box is left to 'clipPolyline' and
-- 'clipPolygon'.
ellipseNear :: Box -> Px -> Double -> Double -> Maybe [Px]
ellipseNear box@(Box x0 y0 x1 y1) (cx, cy) rx ry
  | x1 < cx - rx || x0 > cx + rx || y1 < cy - ry || y0 > cy + ry = Nothing
  | all inside corners = Just []
  | inBox box (cx, cy) = Just (arcFrom 0 (2 * pi))
  | otherwise = Just (arcFrom lo hi)
  where
    corners = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    unit (x, y) = ((x - cx) / rx, (y - cy) / ry)
    inside p = let (u, v) = unit p in u * u + v * v <= 1
    -- The box doesn't hold the center, so, seen from the center, it lies
    -- in a sector less than a half-turn wide: the sector its corners span.
    mid = let (u, v) = unit ((x0 + x1) / 2, (y0 + y1) / 2) in atan2 v u
    turn a = a - 2 * pi * fromIntegral (round (a / (2 * pi)) :: Int)
    offsets = [ turn (atan2 v u - mid) | c <- corners, let (u, v) = unit c ]
    (lo, hi) = (mid + minimum offsets, mid + maximum offsets)
    arcFrom a b =
      let n = min 100000 (ceiling ((b - a) / sqrt (0.8 / max rx ry)) + 1) :: Int
      in [ (cx + rx * cos t, cy + ry * sin t)
         | k <- [0 .. n], let t = a + (b - a) * fromIntegral k / fromIntegral n ]

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

strokeWith :: Geometry g => PixelRGBA8 -> g -> Drawing PixelRGBA8 ()
strokeWith color = withTexture (uniformTexture color) . stroke 2 JoinRound (CapRound, CapRound)

fillWith :: Geometry g => PixelRGBA8 -> g -> Drawing PixelRGBA8 ()
fillWith color = withTexture (uniformTexture color) . fill

-- | Stroke a polyline: as it is near the image, or else the parts of it
-- near the image.
strokePolyline :: Box -> PixelRGBA8 -> [Px] -> Drawing PixelRGBA8 ()
strokePolyline image color pts
  | not (all finitePx pts) = pure ()
  | near image pts = strokeWith color (polyline (map toPoint pts))
  | otherwise = forM_ (clipPolyline (cutBox image) pts) (strokeWith color . polyline . map toPoint)

-- | Fill a polygon: as it is near the image, or else the part of it near
-- the image.
fillPolygon :: Box -> PixelRGBA8 -> [Px] -> Drawing PixelRGBA8 ()
fillPolygon image color pts
  | not (all finitePx pts) = pure ()
  | near image pts = fillWith color (polygon (map toPoint pts))
  | otherwise = case clipPolygon (cutBox image) pts of
      clipped@(_ : _ : _ : _) -> fillWith color (polygon (map toPoint clipped))
      _ -> pure ()

-- | Draw something centered on a point, if the point is near the image.
nearPoint :: Box -> Px -> (Point -> Drawing PixelRGBA8 ()) -> Drawing PixelRGBA8 ()
nearPoint image p draw = when (finitePx p && near image [p]) (draw (toPoint p))

-- | Draw a layer's commands on the globe: each shape is projected onto
-- the sphere (a straight line or a circle of the plane is sampled, as
-- it's curved there), and only what's on the visible side is drawn.
drawGlobeLayer :: Box -> Projection -> ((Double, Double) -> Px) -> [DrawCommand] -> Drawing PixelRGBA8 ()
drawGlobeLayer image proj toPx = go white white
  where
    white = PixelRGBA8 255 255 255 255
    toPixelColor c = let (r, g, b) = colorToRGB c in PixelRGBA8 r g b 255
    at = fmap toPx . projectToScreen proj
    runs = map (map toPx) . visibleRuns proj
    strokeRuns color = mapM_ (strokePolyline image color)
    -- a closed outline: filled if it's all visible, else its visible runs
    outline filled strokeColor fillColor pts = case runs pts of
      [r] | filled, length r == length pts -> fillPolygon image fillColor r
      rs -> strokeRuns (if filled then fillColor else strokeColor) rs

    go :: PixelRGBA8 -> PixelRGBA8 -> [DrawCommand] -> Drawing PixelRGBA8 ()
    go _ _ [] = pure ()
    go strokeColor fillColor (cmd : cmds) = case cmd of
      SetStroke _ c -> go (toPixelColor c) fillColor cmds
      SetFill   _ c -> go strokeColor (toPixelColor c) cmds
      Clear _ -> go strokeColor fillColor cmds
      DrawPoint _ pt -> do
        forM_ (at pt) $ \p -> nearPoint image p $ \p' -> fillWith strokeColor (circle p' 2)
        go strokeColor fillColor cmds
      DrawLine _ pt1 pt2 -> do
        strokeRuns strokeColor (runs (sampleSegment 64 pt1 pt2))
        go strokeColor fillColor cmds
      DrawCircle _ filled r pt -> do
        outline filled strokeColor fillColor (sampleCircle 96 pt r)
        go strokeColor fillColor cmds
      DrawRect _ filled (x1, y1) (x2, y2) -> do
        let corners = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]
            edges = concat (zipWith (\a b -> init (sampleSegment 32 a b)) corners (drop 1 corners)) ++ [(x1, y1)]
        outline filled strokeColor fillColor edges
        go strokeColor fillColor cmds
      Write _ txt pt -> do
        forM_ overlayFont $ \font -> forM_ (at pt) $ \p -> nearPoint image p $ \p' ->
          withTexture (uniformTexture strokeColor) (drawCenteredText font p' txt)
        go strokeColor fillColor cmds

-- | Draw every layer's commands, in order. Each layer starts with the pen
-- and fill reset to white, matching @UI.ProjectViewer.paintToolLayer@.
drawLayers :: Box -> ((Double, Double) -> Px) -> (Double, Double) -> [[DrawCommand]] -> Drawing PixelRGBA8 ()
drawLayers image toPx pixelSize = mapM_ (drawLayer image toPx pixelSize)

drawLayer :: Box -> ((Double, Double) -> Px) -> (Double, Double) -> [DrawCommand] -> Drawing PixelRGBA8 ()
drawLayer image toPx (pxW, pxH) = go white white
  where
    white = PixelRGBA8 255 255 255 255

    toPixelColor c = let (r, g, b) = colorToRGB c in PixelRGBA8 r g b 255

    go :: PixelRGBA8 -> PixelRGBA8 -> [DrawCommand] -> Drawing PixelRGBA8 ()
    go _ _ [] = pure ()
    go strokeColor fillColor (cmd : cmds) = case cmd of
      SetStroke _ c -> go (toPixelColor c) fillColor cmds
      SetFill   _ c -> go strokeColor (toPixelColor c) cmds
      -- The base image is already cleared; there's no overlay state to
      -- reset here.
      Clear _ -> go strokeColor fillColor cmds

      -- Rasterific throws an arithmetic exception on a NaN or infinite
      -- coordinate (which a script can easily produce, e.g. from an
      -- escaping orbit), so shapes at such points are skipped.
      -- Points are drawn in the stroke color, like the live app does.
      DrawPoint _ pt -> do
        nearPoint image (toPx pt) $ \p -> fillWith strokeColor (circle p 2)
        go strokeColor fillColor cmds

      DrawLine _ pt1 pt2 -> do
        let (p, q) = (toPx pt1, toPx pt2)
        when (finitePx p && finitePx q) $
          if near image [p, q]
            then strokeWith strokeColor (line (toPoint p) (toPoint q))
            else forM_ (clipSegment (cutBox image) p q) $ \(p', q') ->
                   strokeWith strokeColor (line (toPoint p') (toPoint q'))
        go strokeColor fillColor cmds

      DrawCircle _ filled r pt -> do
        -- A circle in model space: an ellipse, when pixels aren't square.
        let c@(cx, cy) = toPx pt
            rx = r / pxW
            ry = r / pxH
            color = if filled then fillColor else strokeColor
        when (finitePx c && finitePx (rx, ry)) $
          if near image [(cx - rx, cy - ry), (cx + rx, cy + ry)]
            then let prim = ellipse (toPoint c) (realToFrac rx) (realToFrac ry)
                 in if filled then fillWith color prim else strokeWith color prim
            else case ellipseNear (cutBox image) c (abs rx) (abs ry) of
              Nothing -> pure ()
              -- (the image is all inside it)
              Just [] -> when filled $ fillPolygon image color (boxCorners (cutBox image))
              Just arc
                | filled -> fillPolygon image color (arc ++ [c])
                | otherwise -> strokePolyline image color arc
        go strokeColor fillColor cmds

      DrawRect _ filled pt1 pt2 -> do
        let (x1, y1) = toPx pt1
            (x2, y2) = toPx pt2
        when (finitePx (x1, y1) && finitePx (x2, y2)) $
          if near image [(x1, y1), (x2, y2)]
            then
              let V2 fx1 fy1 = toPoint (x1, y1)
                  V2 fx2 fy2 = toPoint (x2, y2)
                  prim = rectangle (V2 (min fx1 fx2) (min fy1 fy2)) (abs (fx2 - fx1)) (abs (fy2 - fy1))
              in if filled then fillWith fillColor prim else strokeWith strokeColor prim
            else
              let corners = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
              in if filled then fillPolygon image fillColor corners
                           else strokePolyline image strokeColor (corners ++ take 1 corners)
        go strokeColor fillColor cmds

      -- Text is drawn in the stroke color, like the live app's textColor.
      Write _ txt pt -> do
        forM_ overlayFont $ \font -> nearPoint image (toPx pt) $ \p ->
          withTexture (uniformTexture strokeColor) (drawCenteredText font p txt)
        go strokeColor fillColor cmds

boxCorners :: Box -> [Px]
boxCorners (Box x0 y0 x1 y1) = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
