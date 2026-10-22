{- |
Module       : Actor.Viewer.Overlay
Description  : Rasterize a tool's recorded draw commands onto a rendered viewer image

Composites the draw commands a tool's event handlers accumulate (the
layers 'Actor.Viewer.vDrawCmds' returns) onto a viewer's rendered base
image, using Rasterific (<https://hackage.haskell.org/package/Rasterific>)
for the actual rasterization rather than hand-rolling line\/circle drawing.
Both Rasterific and its JuicyPixels dependency are pure Haskell, adding no
new system-library dependency -- the point of a headless frontend is to
avoid exactly that kind of weight.
-}
module Actor.Viewer.Overlay
  ( compositeDrawLayers
  , compositeDrawLayersToFile
  ) where

import FractalStream.Prelude

import Language.Draw
import Data.Color (colorToRGB)
import qualified Data.Planar as Planar

import Codec.Picture (Image, PixelRGBA8(..), writePng)
import Codec.Picture.Types (newMutableImage, writePixel, unsafeFreezeImage)
import Graphics.Rasterific
import Graphics.Rasterific.Texture (uniformTexture)

import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)

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
  :: (Double, Double)  -- ^ model-space view center
  -> Double             -- ^ model-space size of one pixel
  -> [[DrawCommand]]    -- ^ draw layers, in draw order
  -> (Word32, Word32)   -- ^ buffer dimensions, in pixels
  -> Ptr Word8          -- ^ the rendered base image, as RGB8
  -> IO (Image PixelRGBA8)
compositeDrawLayers center pixelSize layers (w32, h32) buf = do
  let w = fromIntegral w32
      h = fromIntegral h32
  baseImage <- bufferToImage w h buf
  let toPixel = modelToPixel center pixelSize (w, h)
  pure $ renderDrawing w h (PixelRGBA8 0 0 0 255) $ do
    drawImage baseImage 0 (V2 0 0)
    drawLayers toPixel pixelSize layers

-- | 'compositeDrawLayers', written out to a PNG file.
compositeDrawLayersToFile
  :: (Double, Double) -> Double -> [[DrawCommand]] -> (Word32, Word32) -> Ptr Word8
  -> FilePath
  -> IO ()
compositeDrawLayersToFile center pixelSize layers dims buf path =
  writePng path =<< compositeDrawLayers center pixelSize layers dims buf

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
modelToPixel :: (Double, Double) -> Double -> (Int, Int) -> (Double, Double) -> Point
modelToPixel (cx, cy) pixelSize (w, h) pt =
  let halfW = pixelSize * fromIntegral w / 2
      halfH = pixelSize * fromIntegral h / 2
      modelRect :: Planar.Rectangle (Double, Double)
      modelRect = Planar.flippedRectangle (cx - halfW, cy - halfH) (cx + halfW, cy + halfH)
      pixelRect :: Planar.Rectangle (Double, Double)
      pixelRect = Planar.rectangle (0, 0) (fromIntegral w, fromIntegral h)
      (px, py) = Planar.convertRect modelRect pixelRect pt
  in V2 (realToFrac px) (realToFrac py)

-- | Draw every layer's commands, in order. Each layer starts with the pen
-- and fill reset to white, matching @UI.ProjectViewer.paintToolLayer@.
drawLayers :: ((Double, Double) -> Point) -> Double -> [[DrawCommand]] -> Drawing PixelRGBA8 ()
drawLayers toPixel pixelSize = mapM_ (drawLayer toPixel pixelSize)

drawLayer :: ((Double, Double) -> Point) -> Double -> [DrawCommand] -> Drawing PixelRGBA8 ()
drawLayer toPixel pixelSize = go white white
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

      DrawPoint _ pt -> do
        withTexture (uniformTexture fillColor) $
          fill (circle (toPixel pt) 2)
        go strokeColor fillColor cmds

      DrawLine _ pt1 pt2 -> do
        withTexture (uniformTexture strokeColor) $
          stroke 2 JoinRound (CapRound, CapRound) (line (toPixel pt1) (toPixel pt2))
        go strokeColor fillColor cmds

      DrawCircle _ filled r pt -> do
        let rpx = realToFrac (r / pixelSize)
            prim = circle (toPixel pt) rpx
        if filled
          then withTexture (uniformTexture fillColor) (fill prim)
          else withTexture (uniformTexture strokeColor) (stroke 2 JoinRound (CapRound, CapRound) prim)
        go strokeColor fillColor cmds

      DrawRect _ filled pt1 pt2 -> do
        let V2 x1 y1 = toPixel pt1
            V2 x2 y2 = toPixel pt2
            topLeft  = V2 (min x1 x2) (min y1 y2)
            rectW    = abs (x2 - x1)
            rectH    = abs (y2 - y1)
            prim = rectangle topLeft rectW rectH
        if filled
          then withTexture (uniformTexture fillColor) (fill prim)
          else withTexture (uniformTexture strokeColor) (stroke 2 JoinRound (CapRound, CapRound) prim)
        go strokeColor fillColor cmds

      -- Text isn't rasterized yet: that needs a font (via FontyFruity or
      -- similar) and bundling/locating a .ttf at runtime, which is a
      -- separate decision from the rasterizer itself. Silently skipped
      -- for now rather than faking it.
      Write _ _ _ -> go strokeColor fillColor cmds
