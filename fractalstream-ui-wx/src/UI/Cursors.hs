{-# LANGUAGE CPP #-}
-- | The mouse pointers over a viewer, as the web app's: an open hand in
-- Navigate (a closed one while dragging), crosshairs while a tool is in
-- use, and the arrow over the viewer's buttons.
--
-- macOS has open and closed hands of its own; wxWidgets has none on
-- Windows or Linux (it gives the pointing finger instead), so there
-- they're drawn here: shapes (fingers, palm, thumb, wrist), filled white
-- and outlined in black, each pixel's color and alpha from 4 x 4 samples,
-- so the edges are smooth. (wxHaskell can't set an image's alpha, so the
-- picture goes to wx as a PNG, by way of a temporary file.)
module UI.Cursors
  ( ViewerCursors(..)
  , newViewerCursors
  ) where

import Codec.Picture (PixelRGBA8(..), generateImage, writePng)
import Control.Exception (SomeException, try)
import Graphics.UI.WXCore
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | The pointers a viewer shows.
data ViewerCursors = ViewerCursors
  { vcOpenHand   :: Cursor ()
  , vcClosedHand :: Cursor ()
  , vcCrosshair  :: Cursor ()
  , vcArrow      :: Cursor ()
  }

-- | (With FS_DRAWN_CURSORS=1, the drawn hands on macOS too: to see them.)
newViewerCursors :: IO ViewerCursors
newViewerCursors = do
  drawn <- maybe False (not . null) <$> lookupEnv "FS_DRAWN_CURSORS"
  (open, closed) <- if drawn || not systemHands
    then (,) <$> drawnCursor "open" openHand <*> drawnCursor "closed" closedHand
    -- (wxCURSOR_OPEN_HAND and wxCURSOR_CLOSED_HAND: wx's on macOS only,
    -- so wxHaskell doesn't name them)
    else (,) <$> cursorCreateFromStock 29 <*> cursorCreateFromStock 30
  cross <- cursorCreateFromStock wxCURSOR_CROSS
  arrow <- cursorCreateFromStock wxCURSOR_ARROW
  pure ViewerCursors { vcOpenHand = open, vcClosedHand = closed, vcCrosshair = cross, vcArrow = arrow }

-- | Whether the system has open and closed hands of its own.
systemHands :: Bool
#if defined(darwin_HOST_OS)
systemHands = True
#else
systemHands = False
#endif

-- | A hand: its shapes, in a 16 x 16 square (y down), each a signed
-- distance (negative inside).
type Shape = Double -> Double -> Double

-- | The cursor of a hand, 32 pixels square, its hot spot in the middle
-- (or wx's crosshair, if the picture can't be made).
drawnCursor :: String -> [Shape] -> IO (Cursor ())
drawnCursor name shapes = do
  r <- try $ do
    tmp <- getTemporaryDirectory
    let path = tmp </> ("fractalstream-" ++ name ++ "-hand.png")
    writePng path (generateImage (pixel shapes) size size)
    img <- imageCreateFromFile path
    removeFile path
    imageSetOptionInt img "HotSpotX" (size `div` 2)
    imageSetOptionInt img "HotSpotY" (size `div` 2)
    cursorCreateFromImage img
  either (\(_ :: SomeException) -> cursorCreateFromStock wxCURSOR_CROSS) pure r
  where size = 32

-- | A pixel of a hand: white inside, black within the outline's width
-- outside, clear beyond; from 4 x 4 samples.
pixel :: [Shape] -> Int -> Int -> PixelRGBA8
pixel shapes i j =
  let ss = 4 :: Int
      scale = 16 / 32
      samples = [ distance ((fromIntegral i + (fromIntegral si + 0.5) / fromIntegral ss) * scale)
                           ((fromIntegral j + (fromIntegral sj + 0.5) / fromIntegral ss) * scale)
                | sj <- [0 .. ss - 1], si <- [0 .. ss - 1] ]
      distance x y = minimum [ s x y | s <- shapes ]
      n = fromIntegral (ss * ss) :: Double
      inside = fromIntegral (length (filter (< 0) samples)) / n
      covered = fromIntegral (length (filter (< outline) samples)) / n
      -- (the color, unpremultiplied: the inside's share of what's covered)
      v = if covered > 0 then inside / covered else 0
      byte x = fromIntegral (round (255 * x) :: Int)
  in PixelRGBA8 (byte v) (byte v) (byte v) (byte covered)
  where outline = 0.85

-- | A segment from (ax, ay) to (bx, by), r around it.
capsule :: Double -> Double -> Double -> Double -> Double -> Shape
capsule ax ay bx by r x y =
  let (dx, dy) = (bx - ax, by - ay)
      t = max 0 (min 1 (((x - ax) * dx + (y - ay) * dy) / max 1e-9 (dx * dx + dy * dy)))
  in sqrt ((x - ax - t * dx) ^ (2 :: Int) + (y - ay - t * dy) ^ (2 :: Int)) - r

-- | A rectangle from (x0, y0) to (x1, y1), its corners rounded by r.
roundedRect :: Double -> Double -> Double -> Double -> Double -> Shape
roundedRect x0 y0 x1 y1 r x y =
  let (cx, cy) = ((x0 + x1) / 2, (y0 + y1) / 2)
      (hx, hy) = ((x1 - x0) / 2 - r, (y1 - y0) / 2 - r)
      (qx, qy) = (abs (x - cx) - hx, abs (y - cy) - hy)
  in sqrt (max qx 0 ^ (2 :: Int) + max qy 0 ^ (2 :: Int)) + min (max qx qy) 0 - r

-- (four fingers, then the thumb, the palm and the wrist; curled, the
-- fingers are just their knuckles)
openHand, closedHand :: [Shape]
openHand =
  [ capsule 4.3 4.0 4.3 9 1.0, capsule 7.0 2.4 7.0 9 1.0, capsule 9.7 3.0 9.7 9 1.0
  , capsule 12.3 5.0 12.3 9.5 0.95, capsule 2.0 7.6 4.8 11.6 1.2
  , roundedRect 3.2 8.0 13.4 13.6 2.2, roundedRect 4.6 11.0 11.8 15.0 1.0 ]
closedHand =
  [ capsule 4.3 6.6 4.3 9 1.0, capsule 7.0 6.2 7.0 9 1.0, capsule 9.7 6.4 9.7 9 1.0
  , capsule 12.3 7.0 12.3 9.5 0.95, capsule 2.6 9.4 5.2 11.8 1.2
  , roundedRect 3.2 7.4 13.4 13.6 2.2, roundedRect 4.6 11.0 11.8 15.0 1.0 ]
