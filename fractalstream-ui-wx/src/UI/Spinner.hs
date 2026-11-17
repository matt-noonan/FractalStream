{- |
Module      : UI.Spinner
Description : A turning ring, for saying that something's under way

The spinner the "Opening a project" and "Running a tool" windows show: a
faint ring with an arc of it in an accent blue, turning about once a
second, as the web app's does (@.spinner-ring@ in @web/style.css@, whose
@--line@ and @--accent@ colors these are, light and dark).
It's drawn rather than native, so that it's the same on every platform:
wxWidgets' own (@wxActivityIndicator@) isn't in wxHaskell, and couldn't be
added on Windows, where wxWidgets is a prebuilt DLL.
-}
module UI.Spinner
  ( spinnerRing
  ) where

import Graphics.UI.WX hiding (when, Color, Layout, finally)
import Graphics.UI.WXCore (gcdcCreate, gcdcDelete, windowSetMinSize)
import Control.Exception (finally)
import UI.CodeEditor (frameIsLight)
import Data.IORef

-- | A spinner in a window, turning until the window's destroyed.
spinnerRing :: Window a -> IO (Window ())
spinnerRing owner = do
  let side = 18 :: Int
  w <- window owner [ clientSize := sz side side ]
  -- (its size in a layout: a plain window has none of its own)
  windowSetMinSize w (sz side side)
  -- (painted over the window's own background: its parent's)
  get owner bgcolor >>= \bg -> set w [ bgcolor := bg ]
  light <- frameIsLight owner
  angle <- newIORef (90 :: Double)
  let accent = if light then rgb 0x2f 0x6f (0xd6 :: Int) else rgb 0x6e 0xa2 (0xff :: Int)
      track = if light then rgb 0xd9 0xd9 (0xd4 :: Int) else rgb 0x3a 0x3a (0x38 :: Int)
      ring = rect (pt 2 2) (sz (side - 4) (side - 4))
  set w [ on paintRaw := \paintDC _ _ -> do
            dcClear paintDC
            -- (through a graphics context, for smooth edges)
            dc <- gcdcCreate (objectCast paintDC)
            flip finally (gcdcDelete dc) $ do
              a <- readIORef angle
              ellipse dc ring [ pen := penColored track 2, brush := brushTransparent ]
              ellipticArc dc ring (a - 100) a [ pen := penColored accent 2, brush := brushTransparent ] ]
  -- (clockwise, a turn in 0.9 s; the timer goes with the window)
  ticker <- timer w [ interval := 30 ]
  set ticker [ on command := modifyIORef' angle (\a -> let a' = a - 12 in if a' < -360 then a' + 360 else a')
                             >> repaint w ]
  pure w
