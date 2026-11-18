{-# LANGUAGE CPP #-}
-- | The mouse pointers over a viewer, as the web app's: an open hand in
-- Navigate (a closed one while dragging), and crosshairs while a tool is
-- in use.
--
-- macOS has open and closed hands of its own; wxWidgets has none on
-- Windows or Linux (it gives the pointing finger instead), so there
-- they're drawn here, from the pictures below.
module UI.Cursors
  ( ViewerCursors(..)
  , newViewerCursors
  ) where

import Graphics.UI.WXCore
import System.Environment (lookupEnv)

-- | The pointers a viewer shows.
data ViewerCursors = ViewerCursors
  { vcOpenHand   :: Cursor ()
  , vcClosedHand :: Cursor ()
  , vcCrosshair  :: Cursor ()
  }

-- | (With FS_DRAWN_CURSORS=1, the drawn hands on macOS too: to see them.)
newViewerCursors :: IO ViewerCursors
newViewerCursors = do
  drawn <- maybe False (not . null) <$> lookupEnv "FS_DRAWN_CURSORS"
  (open, closed) <- if drawn || not systemHands
    then (,) <$> drawnCursor openHand <*> drawnCursor closedHand
    -- (wxCURSOR_OPEN_HAND and wxCURSOR_CLOSED_HAND: wx's on macOS only,
    -- so wxHaskell doesn't name them)
    else (,) <$> cursorCreateFromStock 29 <*> cursorCreateFromStock 30
  cross <- cursorCreateFromStock wxCURSOR_CROSS
  pure ViewerCursors { vcOpenHand = open, vcClosedHand = closed, vcCrosshair = cross }

-- | Whether the system has open and closed hands of its own.
systemHands :: Bool
#if defined(darwin_HOST_OS)
systemHands = True
#else
systemHands = False
#endif

-- | A cursor from a picture (16 x 16: @#@ black, @.@ white, a space
-- clear), at twice its size, its hot spot in the middle.
drawnCursor :: [String] -> IO (Cursor ())
drawnCursor rows = do
  let color :: Int -> Int -> Int -> Color
      color = rgb
      clear = color 255 0 255
      colorOf = \case
        '#' -> color 0 0 0
        '.' -> color 255 255 255
        _ -> clear
      n = length rows
  img <- imageCreateFromPixels (sz n n) (map colorOf (concat rows))
  imageSetMaskColour img clear
  big <- imageScale img (sz (2 * n) (2 * n))
  imageSetOptionInt big "HotSpotX" n
  imageSetOptionInt big "HotSpotY" n
  cursorCreateFromImage big

-- (rectangles, filled, with every pixel next to them outlined)
openHand, closedHand :: [String]
openHand =
  [ "     ####       "
  , "     #..####    "
  , "  ####..#..#    "
  , "  #..#..#..#### "
  , "  #..#..#..#..# "
  , "  #..#..#..#..# "
  , "###..#..#..#..# "
  , "#....#..#..#..# "
  , "#.............# "
  , "#.............# "
  , "##............# "
  , " ##...........# "
  , "  #...........# "
  , "  ##.........## "
  , "   ##.......##  "
  , "    #########   " ]
closedHand =
  [ "                "
  , "                "
  , "                "
  , "                "
  , "  ############# "
  , "  #..#..#..#..# "
  , "  #...........# "
  , "###...........# "
  , "#.............# "
  , "#.............# "
  , "#.............# "
  , "##............# "
  , " ##...........# "
  , "  ##.........## "
  , "   ##.......##  "
  , "    #########   " ]
