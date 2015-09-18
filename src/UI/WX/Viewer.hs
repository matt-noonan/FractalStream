{- |
Module      : UI.WX.Viewer
Description : Interactive view based on wxWidgets
-}
module UI.WX.Viewer ( wxView
                    ) where

import Lang.Numbers
import Lang.Planar
import Exec.Region
import Color.Color
import Color.Colorize
import UI.Tile

import Graphics.UI.WX
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.Draw

import Foreign.ForeignPtr

-- | Create a window with an interactive view of a complex-dynamical system.
wxView :: (C, C)       -- ^ The upper-left and lower-right corners of the view.
       -> Dynamics C   -- ^ The dynamical system to explore.
       -> Colorizer C  -- ^ How to color the iteration results.
       -> IO ()
wxView (ul, lr) dyn col = start $ do

    let (width, height) = (512, 512)

    -- Create the main viewer frame
    f <- frame [text := "FractalStream", clientSize := sz width height]

    -- Create the menus

    -- File menu
    file <- menuPane      [text := "&File"]
    _    <- menuQuit file [help := "Quit", on command := close f]

    -- Help menu
    hlp   <- menuHelp      []
    about <- menuAbout hlp [help := "About FractalStream"]

    -- Viewer status bar
    status <- statusField   [text := "FractalStream status bar"]

    -- Panel and tile for initial view

    let mRect = rectangle ul lr
    viewerTile <- renderTile dyn col (width, height) mRect
    p <- panel f [on paint := \dc r -> (windowGetViewRect f >>= paintTile viewerTile dc r)]

    -- Add a timer which will check for repainting requests
    _ <- timer f [interval := 20, on command := ifModified viewerTile $ repaint p ]

    -- Add the status bar, menu bar, and layout to the frame
    set f [ statusBar := [status]
          , menuBar   := [file,hlp]
          , layout    := fill $ minsize (sz width height) $ widget p
          , on (menu about) := infoDialog f "About FractalStream" "Contributors:\nMatt Noonan"
          ]

-- | Paint the state of a tile into a device context.
paintTile :: Tile a  -- ^ A tile to convert to an image
          -> DC d    -- ^ The WX device context
          -> Rect    -- ^ The rectangle which needs updating
          -> Rect    -- ^ The enclosing view rectangle
          -> IO ()

paintTile viewerTile dc _ windowRect = do
    
    let (width, height, fptr) = tileData viewerTile

    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect

    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )

    withForeignPtr fptr $ \buf -> do
        pbuf <- pixelBufferCreate (sz width height)

        pixels <- mapM (peekColor buf) [0..(width * height - 1)]
        pixelBufferSetPixels pbuf pixels

        img <- imageCreateFromPixelBuffer pbuf
        drawImage dc img (pt x0 y0) []
