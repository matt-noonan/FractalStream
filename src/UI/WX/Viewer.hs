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

import Foreign.ForeignPtr

wxView :: Dynamics C -> Colorizer C -> IO ()
wxView dyn col = start $ do

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

    let mRect = rectangle (complex (-2) 2) (complex 2 (-2))
    viewerTile <- renderTile dyn col (width, height) mRect
    p <- panel f [on paint := paintTile viewerTile]

    -- Add a timer which will check for repainting requests
    _ <- timer f [interval := 20, on command := ifModified viewerTile $ repaint p ]

    -- Add the status bar, menu bar, and layout to the frame
    set f [ statusBar := [status]
          , menuBar   := [file,hlp]
          , layout    := minsize (sz width height) $ widget p
          , on (menu about) := infoDialog f "About FractalStream" "FractalStream Contributors\nMatt Noonan"
          ]

paintTile :: Tile a -> DC d -> Rect -> IO ()

paintTile viewerTile dc _ = do

    putStrLn "redraw tile!"

    let (width, height, fptr) = tileData viewerTile

    withForeignPtr fptr $ \buf -> do
        pbuf <- pixelBufferCreate (sz width height)

        pixels <- mapM (peekColor buf) [0..(width * height - 1)]
        pixelBufferSetPixels pbuf pixels

        img <- imageCreateFromPixelBuffer pbuf
        drawImage dc img (pt 0 0) []
