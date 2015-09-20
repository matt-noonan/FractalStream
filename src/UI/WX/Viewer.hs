{- |
Module      : UI.WX.Viewer
Description : Interactive view based on wxWidgets
-}
module UI.WX.Viewer ( wxView
                    ) where

import Lang.Planar
import Exec.Region
import Color.Color (peekColor, clear)
import Color.Colorize
import UI.Tile

import Graphics.UI.WX
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.Draw
import Graphics.UI.WXCore.WxcTypes (rgba)
import Graphics.UI.WXCore.WxcClassTypes

import Foreign.ForeignPtr

-- | Create a window with an interactive view of a complex-dynamical system.
wxView :: (Planar a, Show a)
       => Rectangle a  -- ^ The upper-left and lower-right corners of the view.
       -> Dynamics a   -- ^ The dynamical system to explore.
       -> Colorizer a  -- ^ How to color the iteration results.
       -> IO ()
wxView modelRect dyn col = start $ do

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

    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]

    -- Panel and tile for initial view
    viewerTile <- renderTile dyn col (width, height) modelRect
    currentTile <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]

    p <- panel f []
    set p [ on paint := \dc r -> do
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (width, height, _) = tileData curTile
                img <- get savedTileImage value
                case img of
                    Nothing -> return ()
                    Just im -> drawCenteredImage im dc viewRect (width, height)
                paintToolLayer lastClick draggedTo dc r viewRect
          ]

    let viewRect = rectangle (Viewport (0,0)) (Viewport (width, height))
    let viewToModel p = convertRect viewRect modelRect $ Viewport (pointX p, pointY p)

    -- Set click and drag event handlers
    set p [ on click   := \pt -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

          , on unclick := \pt -> do

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> return ()
                    Just box -> selectRegion box >> repaint p

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

          , on drag := \pt -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                set status [text := show (viewToModel pt)]

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing -> return ()
                    Just _  -> repaint p

                propagateEvent

          , on motion := \pt -> do
                set status [text := show (viewToModel pt)]
                propagateEvent
          ]

    -- Add a timer which will check for repainting requests
    _ <- timer f [ interval := 5
                 , on command := do
                        curTile <- get currentTile value
                        ifModified curTile $ do
                            viewRect <- windowGetViewRect f
                            tileImage <- generateTileImage curTile viewRect
                            set savedTileImage [value := Just tileImage]
                            repaint p
                 ]

    -- Add the status bar, menu bar, and layout to the frame
    set f [ statusBar := [status]
          , menuBar   := [file,hlp]
          , layout    := fill $ minsize (sz 512 512) $ widget p
          , on resize := resizeWxView >> propagateEvent
          , on (menu about) := infoDialog f "About FractalStream" "Contributors:\nMatt Noonan"
          ]

-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile a  -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())

generateTileImage viewerTile windowRect = do
    let (width, height, fptr) = tileData viewerTile
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )

    withForeignPtr fptr $ \buf -> do
        pbuf <- pixelBufferCreate (sz width height)
        pixels <- mapM (peekColor buf) [0..(width * height - 1)]
        pixelBufferSetPixels pbuf pixels
        imageCreateFromPixelBuffer pbuf

drawCenteredImage :: Image b -> DC d -> Rect -> (Int,Int) -> IO ()

drawCenteredImage img dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawImage dc img (pt x0 y0) []

viewportToPoint :: Viewport -> Point
viewportToPoint (Viewport (x,y)) = Point { pointX = x, pointY = y }

paintToolLayer :: Var (Maybe Viewport)
               -> Var (Maybe Viewport)
               -> DC d
               -> Rect 
               -> Rect
               -> IO ()
paintToolLayer lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba 0 128 255 128) white boxPts

getDragBox :: Var (Maybe Viewport)
           -> Var (Maybe Viewport)
           -> IO (Maybe (Rectangle Viewport))
getDragBox lastClick draggedTo = do
    dragSrc <- get lastClick value
    dragTgt <- get draggedTo value
    return $ case (dragSrc, dragTgt) of
        (Just p1, Just p2) -> Just $ rectangle p1 p2
        _ -> Nothing

drawBox :: DC d
        -> Color
        -> Color
        -> [Point]
        -> IO ()
drawBox dc fillColor lineColor coords =
    polygon dc coords [ brush := brushSolid fillColor
                      , pen := penColored lineColor 2
                      ]

resizeWxView :: IO ()
resizeWxView = return ()

selectRegion :: Rectangle Viewport -> IO ()
selectRegion r = do
    putStrLn $ "selected region " ++ show r
    return ()
