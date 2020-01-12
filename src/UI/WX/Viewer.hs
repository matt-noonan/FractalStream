{-# language RecordWildCards #-}
{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : UI.WX.Viewer
Description : Interactive view based on wxWidgets
-}
module UI.WX.Viewer ( wxView
                    ) where

import           Color.Color                      (peekColor)
import           Color.Colorize
import           Lang.Planar
import           UI.Tile

import           Graphics.UI.WX hiding (pt)
import           qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.Image
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcTypes      (rgba)

import           Foreign.ForeignPtr
import Control.Concurrent

data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  }

modelToRect :: Planar a => (Int,Int) -> Model -> Rectangle a
modelToRect (w,h) Model{..} = flippedRectangle (fromCoords ul) (fromCoords lr)
  where
    ul = (cx - px * fromIntegral w / 2, cy - py * fromIntegral h / 2)
    lr = (cx + px * fromIntegral w / 2, cy + py * fromIntegral h / 2)
    (cx, cy) = modelCenter
    (px, py) = modelPixelDim
    
-- | Create a window with an interactive view of a complex-dynamical system.
wxView :: forall a. (Planar a, Show a)
       => Rectangle a  -- ^ The upper-left and lower-right corners of the view.
       -> ([a] -> IO [Color]) -- ^ The rendering action
       -> IO ()
wxView _modelRect renderAction = start $ do

    model <- variable [value := Model (0,0) (1/128,1/128)]
    
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from wxView, on thread " ++ show tid ++ " " ++ show capInfo ++ " " ++ show bound)
    
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
    pendingResize <- variable [value := False]
    
    -- Panel and tile for initial view
    viewerTile     <- renderTile' renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]

    -- Test dialog
    dlog <- dialog f [ text := "Test dialog"
                     , on resize := propagateEvent ]
    dlog_q <- button dlog [ text := "Say \"Butt\""
                          , on command := putStrLn "butt" ]

    editor <- textCtrl dlog [ font := fontFixed
                            , text := "f(z) = z^2 + C"
                            ]

    set dlog [ layout := column 5 [ margin 5 $ fill $ stretch $ expand $ widget editor
                                  , row 0 [hglue, centre $ widget dlog_q, hglue]
                                  , hstretch $ vspace 5
                                  ]
             , visible := True
             , clientSize := sz 256 512]

    p <- panel f []
    set p [ on paint := \dc r -> do
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (w, h) = tileRect curTile
                get savedTileImage value >>= \case
                    Nothing -> return ()
                    Just im -> drawCenteredImage im dc viewRect (w, h)
                paintToolLayer lastClick draggedTo dc r viewRect
          ]

    let viewToModel pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @a dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))

    -- Set click and drag event handlers
    set p [ on click   := \pt -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

          , on unclick := \pt -> do

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> do
                        -- Completed a click, recenter to the clicked point.
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        oldModel <- get model value
                        newCenter <- viewToModel pt
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter }]
                        newViewerTile <- renderTile' renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                    Just box -> do
                        -- Completed a drag. Zoom in to the dragged box, unless
                        -- the box is pathologically small; in that case, treat
                        -- the action as if it were a simple click.
                        selectRegion box
                        oldModel <- get model value
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        newCenter <- viewToModel (viewportToPoint $ rectCenter box)
                        let (px, py) = modelPixelDim oldModel
                            (boxW, boxH) = dimensions box
                            oldArea = fromIntegral (w * h)
                            newArea = boxW * boxH
                            literalScale = sqrt (newArea / oldArea)
                            scale = if literalScale < 0.001 then 1 else literalScale
                        putStrLn ("new viewport center: " ++ show (viewportToPoint (rectCenter box)))
                        putStrLn ("new model center: " ++ show (toCoords newCenter) ++ ", scale by: " ++ show scale)
                        set model [value := Model
                                    { modelCenter = toCoords newCenter
                                    , modelPixelDim = (px * scale, py * scale) }]
                        newViewerTile <- renderTile' renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                        repaint p

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

          , on drag := \pt -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                mpt <- viewToModel pt
                set status [text := show mpt]

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing -> return ()
                    Just _  -> repaint p

                propagateEvent

          , on motion := \pt -> do
                mpt <- viewToModel pt
                set status [text := show mpt]
                propagateEvent
          ]

    -- Add a timer which will check for repainting requests, ~10Hz
    _ <- timer f [ interval := 100
                 , on command := do
                        curTile <- get currentTile value
                        ifModified curTile $ do
                            viewRect <- windowGetViewRect f
                            tileImage <- generateTileImage curTile viewRect
                            set savedTileImage [value := Just tileImage]
                            repaint p
                 ]

    -- onResizeTimer is a one-shot timer that fires 100ms after the
    -- frame has been resized. If another resize event comes in during
    -- that interval, the timer is reset to 100ms. When the timer fires,
    -- we kick off a new rendering task to build the contents of the
    -- window. Using a timer lets us avoid starting hundreds of rendering
    -- tasks while the user adjusts their window size.
    onResizeTimer <- timer f [ interval := 100
                             , enabled := False ]
    set onResizeTimer [ on command := do
                              putStrLn "<timer>"
                              set onResizeTimer [enabled := False] -- one-shot
                              needResize <- get pendingResize value
                              when needResize $ do
                                  putStrLn "<resize>"
                                  set pendingResize [value := False]
                                  Size { sizeW = w0, sizeH = h0 } <- get f clientSize
                                  let w = roundUp w0 16
                                      h = roundUp h0 16
                                      roundUp x n = case x `mod` n of
                                          0 -> x
                                          k -> x + (n - k)
                                  newViewerTile <- renderTile' renderAction (w, h) model
                                  set currentTile [value := newViewerTile]
                      ]
      
    -- Add the status bar, menu bar, and layout to the frame
    set f [ statusBar := [status]
          , menuBar   := [file,hlp]
          , layout    := fill $ minsize (sz 512 512) $ widget p
          , on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent
          , on (menu about) := infoDialog f "About FractalStream" "Contributors:\nMatt Noonan"
          ]

renderTile' :: (Planar a, Valued w)
            => ([a] -> IO [Color])
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' action dim model = do
    modelRect <- modelToRect dim <$> get model value
    renderTile action dim modelRect
    
-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile    -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())

generateTileImage viewerTile _windowRect = do    
    let (width, height) = tileRect viewerTile
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("generateTileImage w=" ++ show width ++ ", h=" ++ show height
              ++ " on thread " ++ show tid ++ " " ++ show capInfo ++ " " ++ show bound)
    --let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    --let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
    --                 (fHeight + height) `div` 2 - height )

    withSynchedTileBuffer viewerTile $ \fptr -> do
      withForeignPtr fptr $ \buf -> do
        pbuf <- pixelBufferCreate (sz width height)
        pixels <- mapM (peekColor buf) [0..(width * height - 1)]
        pixelBufferSetPixels pbuf pixels
        result <- imageCreateFromPixelBuffer pbuf
        pure result

drawCenteredImage :: Image b -> DC d -> Rect -> (Int,Int) -> IO ()

drawCenteredImage img dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawImage dc img (WX.pt x0 y0) []

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
        _                  -> Nothing

drawBox :: DC d
        -> Color
        -> Color
        -> [Point]
        -> IO ()
drawBox dc fillColor lineColor coords =
    polygon dc coords [ brush := brushSolid fillColor
                      , pen := penColored lineColor 2
                      ]

selectRegion :: Rectangle Viewport -> IO ()
selectRegion r = do
    putStrLn $ "selected region " ++ show r
    return ()
