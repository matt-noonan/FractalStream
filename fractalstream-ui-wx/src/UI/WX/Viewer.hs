{-# language RecordWildCards #-}
{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : UI.WX.Viewer
Description : Interactive view based on wxWidgets
-}
module UI.WX.Viewer ( wxView
                    ) where

import qualified Data.Color as FSColor
import Data.Planar
import UI.Tile

import           Graphics.UI.WX hiding (pt)
import           qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcTypes      (rgba)
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ

import Control.Concurrent
import Data.Time (diffUTCTime, getCurrentTime)
import Data.IORef

import UI
import UI.WxWidgets

import Actor.Viewer

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

interpolateModel :: Double -> Model -> Model -> Model
interpolateModel t m1 m2 = m2
    { modelCenter   = interpolate modelCenter
    , modelPixelDim = logInterpolate modelPixelDim
    }
  where
    interp p q = p + t * (q - p)
    interp2 (p1,p2) (q1,q2) = (interp p1 q1, interp p2 q2)
    interpolate f = interp2 (f m1) (f m2)
    logInterp p q = p * (q/p) ** t
    logInterp2  (p1,p2) (q1,q2) = (logInterp p1 q1, logInterp p2 q2)
    logInterpolate f = logInterp2 (f m1) (f m2)

helloFrom :: String -> IO ()
helloFrom me = do
    tid <- myThreadId
    bound <- isCurrentThreadBound
    capInfo <- threadCapability tid
    putStrLn ("Hello from " ++ me
              ++ ", on thread " ++ show tid
              ++ " cap=" ++ show capInfo
              ++ " bound=" ++ show bound)


-- | Create a window with an interactive view of a complex-dynamical system.
wxView :: Rectangle (Double, Double)
          -- ^ The upper-left and lower-right corners of the view.
       -> ([(Double, Double)] -> IO [Color])
          -- ^ The rendering action
       -> Viewer
          -- ^ The script-defined viewer
       -> IO ()
wxView _modelRect renderAction testViewer = start $ do

    model <- variable [value := Model (0,0) (1/128,1/128)]

    helloFrom "wxView"

    let (width, height) = (512, 512)

    -- Create the main viewer frame
    f <- frame [ text := "FractalStream"
               , clientSize := sz width height ]

    _myViewer <- toUI @WX testViewer ()

    -- Create the menus

    -- File menu
    file <- menuPane      [text := "&File"]
    menuItem file [ text := "&New", on command := putStrLn "TODO"]
    _    <- menuQuit file [text := "&Quit", on command := close f]

    -- Help menu
    hlp   <- menuHelp      []
    about <- menuAbout hlp [text := "About FractalStream"]

    -- Viewer status bar
    status <- statusField   [text := "Pointer location"]
    toolStatus <- statusField [text := ""]

    -- Tool menu
    tools <- menuPane [text := "&Tool"]
    mapM_ (\(x,y,z) -> menuRadioItem tools [ text := x
                                           , help := y
                                           , on command := do
                                               set toolStatus [text := y]
                                               z])
      [ ("Navigate",
         "Move around a dynamical system, select a point by ctrl-clicking",
         do putStrLn "hi")
      , ("Trace",
         "Follow the orbit of a point",
         do putStrLn "bye")
      ]

    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]
    pendingResize <- variable [value := False]

    -- Panel and tile for initial view
    renderId <- newIORef (0 :: Int)
    viewerTile     <- renderTile' renderId renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]

    {-
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

    -}

    p <- panel f []


    -- trigger repaint
    let triggerRepaint = do
          repaint p
          windowRefresh p True -- True=redraw background
          windowUpdateWindowUI p

    animate <- variable [value := Nothing]
    let startAnimatingFrom oldModel = do
            now <- getCurrentTime
            img <- get savedTileImage value >>= traverse imageCopy
            set lastTileImage [value := img]
            set animate [value := Just (now, oldModel, img)]

    set p [ on paintRaw := \dc r _dirty -> get animate value >>= \case
                  Nothing -> do
                      -- Normal paint. Draw current rendering, then layer
                      -- tool imagery on top.
                      viewRect <- windowGetViewRect f
                      curTile <- get currentTile value
                      let (w, h) = tileRect curTile
                      get savedTileImage value >>= \case
                          Nothing -> pure ()
                          Just im -> drawCenteredImage im dc viewRect (w, h)
                      paintToolLayer lastClick draggedTo dc r viewRect

                  Just (startTime, oldModel, oldImage) -> do
                      -- Animated paint. Zoom and blend smoothly between
                      -- the new and old images.
                      now <- getCurrentTime
                      let speed :: forall n. Num n => n
                          speed = 4
                          blend = min 255 (round (speed * 255 * toRational (diffUTCTime now startTime)))
                          t = min 1.0 (speed * fromRational (toRational (diffUTCTime now startTime)) :: Double)
                      when (blend >= 255) (set animate [value := Nothing])
                      curTile <- get currentTile value
                      let (w, h) = tileRect curTile

                      gc <- graphicsContextCreate dc
                      newModel <- get model value
                      let midModel = interpolateModel t oldModel newModel
                          withLayer opacity action = do
                              graphicsContextBeginLayer gc opacity
                              action
                              graphicsContextEndLayer gc
                          restoringContext action = do
                              graphicsContextPushState gc
                              action
                              graphicsContextPopState gc

                      let zoom :: (Double, Double) -> (Double, Double) -> IO () -> IO ()
                          zoom (scaleX, scaleY) (cx, cy) action = do
                              restoringContext $ do
                                  graphicsContextTranslate gc cx cy
                                  graphicsContextScale gc (sz scaleX scaleY)
                                  action

                          viewCenterX = fromIntegral w / 2
                          viewCenterY = fromIntegral h / 2
                          dx = (fst (modelCenter newModel) - fst (modelCenter oldModel))
                              / fst (modelPixelDim oldModel)
                          dy = negate (snd (modelCenter newModel) - snd (modelCenter oldModel))
                                     / snd (modelPixelDim oldModel)

                      -- draw the old image
                      let k = 1 / sqrt ( (fst (modelPixelDim oldModel) * snd (modelPixelDim oldModel))
                                   / (fst (modelPixelDim newModel) * snd (modelPixelDim newModel)))
                          t' = if (k - 1)^2 < 0.05 then t else (1 - k ** t) / (1 - k)
                      graphicsContextTranslate gc viewCenterX viewCenterY
                      graphicsContextScale gc (sz (fst (modelPixelDim oldModel)
                                                 / fst (modelPixelDim midModel))
                                                  (snd (modelPixelDim oldModel)
                                                 / snd (modelPixelDim midModel)))
                      graphicsContextTranslate gc (negate viewCenterX) (negate viewCenterY)
                      graphicsContextTranslate gc (negate $ dx * t') (negate $ dy * t')

                      withLayer 1 $ restoringContext $ do
                              zoom (1.0, 1.0)
                                   (viewCenterX, viewCenterY)

                              $ case oldImage of
                                    Nothing -> pure ()
                                    Just im -> drawImage dc im (WX.pt
                                                               (round $ negate viewCenterX)
                                                               (round $ negate viewCenterY)) []
                      -- draw the new image
                      withLayer (min 1 t) $ do
                          let zoomRatioX = fst (modelPixelDim newModel) / fst (modelPixelDim oldModel)
                              zoomRatioY = snd (modelPixelDim newModel) / snd (modelPixelDim oldModel)
                          restoringContext $ do
                              zoom (zoomRatioX, zoomRatioY)
                                   (viewCenterX + dx, viewCenterY + dy)

                                   $ get savedTileImage value >>= \case
                                       Nothing -> pure ()
                                       Just im -> drawImage dc im (WX.pt
                                                                  (round $ negate viewCenterX)
                                                                  (round $ negate viewCenterY)) []
          ]

    let viewToModel pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @(Double,Double) dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))

    -- Set click and drag event handlers
    set p [ on mouse   := \case
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> do
                        -- Completed a click, recenter to the clicked point.
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        oldModel <- get model value
                        newCenter <- viewToModel pt
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter }]

                        get currentTile value >>= cancelTile
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

                        startAnimatingFrom oldModel
                        triggerRepaint
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
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter
                                    , modelPixelDim = (px * scale, py * scale)
                                    }]
                        get currentTile value >>= cancelTile
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                        startAnimatingFrom oldModel
                        triggerRepaint

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                mpt <- viewToModel pt
                set status [text := show mpt]

                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing -> return ()
                    Just _  -> triggerRepaint

                propagateEvent

              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                set status [text := show mpt]
                propagateEvent

              -- other mouse events
              _ -> propagateEvent
          ]

    -- Add a timer which will check for repainting requests, ~10Hz
    _ <- timer f [ interval := 100
                 , enabled := True
                 , on command := do
                        curTile <- get currentTile value
                        ifModified curTile $ do
                            viewRect <- windowGetViewRect f
                            tileImage <- generateTileImage curTile viewRect
                            saved <- imageCopy tileImage
                            set savedTileImage [value := Just saved]
                            triggerRepaint
                 ]

    -- Animation timer. At ~65Hz, check if we are animating between
    -- two views. If so, step the animation and repaint.
    _ <- timer f [ interval := 16
                 , enabled := True
                 , on command := get animate value >>= \case
                         Nothing -> pure ()
                         Just _  -> triggerRepaint
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
                              set onResizeTimer [enabled := False] -- one-shot
                              needResize <- get pendingResize value
                              when needResize $ do
                                  set pendingResize [value := False]
                                  Size { sizeW = w0, sizeH = h0 } <- get f clientSize
                                  let w = roundUp w0 16
                                      h = roundUp h0 16
                                      roundUp x n = case x `mod` n of
                                          0 -> x
                                          k -> x + (n - k)
                                  get currentTile value >>= cancelTile
                                  newViewerTile <- renderTile' renderId renderAction (w, h) model
                                  set currentTile [value := newViewerTile]
                                  -- no animation?
                                  triggerRepaint
                      ]

    -- Add the status bar, menu bar, and layout to the frame
    set f [ statusBar := [status, toolStatus]
          , menuBar   := [file,tools,hlp]
          , layout    := fill $ minsize (sz 512 512) $ widget p
          , on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent
          , on (menu about) :=
              infoDialog f "About FractalStream" $ unlines
              [ "Contributors:"
              , "Matt Noonan"
              ]
          ]


renderTile' :: Valued w
            => IORef Int
            -> ([(Double, Double)] -> IO [Color])
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    modelRect <- modelToRect dim <$> get model value
    let action' pts = map colorConvert <$> do
            curId <- readIORef renderId
            if (curId == iD) then action pts else pure []
        colorConvert c =
          FSColor.rgbToColor (colorRed c, colorGreen c, colorBlue c)
    renderTile action' dim modelRect

-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile    -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())

generateTileImage viewerTile _windowRect = do
    let (width, height) = tileRect viewerTile
    --let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    --let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
    --                 (fHeight + height) `div` 2 - height )
    --putStrLn "generateTileImage"
    withSynchedTileBuffer viewerTile (imageCreateFromData (sz width height))

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
