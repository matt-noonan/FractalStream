{-# language OverloadedStrings, UndecidableInstances, NumericUnderscores #-}
{-# options_ghc -Wno-orphans #-}

module UI.ProjectViewer
  ( viewProject
  ) where

import FractalStream.Prelude hiding (get)

import Language.Environment
import Data.Color (Color, colorToRGB)
import Control.Concurrent.MVar

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcDefs
import           Graphics.UI.WXCore.WxcTypes      (rgba)
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (frameDefaultStyle)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Planar

import UI.Tile
import UI.Layout

import Data.IORef

import Actor.UI
import Actor.Tool
import Actor.Layout
import Actor.Configuration
import Actor.Viewer.Complex
import Actor.Event (Event(..))
import Language.Draw

import Data.DynamicValue
import Language.Code (usedVarsInCode)
import Task.Block (BlockComputeAction)

import qualified Data.Set as Set

viewProject :: Window ()
            -> (forall a. Frame a -> [Menu ()] -> IO ())
            -> UI
viewProject projectWindow addMenuBar = UI
  { newEnsemble = pure ()

  , runSetup = \_ title setupUI continue -> do
      f <- frameEx frameDefaultStyle [ text := title
                 , on resize := propagateEvent
                 ] projectWindow

      addMenuBar f []
      WX.windowOnClose f (set f [ visible := False ])

      p <- panel f []
      (stopListening, innerLayout) <- generateWxLayout (\_ -> pure ()) p setupUI
      compileButton <- button p [ text := "Go!"
                                , on command := do
                                    set f [ visible := False ]
                                    continue
                                ]
      set f [ layout := margin 5 . container p $ fill $ column 5
              $ [ innerLayout, hfloatRight $ widget compileButton ]
            , on closing :~ (>> stopListening)
            ]
      windowReLayout f

  , makeLayout = \_ title ui -> do
      f <- frameEx frameDefaultStyle [ text := title
                 , on resize := propagateEvent
                 ] projectWindow

      WX.windowOnClose f (set f [ visible := False ])

      addMenuBar f []

      (stopListening, innerLayout) <- generateWxLayout (\_ -> pure ()) f ui
      set f [ layout := fill . margin 5 . column 5 $ [ innerLayout ]
            , on closing :~ (>> stopListening) ]
      windowReLayout f

  , makeViewer = const (makeWxComplexViewer projectWindow addMenuBar)
  }

makeWxComplexViewer :: Window ()
                    -> (forall a. Frame a -> [Menu ()] -> IO ())
                    -> ViewerUIProperties
                    -> ComplexViewer'
                    -> IO ()
makeWxComplexViewer
  projectWindow
  addMenuBar
  vup@ViewerUIProperties{..}
  theViewer@ComplexViewer'{..} = do

    let listenWith :: a -> (b -> c -> IO ()) -> IO ()
        listenWith _ _ = putStrLn "listenWith TODO"

    let clone = do
          newCV <- cloneComplexViewer theViewer
          makeWxComplexViewer projectWindow addMenuBar vup newCV

    let (width, height) = (fst vpSize, snd vpSize)
    f <- frameEx frameDefaultStyle [ text := vpTitle
               , on resize := propagateEvent
               ] projectWindow
    WX.windowOnClose f (set f [ visible := False ])

    p <- scrolledWindow f [ scrollRate := sz 10 10 ]

    -- Viewer status bar
    status <- statusField [ text := "Pointer location" ]
    toolStatus <- statusField [text := ""]

    set f [ statusBar := [toolStatus, status]
          , layout := fill (minsize (sz 128 128) (widget p))
          , clientSize := sz width height
          ]
    windowReLayout f

    -- `requestRefresh` can be used from any thread to queue up a
    -- window refresh.
    offThreadRefresh <- newEmptyMVar
    let requestRefresh = void (tryPutMVar offThreadRefresh ())

    -- Build the initial view model
    initialX :+ initialY <- getDynamic cvCenter'
    initialPx <- getDynamic cvPixelSize'

    let initialModel = Model (initialX, initialY) (initialPx, initialPx)
    model <- variable [value := initialModel]

    -- History tracking
    history <- newIORef (History [] initialModel [])

    -- Get the tools
    let theTools = cvTools'

    -------------------------------------------------------
    -- Main view
    -------------------------------------------------------


    -- trigger repaint
    let triggerRepaint = do
          isVisible <- get f visible
          when isVisible $ do
            repaint p
            windowRefresh p True -- True=redraw background
            windowUpdateWindowUI p

    renderId <- newIORef (0 :: Int)

    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]
    pendingResize <- variable [value := False]

    viewerTile     <- do
      renderAction <- cvGetFunction
      renderTile' renderId True renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]
    currentToolIndex <- variable [value := Nothing]
    useSmoothing <- variable [value := True]

    let startAnimatingFrom oldModel = do
            alreadyAnimating <- isJust <$> get animate value
            unless alreadyAnimating $ do
              now <- getCurrentTime
              img <- get savedTileImage value >>= traverse imageCopy
              set lastTileImage [value := img]
              set animate [value := Just (now, oldModel, img)]

    -- Putting a value in this MVar is used to trigger a Refresh event
    -- on the currently active tool.
    needToSendRefreshEvent <- newEmptyMVar

    let changeViewTo newModel = do
          modifyIORef' history (historyAppend newModel)
          jumpViewTo newModel

        jumpViewTo newModel = do
          oldModel <- get model value
          Size { sizeW = w, sizeH = h } <- get f clientSize
          set model [ value := newModel ]
          get currentTile value >>= cancelTile
          renderAction <- cvGetFunction
          smoothing <- get useSmoothing value
          newViewerTile <- renderTile' renderId smoothing renderAction (w, h) model
          set currentTile [ value := newViewerTile ]
          startAnimatingFrom oldModel
          void $ tryPutMVar needToSendRefreshEvent ()
          triggerRepaint


    -- Convert back and forth between viewport coordinates and coordinates
    -- in the underlying model (e.g. viewport coordinates to C-plane coordinates in
    -- the dynamical system or parameter plane)
    let viewToModel pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @(Double,Double) dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))
        modelToView mdl pt = do
          Size { sizeW = w, sizeH = h } <- get f clientSize
          let dim = (w,h)
              fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
          let modelRect = modelToRect @(Double,Double) dim mdl
          let Viewport (px, py) = convertRect modelRect fullViewRect pt
          pure (point px py)

    -- Set paint handlers
    set p [ on paintRaw := \dc r _dirty -> get animate value >>= \case
              Nothing -> do
                gc <- graphicsContextCreate dc
                -- Normal paint. Draw current rendering, then layer
                -- tool imagery on top.
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (w, h) = tileRect curTile
                get savedTileImage value >>= \case
                  Nothing -> pure ()
                  Just im -> drawCenteredImage im dc viewRect (w, h)

                mdl <- get model value
                get currentToolIndex value >>= \case
                  Just{}  -> paintToolLayer (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands dc
                  Nothing -> paintToolLayerWithDragBox (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands lastClick draggedTo dc r viewRect

                -- Flush the graphics context
                graphicsContextDelete gc

              Just (startTime, oldModel, oldImage) -> do
                -- Animated paint. Zoom and blend smoothly between
                -- the new and old images.
                now <- getCurrentTime
                let speed :: forall n. Num n => n
                    speed = 6
                    blend = min 255 (round (speed * 255 * toRational (diffUTCTime now startTime)) :: Integer)
                    t = min 1.0 (speed * fromRational (toRational (diffUTCTime now startTime)) :: Double)
                when (blend >= 255) (set animate [value := Nothing])
                curTile <- get currentTile value
                let (w, h) = tileRect curTile

                gc <- graphicsContextCreate dc
                newModel <- get model value
                let midModel = interpolateModel t oldModel newModel
                    withLayer (opacity :: Double) action = do
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
                restoringContext $ do
                  let k = 1 / sqrt ( (fst (modelPixelDim oldModel) * snd (modelPixelDim oldModel))
                                   / (fst (modelPixelDim newModel) * snd (modelPixelDim newModel)))
                      t' = if (k - 1)^(2 :: Int) < 0.05 then t else (1 - k ** t) / (1 - k)
                  graphicsContextTranslate gc viewCenterX viewCenterY
                  graphicsContextScale gc (sz (fst (modelPixelDim oldModel)
                                              / fst (modelPixelDim midModel))
                                            (snd (modelPixelDim oldModel)
                                              / snd (modelPixelDim midModel)))
                  graphicsContextTranslate gc (negate viewCenterX) (negate viewCenterY)
                  graphicsContextTranslate gc (negate $ dx * t') (negate $ dy * t')

                  withLayer 1 $ restoringContext $ do
                    zoom (1.0, 1.0) (viewCenterX, viewCenterY) $
                      case oldImage of
                        Nothing -> pure ()
                        Just im -> do
                          drawImage dc im (WX.pt
                                           (round $ negate viewCenterX)
                                           (round $ negate viewCenterY))
                            []
                  -- draw the new image
                  withLayer (min 1 t) $ do
                    let zoomRatioX = fst (modelPixelDim newModel) / fst (modelPixelDim oldModel)
                        zoomRatioY = snd (modelPixelDim newModel) / snd (modelPixelDim oldModel)
                    restoringContext $ do
                      zoom (zoomRatioX, zoomRatioY)
                           (viewCenterX + dx, viewCenterY + dy)
                        $ get savedTileImage value >>= \case
                              Nothing -> pure ()
                              Just im -> do
                                drawImage dc im (WX.pt
                                              (round $ negate viewCenterX)
                                              (round $ negate viewCenterY)) []
                paintToolLayer (modelToView midModel) (modelPixelDim midModel) cvGetDrawCommands dc

                -- Flush the graphics context
                graphicsContextDelete gc
          ]

    lastKnownMouse <- newIORef Nothing

    let getToolEventHandler ix = getDynamic (toolEventHandler (theTools !! ix))
        runToolEventHandler ix e = do
          handle <- getToolEventHandler ix
          fromMaybe (pure ()) (handle e)

    -- Set click and drag event handlers
    set p [ on mouse   := \case
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                let recenterAction = do
                      -- Completed a click, recenter to the clicked point.
                      oldModel <- get model value
                      newCenter <- viewToModel pt
                      changeViewTo oldModel { modelCenter = toCoords newCenter }
                ptz <- viewToModel pt
                toolClickAction <- get currentToolIndex value <&> \case
                  Nothing -> recenterAction
                  Just ix -> do
                    handle <- getToolEventHandler ix
                    case handle (Click ptz) of
                      Nothing -> recenterAction
                      Just action -> do
                        action
                        cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint

                case dragBox of
                    Nothing  -> toolClickAction

                    Just box -> do
                      let finishDrag = do
                            -- Completed a drag. Zoom in to the dragged box, unless
                            -- the box is pathologically small; in that case, treat
                            -- the action as if it were a simple click.
                            oldModel <- get model value
                            Size { sizeW = w, sizeH = h } <- get f clientSize
                            newCenter <- viewToModel (viewportToPoint $ rectCenter box)
                            let (px, py) = modelPixelDim oldModel
                                (boxW, boxH) = dimensions box
                                oldArea = fromIntegral (w * h)
                                newArea = boxW * boxH
                                scale = sqrt (newArea / oldArea)
                            if scale < 0.001
                              then toolClickAction
                              else changeViewTo Model { modelCenter = toCoords newCenter
                                                      , modelPixelDim = (px * scale, py * scale) }
                      get currentToolIndex value >>= \case
                        Just ix -> do
                          get lastClick value >>= \case
                            Nothing -> pure ()
                            Just (Viewport vstart) -> do
                              zstart <- viewToModel (uncurry Point vstart)
                              z <- viewToModel pt
                              handle <- getToolEventHandler ix
                              case handle (DragDone z zstart) of
                                Just action -> do
                                  action
                                  cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                                Nothing -> finishDrag
                        Nothing -> finishDrag

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                set status [text := show mpt]
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                let dragAction = do
                      dragBox <- getDragBox lastClick draggedTo
                      case dragBox of
                        Nothing -> return ()
                        Just _  -> triggerRepaint

                      propagateEvent

                get currentToolIndex value >>= \case
                  Nothing -> dragAction

                  Just ix -> do
                     get lastClick value >>= \case
                       Nothing -> pure ()
                       Just (Viewport vstart) -> do
                         zstart <- viewToModel (uncurry Point vstart)
                         z <- viewToModel pt
                         handle <- getToolEventHandler ix
                         case handle (Drag z zstart) of
                           Just action -> do
                             action
                             cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                           Nothing -> dragAction
                     propagateEvent


              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                writeIORef lastKnownMouse (Just mpt)
                set status [text := show mpt]
                propagateEvent

              MouseLeftDClick pt modifiers | isNoShiftAltControlDown modifiers -> do
                get currentToolIndex value >>= \case
                   Nothing -> pure ()
                   Just ix -> do
                     z <- viewToModel pt
                     runToolEventHandler ix (DoubleClick z)
                     cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     propagateEvent

              -- other mouse events
              _ -> propagateEvent
          ]

    WX.windowOnScroll p $ \scroll -> do
      let mb = case scroll of
            WX.ScrollLineUp   WX.Vertical _ -> Just False
            WX.ScrollLineDown WX.Vertical _ -> Just True
            _ -> Nothing
      case mb of
        Just b -> do

            oldModel <- get model value
            let (px, py) = modelPixelDim oldModel
                speed = 4
                scale = if b then speed else 1.0 / speed
            {-
            -- This *seems* like it should work to get the position of the
            -- mouse, based on similar code in wxcore's Events module for
            -- getting the coordinates of mouse events. But no luck, so
            -- for now we're just stealing the last known mouse coordinate.
            -- FIXME
            ptrLoc' <- wxcGetMousePosition
            ptrLoc <- windowCalcUnscrolledPosition (objectCast p) ptrLoc'
            (pmx, pmy) <- viewToModel ptrLoc
            -}
            -- Calculate the new model center, so that the model coordinates of
            -- the pointer are the same in the new model and the old model.
            readIORef lastKnownMouse >>= \case
              Nothing -> propagateEvent
              Just (pmx, pmy) -> do
                isAnimating <- isJust <$> get animate value
                unless isAnimating $ do
                  let (cmx, cmy) = modelCenter oldModel
                      cmx' = pmx + (cmx - pmx) * scale
                      cmy' = pmy + (cmy - pmy) * scale

                  changeViewTo Model { modelPixelDim = (px * scale, py * scale)
                                     , modelCenter = (cmx', cmy') }

        _ -> propagateEvent


    -- Add a timer which will check for repainting requests from WX, ~10Hz
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

    -- Add a timer which will check for repainting requests from off the main
    -- UI thread, ~10Hz
    _ <- timer f [ interval := 100
                 , enabled := True
                 , on command := do
                     needRefresh <- (== Just ()) <$> tryTakeMVar offThreadRefresh
                     when needRefresh (changeViewTo =<< get model value)
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
                                  jumpViewTo =<< get model value
                      ]

    -- Add a timer which checks if the tool layer should receive a refresh event.
    _ <- timer f [ interval := 50
                 , enabled := True
                 , on command := do
                     needToSendRefresh <- isJust <$> tryTakeMVar needToSendRefreshEvent
                     when needToSendRefresh $ get currentToolIndex value >>= \case
                       Nothing -> pure ()
                       Just ix -> do
                         runToolEventHandler ix Refresh
                         cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     ]

    set f [ on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value   := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent ]

    -------------------------------------------------------
    -- Change tracking
    -------------------------------------------------------

    -- For each variable that the viewer code depends on, trigger a repaint whenever
    -- that variable changes.
    let usedVars = Set.delete (symbolVal cvCoord')
                 $ execState (usedVarsInCode cvCode') Set.empty

    fromContextM_ (\name _ v ->
                      when (symbolVal name `Set.member` usedVars)
                        (v `listenWith` (\_ _ -> requestRefresh))) cvConfig'

    -------------------------------------------------------
    -- Menus
    -------------------------------------------------------

    -- Viewer menu
    viewMenu <- menuPane [text := "&Viewer"]
    menuItem viewMenu [ text := "Reset view\t^"
                      , on command := (historyFirst <$> readIORef history) >>= \case
                          Nothing -> requestRefresh
                          Just (h', m') -> do
                            writeIORef history h'
                            jumpViewTo m'
                      ]
    menuItem viewMenu [ text := "Previous view\t<"
                      , on command := (historyBack <$> readIORef history) >>= \case
                          Nothing -> wxcBell
                          Just (h', m') -> do
                            writeIORef history h'
                            jumpViewTo m'
                      ]
    menuItem viewMenu [ text := "Next view\t>"
                      , on command := (historyForward <$> readIORef history) >>= \case
                          Nothing -> wxcBell
                          Just (h', m') -> do
                            writeIORef history h'
                            jumpViewTo m'
                      ]
    menuItem viewMenu [ text := "Clone viewer\t2"
                      , on command := clone
                      ]
    menuLine viewMenu
    smoothingSelection <- menuItem viewMenu [
      text := "Use subpixel smoothing"
      , checkable := True
      , checked := True
      ]
    set smoothingSelection [
      on command := do
          isChecked <- get smoothingSelection checked
          set useSmoothing [ value := isChecked ]
          requestRefresh
      ]

    -- Tool menu
    tools <- menuPane [text := "&Tool"]

    -- Build the configuration window for each tool
    showToolConfig <- forM (zip [0..] theTools) $ \(ix, Tool{..}) -> getDynamic toolConfig >>= \case
      Nothing  -> pure (\_ -> pure ())
      Just tcfg -> do
        n <- getDynamic (source $ tiName toolInfo)
        tf <- frameTool [ text := n
                        , visible := False
                        , style := wxFRAME_TOOL_WINDOW .+. wxRESIZE_BORDER .+. wxCAPTION
                        ] f
        WX.windowOnClose tf (set tf [ visible := False ])
        let buttonPress txt = do
              runToolEventHandler ix (ButtonPressed txt)
              cvDrawCommandsChanged >>= \yn -> when yn triggerRepaint
        (stopListening, tlo) <- generateWxLayout buttonPress tf (coContents tcfg)
        set tf [ layout := margin 10 $ fill $ tlo
               , on closing :~ (>> stopListening) ]
        windowReLayout tf
        pure (\viz -> do
                 set tf [ visible := viz ]
                 -- Un-steal focus from the config window
                 when viz (set f [ visible := True ]))

    nav <- menuRadioItem tools [ text := "Navigate\tn"
                               , help := "Zoom and translate the view"
                               , on command := do
                                   set toolStatus [text := "Navigate"]
                                   -- When the navigate tool is selected, send the
                                   -- current tool a Deactivated event
                                   get currentToolIndex value >>= \case
                                     Nothing -> pure ()
                                     Just i -> do
                                       runToolEventHandler i Deactivated
                                       (showToolConfig !! i) False
                                       cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                                   set currentToolIndex [value := Nothing]
                               ]

    -- Change tracking for variables used in each tool
    forM_ theTools $ \Tool{..} -> do
      fromContextM_ (\name _ v -> do
                        tvs <- getDynamic toolVars
                        when (symbolVal name `Set.member` tvs) $ do
                          (v `listenWith` (\_ _ -> do
                                              -- Don't run the refresh handler here,
                                              -- we could deadlock since it will also
                                              -- want to access this variable. Just
                                              -- queue up a refresh for later.
                                              void $ tryPutMVar needToSendRefreshEvent ()
                                          ))) cvConfig'
      (fmap coContents <$> getDynamic toolConfig) >>= \case
        Nothing -> pure ()
        Just tlo -> getDynamic (layoutContext tlo) >>= \case
          Left err -> putStrLn ("ERROR: " ++ err)
          Right (SomeContext ctx) ->
            fromContextM_ (\name _ v -> do
                          tvs <- getDynamic toolVars
                          when (symbolVal name `Set.member` tvs) $ do
                            (v `listenWith` (\_ _ -> do
                                               -- Don't run the refresh handler here,
                                               -- we could deadlock since it will also
                                               -- want to access this variable. Just
                                               -- queue up a refresh for later.
                                                void $ tryPutMVar needToSendRefreshEvent ()))) ctx


    -- Add each tool to the tool menu
    forM_  (zip [0..] . map toolInfo $ theTools) $ \(ix, ToolInfo{..}) -> do
      n <- getDynamic (source tiName)
      shortcut <- \case { [c] -> Just c; _ -> Nothing } <$> getDynamic tiShortcut
      hlp <- getDynamic tiShortHelp
      menuRadioItem tools
          [ text := n ++ maybe "" (\c -> "\t" ++ (c : "")) shortcut
          , help := hlp
          , on command := do
              -- When the menu item is selected, send the current tool
              -- a Deactivated event, and then send the selected tool
              -- an Activated event.
              get currentToolIndex value >>= \case
                Nothing -> pure ()
                Just i -> do
                  runToolEventHandler i Deactivated
                  (showToolConfig !! i) False
                  cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
              set toolStatus [text := n]
              set currentToolIndex [value := Just ix]
              runToolEventHandler ix Activated
              (showToolConfig !! ix) True
              shouldRefresh <- getDynamic (toolRefreshOnActivate (theTools !! ix))
              when shouldRefresh (runToolEventHandler ix Refresh)
              cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
          ]

    -- Select the Navigate tool to start
    set nav [ checked := True ]

    let menuBarItems =
          [ viewMenu ] ++
          [ tools | length theTools > 0 ]

    addMenuBar f menuBarItems

-- | A model of the view, in terms of the underlying coordinate system.
data Model = Model
  { modelCenter   :: (Double, Double)
  , modelPixelDim :: (Double, Double)
  }
  deriving Eq

-- | Given the dimensions of the window, work out a rectangle describing
-- the viewport in the underlying coordinate system.
modelToRect :: Planar a => (Int,Int) -> Model -> Rectangle a
modelToRect (w,h) Model{..} = flippedRectangle (fromCoords ul) (fromCoords lr)
  where
    ul = (cx - px * fromIntegral w / 2, cy - py * fromIntegral h / 2)
    lr = (cx + px * fromIntegral w / 2, cy + py * fromIntegral h / 2)
    (cx, cy) = modelCenter
    (px, py) = modelPixelDim

-- | Smoothly interpolate between two models
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

-- | A wrapper around UI.Tile.renderTile that tags each rendering
-- action with an ID, and will stop rendering when the ID no longer
-- matches the current ID value.
renderTile' :: Valued w
            => IORef Int
            -> Bool
            -> BlockComputeAction
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId smooth action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    modelRect <- modelToRect dim <$> get model value
    let action' p q r x y c = do
            curId <- readIORef renderId
            if (curId == iD) then action p q r x y c else pure ()
    renderTile smooth action' dim modelRect

drawCenteredImage :: Image b -> DC d -> Rect -> (Int,Int) -> IO ()
drawCenteredImage img dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawImage dc img (WX.pt x0 y0) []

viewportToPoint :: Viewport -> Point
viewportToPoint (Viewport (x,y)) = Point { pointX = x, pointY = y }


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
        -> WX.Color
        -> WX.Color
        -> [Point]
        -> IO ()
drawBox dc fillColor lineColor coords =
    polygon dc coords [ brush := brushSolid fillColor
                      , pen := penColored lineColor 2
                      ]

-- | Paint the state of a tile into a device context.
generateTileImage
    :: Tile    -- ^ A tile to convert to an image
    -> Rect    -- ^ The enclosing view rectangle
    -> IO (Image ())
generateTileImage viewerTile _windowRect = do
    let (width, height) = tileRect viewerTile
    withSynchedTileBuffer viewerTile (imageCreateFromData (sz width height))

-- | Read draw commands from each tool layer and paint them into the
-- given device context, and then draw the zoom drag box on top.
paintToolLayerWithDragBox
  :: ((Double, Double) -> IO Point)
  -> (Double, Double)
  -> IO [[DrawCommand]]
  -> Var (Maybe Viewport)
  -> Var (Maybe Viewport)
  -> DC d
  -> Rect
  -> Rect
  -> IO ()
paintToolLayerWithDragBox modelToView pxDim getDrawCommands lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    paintToolLayer modelToView pxDim getDrawCommands dc
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba @Word8 0 128 255 128) white boxPts

-- | Read draw commands for each tool layer and paint them into the
-- given device context
paintToolLayer
  :: ((Double, Double) -> IO Point)
  -> (Double, Double)
  -> IO [[DrawCommand]]
  -> DC d
  -> IO ()
paintToolLayer modelToView pxDim getDrawCommands dc = dcEncapsulate dc $ do

    cmdss <- getDrawCommands
    let initialColor = rgba 255 255 255 (255 :: Word8)

    let withFill fil action
          | fil = action []
          | otherwise = action [ brush := brushTransparent ]
        pxSz = sqrt(fst pxDim * snd pxDim)

    forM_ cmdss $ \cmds -> do
      -- Reset the pen and brush for the next drawing layer
      set dc [ pen := penColored initialColor 2
             , brush := brushSolid initialColor
             , textColor := initialColor ]

      forM_ cmds $ \case

        DrawPoint _ pt -> do
          pt' <- modelToView pt
          pc <- get dc penColor
          oldBrush <- get dc brush
          set dc [ brush := brushSolid pc ]
          withFill True (circle dc pt' 2)
          set dc [ brush := oldBrush ]

        DrawLine _ pt1 pt2 -> do
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          withFill False (line dc pt1' pt2')

        DrawCircle _ fil r pt -> do
          pt' <- modelToView pt
          let r' = round (r / pxSz)
          withFill fil (circle dc pt' r')

        DrawRect _ fil pt1 pt3 -> do
          let pt2 = (fst pt1, snd pt2)
              pt4 = (snd pt1, fst pt2)
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          pt3' <- modelToView pt3
          pt4' <- modelToView pt4
          let boxpts = [pt1', pt2', pt3', pt4']
          withFill fil (polygon dc boxpts)

        Clear {} -> pure () -- clear operations should be handled upstream,
                            -- by truncating the draw command list
        SetStroke _ c ->
          set dc [ pen := penColored (fsColorToWxColor c) 2
                 , textColor := fsColorToWxColor c ]

        SetFill _ c ->
          set dc [ brush := brushSolid (fsColorToWxColor c) ]

        Write _ txt pt -> do
          Point x0 y0 <- modelToView pt
          Size tw th <- getTextExtent dc txt
          let pt' = Point (x0 - tw `div` 2) (y0 - th `div` 2)
          withFill False (drawText dc txt pt')

-- | Convert a FractalStream color to a WxWidgets color
fsColorToWxColor :: Color -> WX.Color
fsColorToWxColor c =
  let (r,g,b) = colorToRGB c in rgba r g b 255

data History = History [Model] Model [Model]

historyBack :: History -> Maybe (History, Model)
historyBack (History olds m news) = case olds of
  (m':olds') -> Just (History olds' m' (m:news), m')
  _ -> Nothing

historyForward :: History -> Maybe (History, Model)
historyForward (History olds m news) = case news of
  (m':news') -> Just (History (m:olds) m' news', m')
  _ -> Nothing

historyFirst :: History -> Maybe (History, Model)
historyFirst (History olds m news) = case reverse olds of
  (m':olds') -> Just (History [] m' (olds' ++ [m] ++ news), m')
  [] -> Nothing

historyAppend :: Model -> History -> History
historyAppend m h@(History olds m' _)
  | m == m'   = h
  | otherwise = History (m' : olds) m []
