{-# language OverloadedStrings, UndecidableInstances, NumericUnderscores #-}
{-# options_ghc -Wno-orphans #-}

module UI.Definition
  ( wxUI
  ) where

import Language.Environment
import Language.Type
import Data.Color (Color, colorToRGB, rgbToColor)
import Control.Concurrent.MVar
import Control.Monad.State hiding (get)

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.Draw
import           Graphics.UI.WXCore.WxcClassTypes
import           Graphics.UI.WXCore.WxcTypes      (rgba)
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Planar
import UI.Tile
import Data.IORef
import Data.Word
import Foreign (Ptr)

import Actor.Layout
import Actor.UI
import Actor.Tool
import Actor.Viewer.Complex
import Actor.Event (Event(..))
import Language.Effect.Draw

import Data.DynamicValue
import Language.Value (Value, ValueF(..))
import Language.Code (CodeF(..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits
import Data.Kind
import Data.Indexed.Functor
import Fcf (Exp, Eval, Pure1)

data Unit :: (Environment, FSType) -> Exp Type
type instance Eval (Unit et) = ()

wxUI :: UI
wxUI = UI {..}
  where

  newEnsemble = pure ()

  runSetup = \_ title setupUI continue -> do
    f <- frame [ text := title
               , on resize := propagateEvent
               ]

    WX.windowOnClose f (set f [ visible := False ])

    innerLayout <- generateWxLayout f setupUI
    compileButton <- button f [ text := "Go!"
                              , on command := do
                                  set f [ visible := False ]
                                  continue
                              ]
    set f [ layout := fill . margin 5 . column 5
                      $ [ innerLayout, widget compileButton ]
          ]

  makeLayout = \_ title ui -> do
    f <- frame [ text := title
               , on resize := propagateEvent
               ]

    WX.windowOnClose f (set f [ visible := False ])

    innerLayout <- generateWxLayout f ui
    set f [ layout := fill . margin 5 . column 5 $ [ innerLayout ] ]

  makeViewer = \_ vup@ViewerUIProperties{..} theViewer@ComplexViewer'{..} -> do

    let clone = do
          newCV <- cloneComplexViewer theViewer
          makeViewer () vup newCV

    f <- frame [ text := vpTitle
               , on resize := propagateEvent
               ]
    WX.windowOnClose f (set f [ visible := False ])

    let (width, height) = (fst vpSize, snd vpSize)
    p <- panel f [ clientSize := sz width height ]

    -- Viewer status bar
    status <- statusField [ text := "Pointer location" ]
    toolStatus <- statusField [text := ""]
    set f [ statusBar := [toolStatus, status]
          , layout := fill (minsize (sz 128 128) (widget p))
          ]

    -- `requestRefresh` can be used from any thread to queue up a
    -- window refresh.
    offThreadRefresh <- newEmptyMVar
    let requestRefresh = void (tryPutMVar offThreadRefresh ())

    -- Build the initial view model
    model <- variable [value := Model (0,0) (1/128,1/128)]

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
      renderTile' renderId renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]
    currentToolIndex <- variable [value := Nothing]

    let startAnimatingFrom oldModel = do
            now <- getCurrentTime
            img <- get savedTileImage value >>= traverse imageCopy
            set lastTileImage [value := img]
            set animate [value := Just (now, oldModel, img)]

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
                  Nothing -> paintToolLayer' (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands lastClick draggedTo dc r viewRect
                  Just{} -> paintToolLayer (modelToView mdl) (modelPixelDim mdl) cvGetDrawCommands dc

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
          ]


    -- Set click and drag event handlers
    set p [ on mouse   := \case
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                case dragBox of
                    Nothing  -> get currentToolIndex value >>= \case
                      Nothing -> do
                        -- Completed a click, recenter to the clicked point.
                        Size { sizeW = w, sizeH = h } <- get f clientSize
                        oldModel <- get model value
                        newCenter <- viewToModel pt
                        set model [value := oldModel
                                    { modelCenter = toCoords newCenter }]

                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

                        startAnimatingFrom oldModel
                        triggerRepaint
                      Just ix -> do
                        z <- viewToModel pt
                        toolEventHandler (theTools !! ix) (Click z)
                        cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint

                    Just box -> get currentToolIndex value >>= \case
                      Just ix -> do
                        get lastClick value >>= \case
                          Nothing -> pure ()
                          Just (Viewport vstart) -> do
                            zstart <- viewToModel (uncurry Point vstart)
                            z <- viewToModel pt
                            toolEventHandler (theTools !! ix) (DragDone z zstart)
                            cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                      Nothing -> do
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
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]
                        startAnimatingFrom oldModel
                        triggerRepaint

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                get currentToolIndex value >>= \case
                  Nothing -> do
                    mpt <- viewToModel pt
                    set status [text := show mpt]

                    dragBox <- getDragBox lastClick draggedTo
                    case dragBox of
                      Nothing -> return ()
                      Just _  -> triggerRepaint

                    propagateEvent

                  Just ix -> do
                     get lastClick value >>= \case
                       Nothing -> pure ()
                       Just (Viewport vstart) -> do
                         zstart <- viewToModel (uncurry Point vstart)
                         z <- viewToModel pt
                         toolEventHandler (theTools !! ix) (Drag z zstart)
                         cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     propagateEvent


              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                set status [text := show (pt, mpt)]
                propagateEvent

              MouseLeftDClick pt modifiers | isNoShiftAltControlDown modifiers -> do
                get currentToolIndex value >>= \case
                   Nothing -> pure ()
                   Just ix -> do
                     z <- viewToModel pt
                     toolEventHandler (theTools !! ix) (DoubleClick z)
                     cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
                     propagateEvent

              -- other mouse events
              _ -> propagateEvent
          ]

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
                     when needRefresh $ do
                        Size { sizeW = w, sizeH = h } <- get f clientSize

                        get currentTile value >>= cancelTile
                        renderAction <- cvGetFunction
                        newViewerTile <- renderTile' renderId renderAction (w, h) model
                        set currentTile [value := newViewerTile]

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
                                  renderAction <- cvGetFunction
                                  newViewerTile <- renderTile' renderId renderAction (w, h) model
                                  set currentTile [value := newViewerTile]
                                  -- no animation?
                                  triggerRepaint ]


    set f [ on resize := do
                  set onResizeTimer [enabled := False]
                  set pendingResize [value := True]
                  set onResizeTimer [enabled := True]
                  propagateEvent ]

    -------------------------------------------------------
    -- Change tracking
    -------------------------------------------------------

    -- For each variable that the viewer code depends on, trigger a repaint whenever
    -- that variable changes.
    let usedVars = execState (indexedFoldM gatherUsedVarsInCode cvCode') Set.empty
        gatherUsedVars :: Value et -> State (Set String) ()
        gatherUsedVars = indexedFoldM @Unit gatherUsedVarsInValue
        gatherUsedVarsInValue :: ValueF Unit et -> State (Set String) ()
        gatherUsedVarsInValue = \case
          Var name _ _ -> modify' (Set.insert (symbolVal name))
          LocalLet name _ _ _ _ _ -> modify' (Set.delete (symbolVal name))
          _ -> pure ()
        gatherUsedVarsInCode :: CodeF '[] (Pure1 Value) Unit et -> State (Set String) ()
        gatherUsedVarsInCode = \case
          Let _ name _ v _ _ -> do
            modify' (Set.delete (symbolVal name))
            gatherUsedVars v
          LetBind _ name _ _ _ _ -> modify' (Set.delete (symbolVal name))
          Set _ _ _ v -> gatherUsedVars v
          SetBind{} -> pure ()
          Call{} -> pure ()
          Block{} -> pure ()
          Pure _ v -> gatherUsedVars v
          NoOp{} -> pure ()
          DoWhile{} -> pure ()
          IfThenElse _ tf _ _ -> gatherUsedVars tf
          Effect{} -> error "TODO: Effect"

    fromContextM_ (\name _ v ->
                      when (symbolVal name `Set.member` usedVars)
                        (v `listenWith` (\_ _ -> requestRefresh))) cvConfig'

    -------------------------------------------------------
    -- Tools
    -------------------------------------------------------
    forM_ theTools $ \Tool{..} -> do
      putStrLn ("loading tool `" ++ tiName toolInfo ++ "`")

    -------------------------------------------------------
    -- Menus
    -------------------------------------------------------

    -- File menu
    file <- menuPane      [text := "&File"]
    menuItem file [ text := "&New", on command := putStrLn "TODO"]
--    _    <- menuQuit file [ text := "&Quit"
--                          , help := "Quit FractalStream"]

    -- Help menu
    hlp   <- menuHelp      [ text := "&Help" ]
    menuItem hlp [ text := "blah" ]
    about <- menuAbout hlp [text := "About FractalStream"]

    -- Viewer menu
    viewMenu <- menuPane [text := "&Viewer"]
    menuItem viewMenu [ text := "Reset view\t^"
                      , on command := do
                          set model [ value := Model (0,0) (1/128,1/128) ]
                          requestRefresh
                      ]
    menuItem viewMenu [ text := "Clone viewer\t2"
                      , on command := clone
                      ]

    -- Tool menu
    tools <- menuPane [text := "&Tool"]

    nav <- menuRadioItem tools [ text := "Navigate\tn"
                               , help := "Move around or whatever"
                               , on command := do
                                   set toolStatus [text := "Navigate"]
                                   set currentToolIndex [value := Nothing]
                               ]

    let addToToolMenu ix ToolInfo{..} = menuRadioItem tools
          [ text := tiName ++ maybe "" (\c -> "\t" ++ (c : "")) tiShortcut
          , help := tiShortHelp
          , on command := do
              get currentToolIndex value >>= \case
                Nothing -> pure ()
                Just i -> do
                  toolEventHandler (theTools !! i) Deactivated
                  cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
              set toolStatus [text := tiName]
              set currentToolIndex [value := Just ix]
              toolEventHandler (theTools !! ix) Activated
              cvDrawCommandsChanged >>= \tf -> when tf triggerRepaint
          ]

    mapM_ (uncurry addToToolMenu) ( zip [0..] . map toolInfo $ theTools )
    set nav [ checked := True ]

    let menuBarItems
          = [ file, viewMenu ]
          ++ [ tools | length theTools > 0 ]
          ++ [ hlp ]

    set f [ menuBar := menuBarItems
          , on (menu about) :=
              infoDialog f "About FractalStream" $ unlines
              [ "Contributors:"
              , "Matt Noonan"
              ]
          ]


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

generateWxLayout :: Dynamic dyn
                 => Window a
                 -> Layout dyn
                 -> IO WX.Layout

generateWxLayout frame0 wLayout = do
  panel0 <- panel frame0 []
  computedLayout <- go panel0 wLayout
  pure (container panel0 computedLayout)

 where

   go :: Dynamic dyn => Window a -> Layout dyn -> IO WX.Layout
   go p = \case

     Panel pTitle inner -> do
       p' <- panel p [ ]
       go p' inner
       pure (fill $ boxed pTitle (widget p'))

     Vertical parts -> do
       p' <- panel p []
       lo <- fill . column 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (fill (widget p'))

     Horizontal parts -> do
       p' <- panel p []
       lo <- fill . margin 10 . row 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (fill (widget p'))

     Tabbed ts -> do
       nb <- feed2 [ visible := True ] 0 $
             initialWindow $ \iD rect' ps s -> do
                   e <- notebookCreate p iD rect' s
                   set e ps
                   pure e
       forM_ ts $ \(lab, x) -> do
         c <- panel nb []
         page <- go c x
         set c [ layout := page ]
         notebookAddPage nb c lab True (-1)
       notebookSetSelection nb 0
       pure (fill $ widget nb)

     ColorPicker (Label lab) v -> do
       (r0, g0, b0) <- colorToRGB <$> getDynamic v
       picker <- feed2 [ text := lab, visible := True ] 0 $
                 initialWindow $ \iD rect' ps s -> do
                   e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
                   set e ps
                   pure e
       let newPick = do
             c <- colorPickerCtrlGetColour picker
             let r = colorRed c
                 g = colorGreen c
                 b = colorBlue c
             void (setDynamic v (rgbToColor (r, g, b)))
       WX.windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
       pure (fill $ row 5 [ margin 3 (label lab), hfill (widget picker) ])

     CheckBox (Label lab) v -> do
       initial <- getDynamic v
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  void (setDynamic v isChecked)
              ]
       listenWith v (\_ isChecked -> set cb [ checked := isChecked ])
       pure (widget cb)

     TextBox (Label lab) v -> do
       initial <- getDynamic v
       te <- textEntry p [ text := initial
                         , processEnter := True
                         , tooltip := ""
                         ]
       normalBG <- get te bgcolor
       set te [ on command := do
                  newText <- get te text
                  setDynamic v newText >>= \case
                    Nothing -> set te [ bgcolor := normalBG
                                      , tooltip := "" ]
                    Just err -> do
                      set te [ bgcolor := rgb 160 100 (100 :: Int)
                             , tooltip := unlines
                                 [ "Could not parse an expression"
                                 , ""
                                 , show err ]
                             ]
              , on focus := (\case
                                True -> pure ()
                                False -> do
                                  newText <- get te text
                                  oldText <- getDynamic v
                                  when (newText /= oldText) $ setDynamic v newText >>= \case
                                    Nothing -> set te [ bgcolor := normalBG
                                                      , tooltip := "" ]
                                    Just err -> do
                                      set te [ bgcolor := rgb 160 100 (100 :: Int)
                                             , tooltip := unlines
                                               [ "Could not parse an expression"
                                               , ""
                                               , show err ]
                                             ])
              ]
       listenWith v (\_ newText -> set te [ text := newText ])
       pure (fill $ row 5 [ margin 3 (label lab), hfill (widget te) ])

renderTile' :: Valued w
            => IORef Int
            -> (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ())
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    modelRect <- modelToRect dim <$> get model value
    let action' p q r x y c = do
            curId <- readIORef renderId
            if (curId == iD) then action p q r x y c else pure ()
    renderTile action' dim modelRect

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

selectRegion :: Rectangle Viewport -> IO ()
selectRegion r = do
    putStrLn $ "selected region " ++ show r
    return ()

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

paintToolLayer' :: ((Double, Double) -> IO Point)
               -> (Double, Double)
               -> IO [[DrawCommand]]
               -> Var (Maybe Viewport)
               -> Var (Maybe Viewport)
               -> DC d
               -> Rect
               -> Rect
               -> IO ()
paintToolLayer' modelToView pxDim getDrawCommands lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    paintToolLayer modelToView pxDim getDrawCommands dc
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba @Word8 0 128 255 128) white boxPts

paintToolLayer :: ((Double, Double) -> IO Point)
               -> (Double, Double)
               -> IO [[DrawCommand]]
               -> DC d
               -> IO ()
paintToolLayer modelToView pxDim getDrawCommands dc = dcEncapsulate dc $ do

    cmdss <- getDrawCommands
    let initialPenColor = rgba 255 255 255 (255 :: Word8)
        initialBrushColor = rgba 255 255 255 (255 :: Word8)
    currentPen <- newIORef initialPenColor
    currentBrush <- newIORef initialBrushColor

    let withPen fil action = do
          curPen <- (`penColored` 2) <$> readIORef currentPen
          curBrush <- (if fil then brushSolid else const brushTransparent) <$> readIORef currentBrush
          dcWithPenStyle dc curPen $ dcWithBrushStyle dc curBrush $ action []
        pxSz = sqrt(fst pxDim * snd pxDim)

    forM_ cmdss $ \cmds -> do
      -- Reset the pen and brush for the next drawing layer
      writeIORef currentPen initialPenColor
      writeIORef currentBrush initialBrushColor

      forM_ cmds $ \case

        DrawPoint _ pt -> do
          pt' <- modelToView pt
          withPen True (circle dc pt' 2)

        DrawLine _ pt1 pt2 -> do
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          withPen False (line dc pt1' pt2')

        DrawCircle _ fil r pt -> do
          pt' <- modelToView pt
          let r' = round (r / pxSz)
          withPen fil (circle dc pt' r')

        DrawRect _ fil pt1 pt3 -> do
          let pt2 = (fst pt1, snd pt2)
              pt4 = (snd pt1, fst pt2)
          pt1' <- modelToView pt1
          pt2' <- modelToView pt2
          pt3' <- modelToView pt3
          pt4' <- modelToView pt4
          let boxpts = [pt1', pt2', pt3', pt4']
          withPen fil (polygon dc boxpts)

        Clear {} -> pure () -- clear operations should be handled upstream, by truncating the draw command list
        SetStroke _ c -> writeIORef currentPen (fsColorToWxColor c)

        SetFill _ c -> writeIORef currentBrush (fsColorToWxColor c)

fsColorToWxColor :: Color -> WX.Color
fsColorToWxColor c =
  let (r,g,b) = colorToRGB c in rgba r g b 255
