{-# language OverloadedStrings, UndecidableInstances, NumericUnderscores #-}
{-# options_ghc -Wno-orphans #-}

module UI.ProjectViewer
  ( viewProject
  , ProjectMenuActions(..)
  ) where

import FractalStream.Prelude hiding (get)

import Data.Color (Color, colorToRGB, grey)
import Control.Concurrent.MVar
import Control.Exception (try, throwIO, SomeException, SomeAsyncException, ErrorCall(..), fromException, displayException)
import System.IO.Error (isUserError, ioeGetErrorString)

import Graphics.UI.WX hiding ((#), grey, pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
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
import UI.Widgets
import UI.WxProperties

import Actor.Viewer.Preview (renderPreview)
import Data.ByteString (ByteString)

import Data.IORef

import Actor.UI
import Actor.Tool
import Actor.Layout
import Actor.Configuration
import Actor.Viewer
import Actor.Ensemble (buildToolContext, layoutToArgs)
import Actor.Event (Event(..), EventArgument_, EventArgument(..))
import Language.Type
import Language.Draw
import Language.Environment
import Language.Code.InterpretIO
import Language.Value.Evaluator (HaskellValue)

import Data.DynamicValue hiding (clone)
import Foreign.Ptr (Ptr)

import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Char (toLower)
import qualified System.Info as System

-- | What a viewer's menu can do to the project as a whole.
data ProjectMenuActions = ProjectMenuActions
  { pmSave          :: IO ()
  , pmEditScript    :: IO ()
  , pmEditInfo      :: Window () -> IO ()
    -- ^ Show the project info dialog, over the given window. That must
    -- be a visible window, such as the viewer it was opened from -- not
    -- the session's hidden project window, which wx would show again
    -- once the dialog closed.
  , pmSetPreview    :: ByteString -> IO ()
    -- ^ Make this PNG image the project's preview.
  }

viewProject :: Window ()
            -> WxLayout
            -> (forall a. Frame a -> [Menu ()] -> IO ())
            -> ProjectMenuActions
            -> UI
viewProject projectWindow wxLayout addMenuBar projectActions = UI
  { newEnsemble = pure ()

  , makeLayout = \_ title ui -> do
      f <- frameEx frameDefaultStyle [ text := title
                 , on resize := propagateEvent
                 , style := wxFRAME_TOOL_WINDOW .+. wxRESIZE_BORDER .+.
                            wxCAPTION .+. wxCLOSE_BOX
                 , visible := False
                 ] projectWindow

      WX.windowOnClose f (set f [ visible := False ])

      (restoredConfig, clampConfig) <- restoreConfigWindow wxLayout
      forM_ restoredConfig $ \WindowProperties{..} -> do
        Point x0 y0 <- get f position
        set f [ position := Point (fromMaybe x0 wpX) (fromMaybe y0 wpY) ]
      forgetConfigWindow <- registerConfigWindow wxLayout $ do
        Point x y <- get f position
        pure (WindowProperties (Just x) (Just y))

      addMenuBar f []

      (stopListening, innerLayout) <- generateWxLayout (\_ -> pure ()) f ui
      set f [ layout := fill . margin 5 . column 5 $ [ innerLayout ] ]
      -- Hooks `windowOnDestroy`, not `on closing` -- see the note on
      -- 'UI.Widgets.wxWatchDynamic'.
      previousOnDestroy <- WX.windowGetOnDestroy f
      WX.windowOnDestroy f (forgetConfigWindow >> stopListening >> previousOnDestroy)
      windowReLayout f
      when clampConfig (keepOnPrimaryDisplay f)
      pure (windowShow f >> windowRaise f)

  , makeViewer = const (makeWxComplexViewer projectWindow wxLayout addMenuBar projectActions)
  }

data BuiltinTool = NavTool | DebugTool

makeWxComplexViewer :: Window ()
                    -> WxLayout
                    -> (forall a. Frame a -> [Menu ()] -> IO ())
                    -> ProjectMenuActions
                    -> IO ()
                    -> Dynamic (Either String SomeEnvironment)
                    -> SomeContext EventArgument_
                    -> IO ()
                    -> Viewer
                    -> IO (IO ())
makeWxComplexViewer projectWindow wxLayout addMenuBar ProjectMenuActions{..} raiseConfigWindow configEnv (SomeContext configValues) remakeViewer theViewer@Viewer{..} = do

    {- let clone = cloneViewer theViewer >>= void . makeWxComplexViewer projectWindow addMenuBar saveSessi         on raiseConfigWindow (SomeContext configValues)
    -}
    let baseFontSize = case System.os of
          "darwin" -> 14
          _        -> 10

    -- Pick up wherever this viewer's window was last left, if that was
    -- remembered: its size, position, and current view all replace the
    -- defaults the viewer was built with.
    title0 <- getDynamic vTitle
    (restored, clampViewer) <- restoreViewer wxLayout title0
    forM_ restored $ \ViewerProperties{..} -> do
      Dimensions (w0, h0) <- getDynamic vSize
      setValue' vSize (Dimensions (fromMaybe w0 vpWidth, fromMaybe h0 vpHeight))
      Dimensions (x0, y0) <- getDynamic vPosition
      setValue' vPosition (Dimensions (fromMaybe x0 vpX, fromMaybe y0 vpY))
      mapM_ (setValue' vCenter) vpCenter
      mapM_ (setValue' vPixelSize) vpPixelSize
    let canResize = fromMaybe vCanResize (restored >>= vpResizable)
        startVisible = fromMaybe True (restored >>= vpVisible)
        frameStyle | canResize = frameDefaultStyle
                   | otherwise = frameDefaultStyle .-. wxRESIZE_BORDER .-. wxMAXIMIZE_BOX

    Dimensions (width, height) <- getDynamic vSize
    Dimensions (xpos, ypos) <- getDynamic vPosition
    f <- frameEx frameStyle [ text := title0
                                   , position := Point xpos ypos
                                   , on resize := propagateEvent
                                   ] projectWindow
    WX.windowOnClose f (set f [ visible := False ])

    -- A status bar, for reporting problems the viewer's script ran into
    -- while rendering. It's hidden while there's nothing to report.
    statusText <- statusField [ text := "" ]
    set f [ statusBar := [statusText] ]
    statusBarWindow <- frameGetStatusBar f
    void (windowHide statusBarWindow)

    -- Show a message in the status bar, or hide the bar for an empty
    -- message. The frame grows or shrinks to fit the bar, so that the
    -- image keeps its size and doesn't need to be re-rendered.
    let setStatusMessage msg = do
          shown <- get statusText text
          when (msg /= shown) $ do
            set statusText [ text := msg ]
            isShown <- windowIsShown statusBarWindow
            when (isShown == null msg) $ do
              oldSize <- get f clientSize
              void (if null msg then windowHide statusBarWindow
                                else windowShow statusBarWindow)
              set f [ clientSize := oldSize ]

    p <- scrolledWindow f [ scrollRate := sz 10 10, visible := True ]

    (rememberViewer, forgetViewer) <- registerViewer wxLayout title0 $ do
      Dimensions (x, y) <- getDynamic vPosition
      Dimensions (w, h) <- getDynamic vSize
      viewCenter <- getDynamic vCenter
      pixelSize <- getDynamic vPixelSize
      shown <- windowIsShown f
      -- Leave out what's the same as the default anyway, so that a
      -- saved project file only says what's special about each window.
      pure ViewerProperties
        { vpX = Just x, vpY = Just y, vpWidth = Just w, vpHeight = Just h
        , vpResizable = if canResize then Nothing else Just False
        , vpVisible = if shown then Nothing else Just False
        , vpCenter = Just viewCenter, vpPixelSize = Just pixelSize
        }

    wxWatchDynamic f configEnv $ \_ -> do
      putStrLn "remake"
      rememberViewer
      void $ windowClose f True
      remakeViewer

    -- HUD
    let isDarkMode = True
        overlayBackground = if isDarkMode then rgba 0 0 0 (80 :: Int) else rgba 255 255 255 (180 :: Int)
        overlayForeground = if isDarkMode then white else black

    pointerLocation <- panel f [ bgcolor := overlayBackground
                               , position := Point (width `div` 2 - 155) (height - 30)
                               , visible := False
                               ]
    shouldShowPointerLocation <- variable [ value := False ]
    plp <- panel pointerLocation [ bgcolor := rgba 0 0 0 (0 :: Int) ]
    pointerLocationText <- staticText plp [ text := ""
                                          , textColor := overlayForeground
                                          , font := fontFixed
                                          , fontSize := baseFontSize
                                          , fontWeight := WeightBold
                                          ]
    set pointerLocation [ layout := minsize (sz 310 20) $ floatCentre $ container plp $
                          widget pointerLocationText
                        ]

    let setPointerLocation (x,y) =
          let txt0
                | y < 0     = printf "%.14f - %.14f𝑖" x (negate y)
                | otherwise = printf "%.14f + %.14f𝑖" x y
              txt = if x < 0 then txt0 else ' ' : txt0
          in set pointerLocationText [ text := txt ]

    currentTool <- variable [value := Left NavTool ]
    -- Where the left mouse button went down, and where it has been dragged
    -- to since; both are Nothing when there's no click in progress.
    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]
    toolPanel <- panel f [ bgcolor := overlayBackground
                         , position := Point 5 5 ]
    toolpick <- staticText toolPanel [ text := "▼ Navigate", textColor := overlayForeground ]
    toolPickerMenu <- menuPane [ text := "Tool" ]
    set toolPanel [ layout := margin 5 (widget toolpick) ]
    windowFit toolPanel

    set toolpick [ on click := \(Point px py) -> do
                      Point wx wy <- get toolPanel position
                      menuPopup toolPickerMenu (Point (px + wx) (py + wy)) f
                  ]
    warningPanel <- panel f [ bgcolor := rgba 255 0 0 (50 :: Int)
                            , position := Point 250 5
                            , visible := False ]

    warning <- staticText warningPanel [ text := "Viewer paused"
                                       , color := white ]
    set warningPanel [ layout := fill $ margin 5 $ widget warning ]

    hamburgerPanel <- panel f [ bgcolor := overlayBackground
                              , position := Point (width - 30) 5 ]
    hp <-  panel hamburgerPanel []
    hamburgerMenu <- menuPane [ text := "Viewer" ]
    hamburger <- staticText hp [ text := "☰"
                               , textColor := overlayForeground
                               , fontSize := baseFontSize + 6
                               , size := Size 20 20
                               ]
    set hamburger [ on click := \(Point px py) -> do
                      Point wx wy <- get hamburgerPanel position
                      menuPopup hamburgerMenu (Point (px + wx) (py + wy)) f ]

    set hamburgerPanel [ layout := minsize (sz 25 25) $ container hp $ alignCentre $ widget hamburger ]

    shouldShowToolPicker <- variable [value := True]

    let repositionButtons = do
          Size w h <- get f clientSize
          set pointerLocation [ position := Point (w `div` 2 - 155) (h - 30) ]
          set hamburgerPanel [ position := Point (w - 30) 5 ]
          Size ww wh <- get warningPanel outerSize
          set warningPanel [ position := Point ((w - ww) `div` 2) ((h - wh) `div` 2) ]

    set f [ layout := fill $ widget p
          , clientSize := sz width height
          , on activate :~ \old vis -> do
              shouldShowTools <- get shouldShowToolPicker value
              set toolPanel [ visible := vis && shouldShowTools ]
              set hamburgerPanel [ visible := vis ]
              showPtr <- get shouldShowPointerLocation value
              set pointerLocation [ visible := vis && showPtr ]
              old vis
              propagateEvent
          ]
    windowReLayout f
    when clampViewer (keepOnPrimaryDisplay f)

    -- A viewer whose window was closed when the project was saved

    -- stays closed (just hidden, as closing it does) until asked for.
    unless startVisible (set f [ visible := False ])


    -- `requestRefresh` can be used from any thread to queue up a
    -- window refresh.
    offThreadRefresh <- newEmptyMVar
    let requestRefresh = void (tryPutMVar offThreadRefresh ())

    -- Build the initial view model
    (initialX, initialY) <- getDynamic vCenter
    initialPx <- getDynamic vPixelSize

    let initialModel = Model (initialX, initialY) (initialPx, initialPx)
    model <- variable [value := initialModel]

    -- History tracking
    history <- newIORef (History [] initialModel [])

    -- trigger repaint
    let triggerRepaint = do
          isVisible <- get f visible
          when isVisible $ do
            repaint p
            windowRefresh p True -- True=redraw background
            windowUpdateWindowUI p

    -------------------------------------------------------
    -- Make the debugger window
    -------------------------------------------------------
    debugger <- frameTool [ text := "Debug viewer script"
                          , visible := False
                          , style := wxFRAME_TOOL_WINDOW .+. wxRESIZE_BORDER .+. wxCAPTION .+. wxCLOSE_BOX
                          ] f
    WX.windowOnClose debugger (set debugger [ visible := False ])
    debugPanel <- panel debugger []
    debugResult <- textCtrl debugPanel
      [ text := "Click in the viewer to see the final value of each variable" ]
    set debugger [ layout := fill $ container debugPanel $ margin 5 $ expand $ widget debugResult ]

    let debugAt (x, y) (dx, dy) = case vCodeWithArgs of
          CodeWithArgs getArgs mcode _ -> case mcode of
            Nothing -> set debugResult [ text := "(no script available)" ]
            Just code -> getArgs >>= \case
              Left err -> set debugResult [ text := "(cannot access arguments due to error: " ++
                                            err ++ ")"]
              Right args -> do
                let action = interpretToIOWithLastValues (DrawHandler $ \_ -> pure ()) code
                ctx0 <- forContextM @HaskellValue @IORefTypeOfBinding args (\_ _ -> newIORef)
                vx  <- newIORef x
                vy  <- newIORef y
                vdx <- newIORef dx
                vdy <- newIORef dy
                vc  <- newIORef grey
                let ctx = vx # vy # vdx # vdy # vc # ctx0
                m0 <- Map.fromList <$> fromContextM (\n t v ->
                                                       (symbolVal n,) . SomeHaskellType t <$>
                                                       readIORef v) ctx
                set debugResult [ text := "(working...)" ]
                m <- fst <$> execStateT action (m0, ctx)
                let table = case Map.null m of
                      True  -> "(no variables were assigned to)"
                      False -> unlines [ printf "%s : %s = %s" k (ppType t) (show v)
                                       | (k, v@(SomeHaskellType t _)) <- Map.toList m ]
                set debugResult [ text := unlines
                 [ "  Final values"
                 , "------------"
                 , table ]]

    -------------------------------------------------------
    -- Set up the tools
    -------------------------------------------------------
    -- Get the tools

    -- The timers of the selected tool's `on timer` handlers, which run
    -- only while it is selected.
    runningTimers <- newIORef []
    -- The selected tool's own field values as of its last refresh, so that
    -- editing them can trigger another one.
    lastToolFields <- newIORef Nothing
    let stopToolTimers = do
          timers <- readIORef runningTimers
          writeIORef runningTimers []
          forM_ timers $ \t -> set t [ enabled := False ]

    let getToolEventHandler :: Tool -> IO (Double -> Actor.Event.Event -> Maybe (IO ()))
        getToolEventHandler Tool{..} = getDynamic (dyn toolEventHandler)

        -- Run one of a tool's event handlers, returning whether it
        -- succeeded. If it fails, switch back to the Navigate tool *before*
        -- reporting the error, so that a tool which fails on every event
        -- (each step of a drag, or every refresh) can't produce an endless
        -- stream of error dialogs. The failed tool's Deactivated handler
        -- isn't run, since it may well fail too.
        guardTool :: Tool -> IO () -> IO Bool
        guardTool Tool{..} action = try action >>= \case
          Right () -> pure True
          Left (err :: SomeException)
            | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
            | otherwise -> do
                stopToolTimers
                set currentTool [ value := Left NavTool ]
                -- If it failed partway through a click or drag, forget it,
                -- so that releasing the mouse doesn't zoom or recenter.
                set lastClick [ value := Nothing ]
                set draggedTo [ value := Nothing ]
                set toolpick [ text := "▼ Navigate" ]
                showToolConfig <- getDynamic (dyn toolShowConfig)
                showToolConfig False
                vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
                errorDialog f "Tool error" $ concat
                  [ "The \"", tname, "\" tool stopped because of an error:\n\n"
                  , describeToolError err
                  , "\n\nSwitching back to the Navigate tool." ]
                pure False

        runToolEventHandler theTool e = do
          handle <- getToolEventHandler theTool
          Model _ (px, _) <- get model value
          maybe (pure True) (guardTool theTool) (handle px e)

    let activateTool thisTool@Tool{..} = do
          deactivateCurrentTool
          tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
          set toolpick [ text := "▼ " ++ tname]
          windowFit toolPanel
          set currentTool [ value := Right thisTool ]
          showToolConfig <- getDynamic (dyn toolShowConfig)
          showToolConfig True
          activated <- runToolEventHandler thisTool Activated
          shouldRefresh <- getDynamic toolRefreshOnActivate
          refreshed <- if activated && shouldRefresh
                         then runToolEventHandler thisTool Refresh
                         else pure activated
          vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
          writeIORef lastToolFields . Just =<< toolFieldValues thisTool
          -- Start the tool's timers, unless it already failed.
          when refreshed $ do
            timers <- getDynamic toolTimers
            ts <- forM timers $ \(name, ms) ->
              wxTimer f [ interval := max 10 ms
                        , enabled := True
                        , on command := do
                            void $ runToolEventHandler thisTool (Actor.Event.Timer name)
                            vDrawCmdsChanged >>= \tf -> when tf triggerRepaint ]
            writeIORef runningTimers ts

        activateNavTool = do
          deactivateCurrentTool
          set toolpick [ text := "▼ Navigate" ]
          set currentTool [ value := Left NavTool ]

        activateDebugTool = do
          deactivateCurrentTool
          set toolpick [ text := "▼ Debug" ]
          set debugger [ visible := True ]
          windowRaise f
          focusOn p
          set currentTool [ value := Left DebugTool ]

        deactivateCurrentTool = stopToolTimers >> get currentTool value >>= \case
          Left NavTool -> pure ()
          Left DebugTool -> set debugger [ visible := False ]
          Right oldTool -> do
            void $ runToolEventHandler oldTool Deactivated
            showToolConfig <- getDynamic (dyn $ toolShowConfig oldTool)
            showToolConfig False
            vDrawCmdsChanged >>= \tf -> when tf triggerRepaint

        setupToolPicker tools0 = do
          -- TODO FIXME remove all menu items first
          void $ menuItem toolPickerMenu [ text := "Navigate\tn"
                                         , on command := activateNavTool
                                         ]
          forM_ tools0 $ \thisTool@Tool{..} -> do
            tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
            shortcut <- getDynamic (tiShortcut toolInfo)
            let shorty = if null shortcut then "" else ("\t" ++ shortcut)
            menuItem toolPickerMenu [ text := tname ++ shorty
                                    , on command := activateTool thisTool ]

          void $ menuItem toolPickerMenu [ text := "Debug\t?"
                                         , on command := activateDebugTool
                                         ]

    wxWatchDynamic1 f vTools setupToolPicker

    toolShortcuts <- fmap Map.unions $ do
      tools <- getDynamic vTools
      forM tools $ \tool -> getDynamic (tiShortcut $ toolInfo tool) <&> \case
        (c : "") -> Map.singleton (toLower c) (activateTool tool)
        _        -> Map.empty

    -------------------------------------------------------
    -- Main view
    -------------------------------------------------------

    lastRenderAction <- newMVar (\_ _ _ _ _ _ -> pure mempty)

    let getRenderAction = case vCodeWithArgs of
          CodeWithArgs vGetArgs _ vCode -> do
            vGetArgs >>= \case
              Left err   -> do
                set warningPanel [ visible := True ]
                set warning [ tooltip := err ]
                readMVar lastRenderAction
              Right vaArgs -> do
                set warningPanel [ visible := False ]
                ViewerFunction vf <- getDynamic vCode
                let action = \(w :: Word32) (h :: Word32) (subsamples :: Word32) (dx :+ dy) (x :+ y) vaBuffer -> do
                      let vaWidth  = fromIntegral w
                          vaHeight = fromIntegral h
                          vaSubsamples = fromIntegral subsamples
                          vaPoint = (x, y)
                          vaStep  = (dx, dy)
                      vf ViewerArgs{..}
                modifyMVar_ lastRenderAction (\_ -> pure action)
                pure action

    renderId <- newIORef (0 :: Int)
    -- The issues reported so far by the current render, and its id.
    renderIssues <- newIORef (0 :: Int, mempty :: ViewerIssues)

    pendingResize <- variable [value := False]

    viewerTile     <- do
      renderAction <- getRenderAction
      renderTile' renderId renderIssues True renderAction (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]
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

        jumpViewTo newModel@(Model newCenter (newPixelSize, _)) = do
          oldModel <- get model value
          Size { sizeW = w, sizeH = h } <- get f clientSize
          set model [ value := newModel ]
          setValue' vCenter    newCenter
          setValue' vPixelSize newPixelSize
          get currentTile value >>= cancelTile
          renderAction <- getRenderAction
          smoothing <- get useSmoothing value
          newViewerTile <- renderTile' renderId renderIssues smoothing renderAction (w, h) model
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

                get currentTool value >>= \case
                  Left NavTool -> paintToolLayerWithDragBox (modelToView mdl) (modelPixelDim mdl) vDrawCmds lastClick draggedTo dc r viewRect
                  _  -> paintToolLayer (modelToView mdl) (modelPixelDim mdl) vDrawCmds dc
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
                paintToolLayer (modelToView midModel) (modelPixelDim midModel) vDrawCmds dc

                -- Flush the graphics context
                graphicsContextDelete gc
          ]

    lastKnownMouse <- newIORef Nothing

    -- Set click and drag event handlers
    set p [ on mouse   := \event -> get lastClick value >>= \lastClickNow -> case event of
              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                propagateEvent

              MouseLeftUp _ _ | Nothing <- lastClickNow ->
                -- No click in progress (e.g. a tool failed partway through
                -- one), so there's nothing to finish.
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers -> do
                dragBox <- getDragBox lastClick draggedTo
                let recenterAction = do
                      -- Completed a click, recenter to the clicked point.
                      oldModel <- get model value
                      newCenter <- viewToModel pt
                      changeViewTo oldModel { modelCenter = toCoords newCenter }
                ptz <- viewToModel pt
                toolClickAction <- get currentTool value <&> \case
                  Left NavTool -> recenterAction
                  Left DebugTool -> do
                    (x, y) <- viewToModel pt
                    Model _ (dx, dy) <- get model value
                    debugAt (x, y) (dx, dy)
                  Right tool -> do
                    Model _ (px, _) <- get model value
                    handle <- getToolEventHandler tool
                    case handle px (Click ptz) of
                      Nothing -> recenterAction
                      Just action -> do
                        void (guardTool tool action)
                        vDrawCmdsChanged >>= \tf -> when tf triggerRepaint

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
                      get currentTool value >>= \case
                        Right tool -> do
                          get lastClick value >>= \case
                            Nothing -> pure ()
                            Just (Viewport vstart) -> do
                              zstart <- viewToModel (uncurry Point vstart)
                              z <- viewToModel pt
                              Model _ (px, _) <- get model value
                              handle <- getToolEventHandler tool
                              case handle px (DragDone z zstart) of
                                Just action -> do
                                  void (guardTool tool action)
                                  vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                                Nothing -> finishDrag
                        Left NavTool -> finishDrag
                        Left DebugTool -> pure ()

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                setPointerLocation mpt
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                let dragAction = do
                      dragBox <- getDragBox lastClick draggedTo
                      case dragBox of
                        Nothing -> return ()
                        Just _  -> triggerRepaint

                      propagateEvent

                get currentTool value >>= \case
                  Left NavTool -> dragAction
                  Left DebugTool -> pure ()
                  Right tool -> do
                     get lastClick value >>= \case
                       Nothing -> pure ()
                       Just (Viewport vstart) -> do
                         zstart <- viewToModel (uncurry Point vstart)
                         z <- viewToModel pt
                         Model _ (px, _) <- get model value
                         handle <- getToolEventHandler tool
                         case handle px (Drag z zstart) of
                           Just action -> do
                             void (guardTool tool action)
                             vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                           Nothing -> dragAction
                     propagateEvent


              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                writeIORef lastKnownMouse (Just mpt)
                setPointerLocation mpt
                propagateEvent

              MouseLeftDClick pt modifiers | isNoShiftAltControlDown modifiers -> do
                get currentTool value >>= \case
                   Left _ -> pure ()
                   Right tool -> do
                     z <- viewToModel pt
                     void $ runToolEventHandler tool (DoubleClick z)
                     vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
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
    _ <- wxTimer f [ interval := 100
                   , enabled := True
                   , on command := do
                       curTile <- get currentTile value
                       ifModified curTile $ do
                         viewRect <- windowGetViewRect f
                         tileImage <- generateTileImage curTile viewRect
                         saved <- imageCopy tileImage
                         set savedTileImage [value := Just saved]
                         triggerRepaint
                       -- Report the current render's issues, if any. The
                       -- previous message stays up until the render either
                       -- finds an issue or finishes, so that the bar doesn't
                       -- flicker away at the start of every render.
                       (_, issues) <- readIORef renderIssues
                       finished <- tileFinished curTile
                       when (finished || issues /= mempty) $
                         setStatusMessage (fromMaybe "" (describeViewerIssues issues))
                   ]

    -- Add a timer which will check for repainting requests from off the main
    -- UI thread, ~10Hz
    _ <- wxTimer f [ interval := 100
                   , enabled := True
                   , on command := do
                       needRefresh <- (== Just ()) <$> tryTakeMVar offThreadRefresh
                       when needRefresh (changeViewTo =<< get model value)
                   ]

    -- Animation timer. At ~65Hz, check if we are animating between
    -- two views. If so, step the animation and repaint.
    _ <- wxTimer f [ interval := 16
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
    onResizeTimer <- wxTimer f [ interval := 100
                               , enabled := False ]
    set onResizeTimer [ on command := do
                              set onResizeTimer [enabled := False] -- one-shot
                              needResize <- get pendingResize value
                              when needResize $ do
                                  set pendingResize [value := False]
                                  -- Showing or hiding the status bar resizes
                                  -- the frame but not the image; only
                                  -- re-render if the image's size changed.
                                  Size w h <- get f clientSize
                                  tileSize <- tileRect <$> get currentTile value
                                  when (tileSize /= (w, h)) $
                                    jumpViewTo =<< get model value
                      ]

    -- Add a timer which checks if the tool layer should receive a refresh
    -- event: when the view changed, or the tool's own fields did. Changes
    -- the refresh handler itself makes to those fields don't count, unless
    -- the tool says they should (refreshCanUpdate=true, e.g. to animate).
    _ <- wxTimer f [ interval := 50
                   , enabled := True
                   , on command := do
                       needToSendRefresh <- isJust <$> tryTakeMVar needToSendRefreshEvent
                       get currentTool value >>= \case
                         Left _ -> pure ()
                         Right tool -> do
                           fields <- toolFieldValues tool
                           lastFields <- readIORef lastToolFields
                           when (needToSendRefresh || lastFields /= Just fields) $ do
                             void $ runToolEventHandler tool Refresh
                             vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                             canUpdate <- getDynamic (toolRefreshCanUpdate tool)
                             fields' <- if canUpdate then pure fields else toolFieldValues tool
                             writeIORef lastToolFields (Just fields')
                   ]

    set f [ on resize := do
              set onResizeTimer [enabled := False]
              set pendingResize [value   := True]
              set onResizeTimer [enabled := True]
              repositionButtons
              propagateEvent ]

    -- wxHaskell doesn't seem to expose an "on move" event, so we'll just set up
    -- a timer to update the viewer's position every so often.
    _ <- wxTimer f [ interval := 200, enabled := True
                   , on command := do
                       Point xpos' ypos' <- get f position
                       setValue vPosition (Dimensions (xpos', ypos'))
                   ]

    set p [ on resize := do
              Size w h <- get p clientSize
              setValue' vSize (Dimensions (w, h))
              propagateEvent ]

    -------------------------------------------------------
    -- Change tracking
    -------------------------------------------------------

    -- For each variable that the viewer code depends on, trigger a repaint whenever
    -- that variable changes.
    stopListening' <- onParameterChanges theViewer requestRefresh
    -- Hooks `windowOnDestroy`, not `on closing` -- `f` is exactly the
    -- viewer frame a rebuild tears down with `windowDestroy`, which
    -- never fires `closing` at all; see the note on
    -- 'UI.Widgets.wxWatchDynamic'.
    do previousOnDestroy <- WX.windowGetOnDestroy f
       WX.windowOnDestroy f (forgetViewer >> stopListening' >> previousOnDestroy)

    -------------------------------------------------------
    -- Navigation actions
    -------------------------------------------------------
    let goBack = (historyBack <$> readIORef history) >>= \case
          Nothing -> wxcBell
          Just (h', m') -> do
            writeIORef history h'
            jumpViewTo m'
        goForward = (historyForward <$> readIORef history) >>= \case
          Nothing -> wxcBell
          Just (h', m') -> do
            writeIORef history h'
            jumpViewTo m'
        -- Back to the view the script itself describes, which needn't
        -- be where this window started out if a saved view was restored
        -- over it. Recorded as a new step in the history, like any other
        -- move, so "Last view" can still undo it.
        goHome = changeViewTo
          (Model vOriginalCenter (vOriginalPixelSize, vOriginalPixelSize))

        jumpToView = do
          oldModel <- get model value
          let (oldX, oldY) = modelCenter oldModel
              (oldPx, _)   = modelPixelDim oldModel
          d <- dialog f [ text := "Jump to view..." ]
          jp <- panel d []
          jOk <- button jp [ text := "Ok" ]
          jCancel <- button jp [ text := "Cancel" ]
          jCenterX <- textEntry jp [ text := printf "%0.14f" oldX ]
          jCenterY <- textEntry jp [ text := printf "%0.14f" oldY ]
          jPixel <- textEntry jp [ text := printf "%0.14f" oldPx]
          set d [ layout := margin 10 $ fill $ container jp $ column 5
                  [ row 5 [ label "x = ", expand $ widget jCenterX
                          , hglue, label "y = ", expand $ widget jCenterY ]
                  , row 5 [ label "pixel size = ", expand $ widget jPixel ]
                  , row 5 [ margin 5 $ widget jCancel, hglue, margin 5 $ widget jOk ]]]
          result <- showModal d $ \onDone -> do
            set jCancel [ on command := onDone Nothing ]
            set jOk [ on command := do
                        mx <- readMaybe <$> get jCenterX text
                        my <- readMaybe <$> get jCenterY text
                        mp <- readMaybe <$> get jPixel   text
                        onDone ((\(x,y,dz) -> Model (x, y) (dz, dz)) <$>
                                 ((,,) <$> mx <*> my <*> mp)) ]
          case result of
            Nothing -> pure ()
            Just newModel -> changeViewTo newModel

    -------------------------------------------------------
    -- Menus
    -------------------------------------------------------

    -- Viewer menu
    menuItem hamburgerMenu [ text := "Last view\t<", on command := goBack ]
    menuItem hamburgerMenu [ text := "Next view\t>", on command := goForward ]
    menuItem hamburgerMenu [ text := "Jump to view...\t@", on command := jumpToView ]
    menuItem hamburgerMenu [ text := "Original view\t^", on command := goHome ]
    menuItem hamburgerMenu [ text := "Show configuration\t=", on command := raiseConfigWindow ]
    set p [ on keyboard :~ \old k -> do
              let mods = keyModifiers k
              if not (altDown mods) && not (controlDown mods) && not (metaDown mods)
                then case keyKey k of
                       KeyChar '<' -> goBack
                       KeyChar '>' -> goForward
                       KeyChar '^' -> goHome
                       KeyChar '@' -> jumpToView
--                       KeyChar '2' -> clone
                       KeyChar '=' -> raiseConfigWindow
                       KeyChar 'n' -> activateNavTool
                       KeyChar 'N' -> activateNavTool
                       KeyChar '?' -> activateDebugTool
                       KeyChar c   -> Map.findWithDefault (old k) (toLower c) toolShortcuts
                       _ -> old k
                else old k
          ]
--    menuItem hamburgerMenu [ text := "Clone viewer\t2", on command := clone ]
    menuLine hamburgerMenu
    menuItem hamburgerMenu [ text := "Take a picture..."
                           , on command := takePicture f theViewer ]
    menuItem hamburgerMenu [ text := "Use this as preview"
                           , on command := renderPreview False theViewer >>= \case
                               Left err -> errorDialog f "Couldn't make a preview" err
                               Right png -> pmSetPreview png ]
    menuLine hamburgerMenu
    menuLine hamburgerMenu
    showToolPicker  <- menuItem hamburgerMenu [ text := "Show tool picker"
                                              , checkable := True, checked := True ]
    showPtrPosition <- menuItem hamburgerMenu [ text := "Show pointer position"
                                              , checkable := True, checked := False ]
    menuLine hamburgerMenu
    menuItem hamburgerMenu [ text := "Save...\tCtrl+S", enabled := True, on command := pmSave ]
    menuItem hamburgerMenu [ text := "Edit project script...\tCtrl+Shift+E"
                           , enabled := True
                           , on command := pmEditScript
                           ]
    menuItem hamburgerMenu [ text := "Project info...", on command := pmEditInfo (objectCast f) ]


    menuLine hamburgerMenu
    smoothingSelection <- menuItem hamburgerMenu [ text := "Use subpixel smoothing"
                                                 , checkable := True
                                                 , checked := True
                                                 ]
    set smoothingSelection [
      on command := do
          isChecked <- get smoothingSelection checked
          set useSmoothing [ value := isChecked ]
          requestRefresh
      ]

    set showToolPicker [ on command := do
                           isChecked <- get showToolPicker checked
                           set shouldShowToolPicker [ value := isChecked ]
                           set toolPanel [ visible := isChecked ]
                       ]

    set showPtrPosition [ on command := do
                            isChecked <- get showPtrPosition checked
                            set shouldShowPointerLocation [ value := isChecked ]
                            set pointerLocation [ visible := isChecked ]
                        ]

    -- Build the configuration window and the event handlers for each tool
    tools <- getDynamic vTools
    forM_ tools $ \Tool{..} -> do
      layer <- getDynamic toolDrawLayer
      getDynamic toolConfig >>= \case
        Nothing -> do
          getDynamic (dyn toolScript) >>= (vBuildToolHandler layer (SomeContext configValues)) >>= \case
            Left err -> do
              n <- getDynamic (source $ tiName toolInfo)
              putStrLn ("Problem with tool " ++ n ++ ": " ++ err)
              setValue' toolEventHandler (\_ _ -> Nothing)
            Right h -> setValue' toolEventHandler h
        Just tcfg -> do
          n <- getDynamic (source $ tiName toolInfo)
          tf <- frameTool [ text := n
                          , visible := False
                          , style := wxFRAME_TOOL_WINDOW .+. wxRESIZE_BORDER .+. wxCAPTION .+. wxCLOSE_BOX
                          ] f
          WX.windowOnClose tf (set tf [ visible := False ])
          let buttonPress txt = do
                void $ runToolEventHandler Tool{..} (ButtonPressed txt)
                vDrawCmdsChanged >>= \yn -> when yn triggerRepaint
          (stopListening, tlo) <- generateWxLayout buttonPress tf (coContents tcfg)
          set tf [ layout := margin 10 $ fill $ tlo ]
          -- Hooks `windowOnDestroy`, not `on closing` -- see the note
          -- on 'UI.Widgets.wxWatchDynamic'.
          previousOnDestroy <- WX.windowGetOnDestroy tf
          WX.windowOnDestroy tf (stopListening >> previousOnDestroy)
          windowReLayout tf

          buildToolContext (SomeContext configValues) (Just tcfg) >>= \case
            Left _ -> setValue' toolEventHandler (\_ _ -> Nothing)
            Right toolContext ->
              getDynamic (dyn toolScript) >>= (vBuildToolHandler layer toolContext) >>= \case
                Left err -> do
                  putStrLn ("Problem with tool " ++ n ++ ": " ++ err)
                  setValue' toolEventHandler (\_ _ -> Nothing)
                Right h -> setValue' toolEventHandler h

          setValue' toolShowConfig (\viz -> do
                                       set tf [ visible := viz ]
                                       -- Un-steal focus from the config window
                                       when viz (set f [ visible := True ]))

    -- Change tracking for variables used in each tool
      {- FIXME TODO re-add change tracking for tools
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
-}

    addMenuBar f []
    focusOn p
    pure (void $ windowClose f True)

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
-- | The current values of a tool's own fields, as text, to tell when they
-- change.
toolFieldValues :: Tool -> IO [String]
toolFieldValues Tool{..} = getDynamic toolConfig >>= \case
  Nothing -> pure []
  Just cfg -> layoutToArgs (coContents cfg) >>= \case
    SomeContext' (Left err) -> pure [err]
    SomeContext' (Right (SomeContext ctx)) -> sequence $
      fromContext (\_ ty arg -> maybe "" (toUserString ty) <$> argGetValue arg) ctx

renderTile' :: Valued w
            => IORef Int
            -> IORef (Int, ViewerIssues)
            -> Bool
            -> (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double
                -> Ptr Word8 -> IO ViewerIssues)
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId renderIssues smooth action dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    -- A new render starts out with no issues; each block adds its own,
    -- unless a newer render has started since.
    atomicWriteIORef renderIssues (iD, mempty)
    modelRect <- modelToRect dim <$> get model value
    let action' p q r x y c = do
            curId <- readIORef renderId
            when (curId == iD) $ do
              issues <- action p q r x y c
              unless (issues == mempty) $
                atomicModifyIORef' renderIssues $ \(i, old) ->
                  ((i, if i == iD then old <> issues else old), ())
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

      forM_ (filter finiteDrawCommand cmds) $ \case

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
          let pt2 = (fst pt1, snd pt3)
              pt4 = (fst pt3, snd pt1)
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

-- | A tool's error, as a message for the user.
describeToolError :: SomeException -> String
describeToolError err
  | Just ioe <- fromException err, isUserError ioe = ioeGetErrorString ioe
  | Just (ErrorCall msg) <- fromException err = msg
  | otherwise = displayException err

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


historyAppend :: Model -> History -> History
historyAppend m h@(History olds m' _)
  | m == m'   = h
  | otherwise = History (m' : olds) m []

takePicture :: Window a -> Viewer -> IO ()
takePicture f v0 = do
  d <- dialog f [ text := "Save viewer snapshot"]
  p <- panel d []
  mbox <- textEntry p [ text := "1" ]
  cbox <- checkBox  p [ text := "Use subpixel smoothing?", checkable := True, checked := True ]
  ok     <- button p [ text := "Ok" ]
  cancel <- button p [ text := "Cancel" ]
  p' <- panel p []
  txt <- staticText p' [ text := "Choose any resolution adjustments for the saved picture" ]
  set p' [ layout := margin 5 . floatCentre . widget $ txt ]
  set d [ layout := margin 10 $ fill $ container p $ column 5
          [ hstretch . expand . margin 5 $ widget p'
          , row 5 [ margin 3 $ label "Dimension multiplier: ", hfill $ widget mbox, hglue, margin 5 $ widget cbox ]
          , row 5 [ margin 5 $ widget ok, margin 5 $ widget cancel ]]]

  result <- showModal d $ \done -> do
    set ok [ on command := do
               mm <- readMaybe <$> get mbox text
               s  <- get cbox checked
               case mm of
                 Nothing -> done Nothing
                 Just m  -> if m <= 0 then done Nothing else done (Just (m, s)) ]
    set cancel [ on command := done Nothing ]

  case result of
    Nothing -> pure ()
    Just (multiplier, smooth) -> cloneViewer v0 >>= \vi -> do
      modifyValue (vSize vi) (\(Dimensions (w, h)) -> Dimensions (w * multiplier, h * multiplier))
      modifyValue (vPixelSize vi) (/ fromIntegral multiplier)

      -- Named after the viewer to start with, minus any characters
      -- that can't go in a filename.
      title <- getDynamic (vTitle v0)
      let safeName = map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '-' else c) title
      mpath <- fileSaveDialog f True True "Save a picture of the current view"
        [("PNG files",["*.png"]), ("Any file",["*.*"])] "" (safeName ++ ".png")

      case mpath of
        Nothing   -> pure ()
        Just path -> void $ snapshotToFile vi smooth path
