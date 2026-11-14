{-# OPTIONS_GHC -fmax-pmcheck-models=200 #-}
-- (The mouse handler's one big case, with its guards, is past the pattern
-- match checker's default limit: it then reports exhaustive matches as
-- incomplete.)
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
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Planar

import UI.Tile
import UI.Magnify (onPinch)
import Task.Block (subBlockSize)
import UI.Layout
import UI.Widgets
import UI.WxProperties
import Actor.Viewer.ViewModel
import UI.MovieMaker
import UI.ToolProgress
import Actor.Movie (canAnimate)
import Actor.Viewer.Sphere
import Actor.Viewer.Disk
import Actor.Viewer.Clip

import Actor.Viewer.Preview (renderPreview)
import Data.ByteString (ByteString)

import Data.IORef

import Actor.UI
import Actor.Tool
import Actor.Layout
import Actor.Configuration
import Actor.Viewer
import Actor.Ensemble (buildToolContext, layoutToArgs, Colorizer(..), ViewerColorizer(..))
import Actor.Colorizer (choiceName, choiceArguments, missingArguments, argumentFields, choiceWithArguments)
import Actor.Event (Event(..), EventArgument_, EventArgument(..), Coordinate(..), ViewKind(..), hyperbolicDisplay, hyperbolicModel, ToolDispatcher, HandlerRun, newCancelFlag)
import Language.Value.Parser (parseConstant)
import Language.Type
import Language.Draw
import Language.Environment
import Language.Code.InterpretIO
import Language.Code.Parser (CommonDefs)
import Language.Value.Evaluator (HaskellValue)

import Data.DynamicValue hiding (clone)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.List (find)
import qualified System.Info as System

-- | What a viewer's menu can do to the project as a whole.
data ProjectMenuActions = ProjectMenuActions
  { pmSave          :: IO ()
    -- ^ Save the project to its file, or ask where to if it has none yet.
  , pmSaveAs        :: IO ()
    -- ^ Ask where to save the project, and save it there.
  , pmEditScript    :: IO ()
  , pmEditInfo      :: Window () -> IO ()
    -- ^ Show the project info dialog, over the given window. That must
    -- be a visible window, such as the viewer it was opened from -- not
    -- the session's hidden project window, which wx would show again
    -- once the dialog closed.
  , pmSetPreview    :: ByteString -> IO ()
    -- ^ Make this PNG image the project's preview.
  , pmClose         :: IO ()
    -- ^ Close the project, after offering to save any unsaved changes.
  , pmCloseWindow   :: IO ()
    -- ^ The same, for a viewer's own close button: which also asks
    -- before closing a project with other viewers.
  , pmResetValues   :: IO ()
    -- ^ Put every configuration value (the project's and its tools')
    -- back to its default from the script, and every viewer back to its
    -- default colorizer.
  , pmColorizers    :: String -> IO (Maybe ([Colorizer], ViewerColorizer))
    -- ^ The colorizers the viewer with this title can use, and how it's
    -- colorized now.
  , pmEditAutocolor :: IO ()
    -- ^ Show the project's autocolor window (see "UI.AutocolorEditor").
  , pmMovies        :: MovieSupport
    -- ^ The project's viewers, for making movies of (see "UI.MovieMaker").
  , pmCommonDefs    :: IO (Either String CommonDefs)
    -- ^ The definitions the project's @\@common@ stanzas make.
  , pmConfigFields  :: IO [Field]
    -- ^ The fields of the project's configuration.
  , pmUnsaved       :: Variable Bool
    -- ^ Whether the project has unsaved changes.
  , pmProjectName   :: IO String
  , pmSetColorizer  :: String -> String -> IO (Either String ())
    -- ^ Colorize the viewer with this title with this colorizer (written
    -- as in a project file: @Rainbow@, @Smooth(z)@), rebuilding the
    -- project -- just after this returns, since the rebuild replaces the
    -- viewer's window. An error means the project was left as it was.
  }

viewProject :: Window ()
            -> WxLayout
            -> (forall a. Frame a -> [Menu ()] -> IO ())
            -> ProjectMenuActions
            -> UI
viewProject projectWindow wxLayout addMenuBar projectActions = UI
  { newEnsemble = pure ()

  , makeLayout = \_ title ui -> do
      -- The configuration window from before the project was rebuilt
      -- (emptied: see 'releaseConfigFrame'), if there is one, or a new
      -- one. Its fields go in `host`, a panel filling it, so that
      -- emptying it for the next build is destroying `host`.
      spare <- takeSpareConfigFrame wxLayout
      (restoredConfig, clampConfig) <- restoreConfigWindow wxLayout
      f <- case spare of
        Just f' -> f' <$ set f' [ text := title ]
        Nothing -> do
          f' <- frameEx frameDefaultStyle [ text := title
                     , on resize := propagateEvent
                     , style := wxFRAME_TOOL_WINDOW .+. wxRESIZE_BORDER .+.
                                wxCAPTION .+. wxCLOSE_BOX
                     , visible := False
                     ] projectWindow
          WX.windowOnClose f' (set f' [ visible := False ])
          forM_ restoredConfig $ \WindowProperties{..} -> do
            Point x0 y0 <- get f' position
            set f' [ position := Point (fromMaybe x0 wpX) (fromMaybe y0 wpY) ]
          pure f'
      host <- panel f []

      forgetConfigWindow <- registerConfigWindow wxLayout $ do
        Point x y <- get f position
        pure (WindowProperties (Just x) (Just y))

      -- (a reused window keeps its menu bar: see 'makeWxComplexViewer')
      when (isNothing spare) (addMenuBar f [])

      (stopListening, innerLayout) <- generateWxLayout (runAppAction projectActions) (\_ -> pure ()) host ui
      resetButton <- button host [ text := "Reset to defaults"
                                 , on command := pmResetValues projectActions ]
      set host [ layout := fill . margin 5 . column 5 $
                   [ innerLayout, hfloatRight (widget resetButton) ] ]
      set f [ layout := fill $ widget host ]

      -- Hooks `windowOnDestroy`, not `on closing` -- see the note on
      -- 'UI.Widgets.wxWatchDynamic'.
      onOwnDestroy host (forgetConfigWindow >> stopListening)
      forgetFrame <- registerConfigFrame wxLayout (f <$ windowDestroy host)
      onOwnDestroy f forgetFrame
      -- (a reused window fits its new fields, but stays where it is)
      windowReLayout f
      when (clampConfig && isNothing spare) (keepOnPrimaryDisplay f)
      pure (windowShow f >> windowRaise f)

  , makeViewer = const (makeWxComplexViewer projectWindow wxLayout addMenuBar projectActions)
  }

data BuiltinTool = NavTool | DebugTool | MoviePathTool

-- | Ask for the arguments to give a colorizer with parameters, as they'd
-- be written between its parentheses.
askColorizerArguments :: Frame a -> String -> [(String, String, String)] -> IO (Maybe [String])
askColorizerArguments f name params = do
  d <- dialog f [ text := name ]
  p <- panel d []
  ok <- button p [ text := "Ok" ]
  cancel <- button p [ text := "Cancel" ]
  fields <- forM params $ \(param, _, current) -> textEntry p [ text := current, tooltip := param ]
  let fieldRow (param, note, _) field = row 5
        [ margin 3 (label (param ++ " =")), hfill (widget field), margin 3 (label note) ]
  set d [ layout := margin 10 $ fill $ container p $ column 5 $
          [ label ("Arguments for " ++ name ++ ":") ]
          ++ zipWith fieldRow params fields
          ++ [ label "An empty field means its default, or else the viewer's variable of that name."
             , row 5 [ margin 5 (widget cancel), hglue, margin 5 (widget ok) ] ] ]
  showModal d $ \onDone -> do
    set cancel [ on command := onDone Nothing ]
    set ok [ on command := mapM (`get` text) fields >>= onDone . Just ]

-- | Do what an 'ActionButton' in a layout asks.
runAppAction :: ProjectMenuActions -> AppAction -> IO ()
runAppAction pm = \case
  OpenAutocolorSettings -> pmEditAutocolor pm

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
      -- (a complex viewer's pixels are always square)
      case vCoordinate of
        RealCoordinates{} -> mapM_ (setValue' vAspect) vpAspect
        ComplexCoordinate{} -> do
          -- (and a complex viewer's view: the plane, or the globe)
          -- (a viewer of the hyperbolic plane can show the disk or the
          -- half-plane; others, the plane or the sphere)
          let hyperbolic = isJust (hyperbolicDisplay vOriginalView)
          case vpView of
            Just "sphere" | not hyperbolic -> setValue' vView SphereView
            Just "plane" | not hyperbolic -> setValue' vView PlaneView
            Just "disk" | hyperbolic -> setValue' vView DiskView
            Just "halfplane" | hyperbolic -> setValue' vView HalfPlaneView
            _ -> pure ()
          case vpRotation of
            Just [w, x, y, z] -> setValue' vRotation (normalizeRotation (Rotation w x y z))
            _ -> pure ()
          -- (a disk viewer's view of the hyperbolic plane)
          case vpIsometry of
            Just [ar, ai, br, bi] -> setValue' vIsometry (normalizeIsometry (Isometry (ar :+ ai) (br :+ bi)))
            _ -> pure ()
    let canResize = fromMaybe vCanResize (restored >>= vpResizable)
        startVisible = fromMaybe True (restored >>= vpVisible)
        frameStyle | canResize = frameDefaultStyle
                   | otherwise = frameDefaultStyle .-. wxRESIZE_BORDER .-. wxMAXIMIZE_BOX

    Dimensions (width, height) <- getDynamic vSize
    Dimensions (xpos, ypos) <- getDynamic vPosition

    -- The viewer's window: the one it had before the project was rebuilt
    -- (emptied: see 'releaseViewerFrame'), if there is one that suits,
    -- or a new one. Everything the viewer itself makes goes in `host`, a
    -- panel filling the window, or is hooked to it: its timers, watchers,
    -- and other windows. Emptying the window for the viewer's next build
    -- ('releaseFrame', below) is destroying `host`.
    spare <- takeSpareViewerFrame wxLayout title0 >>= \case
      Just kept | kfResizable kept == canResize -> pure (Just kept)
                | otherwise -> Nothing <$ windowDestroy (kfFrame kept)
      Nothing -> pure Nothing
    let reused = isJust spare
    -- A status bar, for reporting problems the viewer's script ran into
    -- while rendering. It's hidden while there's nothing to report. (A
    -- window only gets one, so a reused window keeps its own.)
    (f, statusText) <- case spare of
      Just kept -> pure (kfFrame kept, kfStatus kept)
      Nothing -> do
        f' <- frameEx frameStyle [ text := title0
                                 , position := Point xpos ypos
                                 , on resize := propagateEvent
                                 ] projectWindow
        statusText' <- statusField [ text := "" ]
        set f' [ statusBar := [statusText'] ]
        pure (f', statusText')
    host <- panel f []
    -- Closing a viewer's window closes its project (there'd be no easy
    -- way to get the viewer back otherwise), going through the project's
    -- own closing questions. That happens just after this close event,
    -- from a one-shot timer, rather than from inside it, since it tears
    -- down this very window.
    let movieProject = MovieProject
          { mpSupport = pmMovies
          , mpConfig = SomeContext configValues
          , mpCommonDefs = pmCommonDefs
          , mpFields = pmConfigFields
          , mpUnsaved = pmUnsaved
          , mpProjectName = pmProjectName
          , mpOwner = projectWindow }
    forgetMovieViewer <- registerMovieViewer pmMovies theViewer
    -- The configuration variable(s) the "Animate along path" tool moves,
    -- if it can be used here: the viewer's coordinate, if it's a
    -- complex configuration variable or a pair of real ones.
    let animatedVariable
          | canAnimate (SomeContext configValues) vCoordinate = Just vCoordinate
          | otherwise = Nothing
    onOwnDestroy host forgetMovieViewer

    WX.windowOnClose f $ do
      closer <- timer f [ interval := 1 ]
      set closer [ on command := do
                     set closer [ enabled := False ]
                     pmCloseWindow ]

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

    p <- scrolledWindow host [ scrollRate := sz 10 10, visible := True ]

    (rememberViewer, forgetViewer) <- registerViewer wxLayout title0 $ do
      Dimensions (x, y) <- getDynamic vPosition
      Dimensions (w, h) <- getDynamic vSize
      viewCenter <- getDynamic vCenter
      pixelSize <- getDynamic vPixelSize
      aspect <- getDynamic vAspect
      view <- getDynamic vView
      Rotation rw rx ry rz <- getDynamic vRotation
      Isometry (ar :+ ai) (br :+ bi) <- getDynamic vIsometry
      shown <- windowIsShown f
      -- Leave out what's the same as the default anyway, so that a
      -- saved project file only says what's special about each window.
      pure ViewerProperties
        { vpX = Just x, vpY = Just y, vpWidth = Just w, vpHeight = Just h
        , vpResizable = if canResize then Nothing else Just False
        , vpVisible = if shown then Nothing else Just False
        , vpCenter = Just viewCenter, vpPixelSize = Just pixelSize
        , vpAspect = if aspect == 1 then Nothing else Just aspect
        , vpView = if view == vOriginalView then Nothing
                   else Just (case view of { SphereView -> "sphere"; PlaneView -> "plane"; DiskView -> "disk"; HalfPlaneView -> "halfplane" })
        , vpRotation = if view == SphereView then Just [rw, rx, ry, rz] else Nothing
        , vpIsometry = if isJust (hyperbolicDisplay view) then Just [ar, ai, br, bi] else Nothing
        }

    -- When the configuration's variables change (which ones there are,
    -- or their types; not their values), the viewer is rebuilt, in this
    -- same window, starting from where it was left ('rememberViewer').
    -- Emptying the window destroys `host`, which runs everything hooked
    -- to it: its listeners and timers stop, and the layout forgets it.
    -- That happens from a one-shot timer of the window's, rather than
    -- from this watcher, which runs from one of `host`'s own timers.
    wxWatchDynamic host configEnv $ \_ -> do
      later <- timer f [ interval := 1 ]
      set later [ on command := do
                    set later [ enabled := False ]
                    rememberViewer
                    void (releaseViewerFrame wxLayout title0)
                    remakeViewer
                    -- (in case the rebuilt viewer didn't take it)
                    discardSpareFrames wxLayout ]


    -- HUD: the tool picker, the viewer's menu, the pointer's location,
    -- and the "Viewer paused" warning are drawn over the view by the
    -- viewer itself ('paintHud', below), and a click on one is found by
    -- where it was last drawn. (They used to be child windows over the
    -- view, but wx doesn't support child windows that overlap, or that
    -- can be seen through, on Windows: there the view painted over them,
    -- and their backgrounds were opaque.)
    let isDarkMode = True
        overlayBackground = if isDarkMode then rgba 0 0 0 (80 :: Int) else rgba 255 255 255 (180 :: Int)
        overlayForeground = if isDarkMode then white else black
        hudFont = fontDefault { _fontSize = baseFontSize }
        menuFont = fontDefault { _fontSize = baseFontSize + 6 }
        pointerFont = fontFixed { _fontSize = baseFontSize, _fontWeight = WeightBold }

    hudToolLabel <- newIORef "▼ Navigate"
    hudPointer <- newIORef ""
    hudWarning <- newIORef (Nothing :: Maybe String)
    -- Whether the viewer is paused while a movie is recorded (see
    -- 'movieRecording'); shown like a configuration problem's pause.
    hudMoviePaused <- newIORef False
    hudActive <- newIORef True
    hudSpots <- newIORef ([] :: [(Rect, HudSpot)])
    hudTooltip <- newIORef ""
    shouldShowPointerLocation <- variable [ value := False ]
    shouldShowToolPicker <- variable [value := True]
    toolPickerMenu <- menuPane [ text := "Tool" ]
    hamburgerMenu <- menuPane [ text := "Viewer" ]
    -- What to bring up to date in the menu just before it's shown (a
    -- check mark that follows the view, whatever changed it)
    beforeMenu <- newIORef (pure () :: IO ())

    let refreshHud = windowRefresh p False

        -- (Nothing: the pointer isn't over the model, as off the globe)
        setPointerLocation mxy = do
          writeIORef hudPointer (pointerText vCoordinate mxy)
          showPtr <- get shouldShowPointerLocation value
          when showPtr $ do
            Size w h <- get p clientSize
            windowRefreshRect p False (Rect 0 (h - 40) w 40)

        setToolLabel name = writeIORef hudToolLabel ("▼ " ++ name) >> refreshHud

        setWarning mw = do
          old <- readIORef hudWarning
          when (old /= mw) (writeIORef hudWarning mw >> refreshHud)

        setMoviePaused paused = do
          old <- readIORef hudMoviePaused
          when (old /= paused) (writeIORef hudMoviePaused paused >> refreshHud)

        hudSpotAt pt' = fmap snd . find (\(r, _) -> rectContains r pt') <$> readIORef hudSpots

        -- Draw the HUD (while the window's active), remembering where. A
        -- paused viewer says so even while its window isn't active (say,
        -- while a movie is recorded, with its progress window in front).
        paintHud dc = do
          active <- readIORef hudActive
          Size w h <- get p clientSize
          let measure fnt txt = set dc [ font := fnt ] >> getTextExtent dc txt
              box bg r = drawRect dc r [ brush := brushSolid bg, pen := penTransparent ]
              -- Text centered in a box.
              labelBox bg fg fnt r@(Rect x y bw bh) txt = do
                box bg r
                Size tw th <- measure fnt txt
                drawText dc txt (Point (x + (bw - tw) `div` 2) (y + (bh - th) `div` 2))
                  [ font := fnt, textColor := fg ]
          activeSpots <- if not active then pure [] else do
            showTools <- get shouldShowToolPicker value
            showPtr <- get shouldShowPointerLocation value

            toolSpot <- if not showTools then pure [] else do
              toolLabel <- readIORef hudToolLabel
              Size tw th <- measure hudFont toolLabel
              let r = Rect 5 5 (tw + 10) (th + 10)
              labelBox overlayBackground overlayForeground hudFont r toolLabel
              pure [(r, ToolPickerSpot)]

            let menuRect = Rect (w - 30) 5 25 25
            labelBox overlayBackground overlayForeground menuFont menuRect "☰"

            when showPtr $ do
              txt <- readIORef hudPointer
              Size tw th <- measure pointerFont txt
              let bw = max 310 (tw + 10)
              labelBox overlayBackground overlayForeground pointerFont
                    (Rect ((w - bw) `div` 2) (h - 30) bw (max 20 (th + 4))) txt

            pure (toolSpot ++ [(menuRect, MenuSpot)])

          -- (a problem with the configuration comes first: it's what
          -- needs doing something about)
          moviePaused <- readIORef hudMoviePaused
          warning <- readIORef hudWarning <&> \case
            Just err -> Just err
            Nothing | moviePaused -> Just "Paused while a movie is recorded: the viewer will \
                                          \redraw once the recording is done."
                    | otherwise -> Nothing
          warningSpot <- case warning of
            Nothing -> pure []
            Just err -> do
              let txt = "Viewer paused"
              Size tw th <- measure hudFont txt
              let r = Rect ((w - tw - 10) `div` 2) ((h - th - 10) `div` 2) (tw + 10) (th + 10)
              labelBox (rgba 255 0 0 (50 :: Int)) white hudFont r txt
              pure [(r, WarningSpot err)]
          writeIORef hudSpots (activeSpots ++ warningSpot)

        -- A click on the HUD (which it says it handled), or the pointer
        -- moving over it (showing the warning's details as a tooltip).
        hudMouse = \case
          MouseLeftDown pt' _ -> hudSpotAt pt' >>= \case
            Just ToolPickerSpot -> True <$ menuPopup toolPickerMenu pt' p
            Just MenuSpot -> True <$ (join (readIORef beforeMenu) >> menuPopup hamburgerMenu pt' p)
            _ -> pure False
          MouseMotion pt' _ -> do
            tip <- hudSpotAt pt' <&> \case
              Just (WarningSpot err) -> err
              _ -> ""
            old <- readIORef hudTooltip
            when (tip /= old) $ do
              writeIORef hudTooltip tip
              set p [ tooltip := tip ]
            pure False
          _ -> pure False

    currentTool <- variable [value := Left NavTool ]
    -- The path the Animate along path tool is drawing (in model coordinates,
    -- newest point first), and whether a drag is drawing it now.
    moviePath <- newIORef ([] :: [(Double, Double)])
    drawingMoviePath <- newIORef False
    -- Where the left mouse button went down, and where it has been dragged
    -- to since; both are Nothing when there's no click in progress.
    draggedTo <- variable [value := Nothing]
    lastClick <- variable [value := Nothing]

    -- (The window's own handlers are replaced, not added to, since a
    -- reused window still has the last build's.)
    set host [ layout := fill $ widget p ]
    set f [ layout := fill $ widget host
          , on activate := \vis -> do
              writeIORef hudActive vis
              refreshHud
              propagateEvent
          ]
    -- (sized for the image: a reused window's is the one it had, which
    -- is what was remembered for it, while laying it out again would
    -- shrink it to fit an empty panel)
    set f [ clientSize := sz width height ]
    windowReLayout f
    set f [ clientSize := sz width height ]
    when (clampViewer && not reused) (keepOnPrimaryDisplay f)

    -- A viewer whose window was closed when the project was saved
    -- stays closed (just hidden, as closing it does) until asked for.
    unless (startVisible || reused) (set f [ visible := False ])


    -- `requestRefresh` can be used from any thread to queue up a
    -- window refresh.
    offThreadRefresh <- newEmptyMVar
    let requestRefresh = void (tryPutMVar offThreadRefresh ())

    -- Build the initial view model
    (initialX, initialY) <- getDynamic vCenter
    initialPx <- getDynamic vPixelSize
    initialAspect <- getDynamic vAspect
    initialShape <- getDynamic vView >>= \k -> case (k, hyperbolicDisplay k) of
      (_, Just display) -> HyperbolicShape (hyperbolicModel vOriginalView) display <$> getDynamic vIsometry
      (SphereView, _) -> GlobeShape <$> getDynamic vRotation
      _ -> pure FlatShape

    let initialModel = Model (initialX, initialY) (initialPx, initialPx * initialAspect) initialShape
    model <- variable [value := initialModel]

    -- History tracking: carried over from before the project was rebuilt,
    -- if the window was (its current view is the one the window was left
    -- showing, which the restored properties gave)
    history <- newIORef $ case spare of
      Just kept | History olds _ news <- kfHistory kept -> History olds initialModel news
      _ -> History [] initialModel []

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
    onOwnDestroy host (void (windowDestroy debugger))
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

    let getToolEventHandler :: Tool -> IO ToolDispatcher
        getToolEventHandler Tool{..} = getDynamic (dyn toolEventHandler)

        -- Switch back to the Navigate tool from a tool whose handler failed
        -- or was stopped, without running its Deactivated handler (which
        -- may well fail, or be slow, too).
        dropToNavigate :: Tool -> IO ()
        dropToNavigate Tool{..} = do
          stopToolTimers
          set currentTool [ value := Left NavTool ]
          -- If it stopped partway through a click or drag, forget it,
          -- so that releasing the mouse doesn't zoom or recenter.
          set lastClick [ value := Nothing ]
          set draggedTo [ value := Nothing ]
          setToolLabel "Navigate"
          showToolConfig <- getDynamic (dyn toolShowConfig)
          showToolConfig False
          vDrawCmdsChanged >>= \tf -> when tf triggerRepaint

        -- Apply a tool handler's effects, returning whether that worked. If
        -- the handler failed, switch back to the Navigate tool *before*
        -- reporting the error, so that a tool which fails on every event
        -- (each step of a drag, or every refresh) can't produce an endless
        -- stream of error dialogs.
        guardTool :: Tool -> IO () -> IO Bool
        guardTool theTool@Tool{..} action = try action >>= \case
          Right () -> pure True
          Left (err :: SomeException)
            | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
            | otherwise -> do
                dropToNavigate theTool
                tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
                errorDialog f "Tool error" $ concat
                  [ "The \"", tname, "\" tool stopped because of an error:\n\n"
                  , describeToolError err
                  , "\n\nSwitching back to the Navigate tool." ]
                pure False

        -- Run one of a tool's event handlers (with a way to stop it, if
        -- it's slow: see "UI.ToolProgress"), returning whether it
        -- succeeded.
        runToolEventHandler theTool e = do
          handle <- getToolEventHandler theTool
          Model _ pxDims _ <- get model value
          maybe (pure True) (runToolHandler theTool) (handle pxDims e)

        -- Run a tool's handler for an event, as 'runToolEventHandler'.
        runToolHandler :: Tool -> HandlerRun -> IO Bool
        runToolHandler theTool@Tool{..} run = do
          tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
          runWithProgress f tname run >>= \case
            RunFinished apply -> guardTool theTool apply
            RunFailed err -> guardTool theTool (throwIO err)
            RunStopped toNavigate -> False <$ when toNavigate (dropToNavigate theTool)
            -- (another tool's handler was running)
            RunSkipped -> pure True

    let activateTool thisTool@Tool{..} = do
          deactivateCurrentTool
          tname <- getDynamic (tiName toolInfo) <&> fromRight "--"
          setToolLabel tname
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
              wxTimer host [ interval := max 10 ms
                        , enabled := True
                        , on command := do
                            void $ runToolEventHandler thisTool (Actor.Event.Timer name)
                            vDrawCmdsChanged >>= \tf -> when tf triggerRepaint ]
            writeIORef runningTimers ts

        activateNavTool = do
          deactivateCurrentTool
          setToolLabel "Navigate"
          set currentTool [ value := Left NavTool ]

        activateDebugTool = do
          deactivateCurrentTool
          setToolLabel "Debug"
          set debugger [ visible := True ]
          windowRaise f
          focusOn p
          set currentTool [ value := Left DebugTool ]

        activateMoviePathTool = do
          deactivateCurrentTool
          setToolLabel "Animate along path"
          set currentTool [ value := Left MoviePathTool ]

        deactivateCurrentTool = stopToolTimers >> get currentTool value >>= \case
          Left NavTool -> pure ()
          Left MoviePathTool -> do
            writeIORef moviePath []
            writeIORef drawingMoviePath False
            triggerRepaint
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

          -- (only where the viewer's coordinate is a configuration
          -- variable, which the path can move: where there's a
          -- "Select ..." tool)
          when (isJust animatedVariable) $
            void $ menuItem toolPickerMenu [ text := "Animate along path"
                                           , on command := activateMoviePathTool
                                           ]
          void $ menuItem toolPickerMenu [ text := "Debug\t?"
                                         , on command := activateDebugTool
                                         ]

    wxWatchDynamic1 host vTools setupToolPicker

    toolShortcuts <- fmap Map.unions $ do
      tools <- getDynamic vTools
      forM tools $ \tool -> getDynamic (tiShortcut $ toolInfo tool) <&> \case
        (c : "") -> Map.singleton (toLower c) (activateTool tool)
        _        -> Map.empty

    -------------------------------------------------------
    -- Main view
    -------------------------------------------------------

    lastRenderAction <- newMVar (\_ -> pure mempty)

    let getRenderAction = case vCodeWithArgs of
          CodeWithArgs vGetArgs _ vCode -> do
            vGetArgs >>= \case
              Left err   -> do
                setWarning (Just err)
                readMVar lastRenderAction
              Right vaArgs -> do
                setWarning Nothing
                -- (the values are bound once, for every block of the render)
                ViewerFunction bind <- getDynamic vCode
                action <- bind vaArgs
                modifyMVar_ lastRenderAction (\_ -> pure action)
                pure action

    renderId <- newIORef (0 :: Int)
    -- The issues reported so far by the current render, and its id.
    renderIssues <- newIORef (0 :: Int, mempty :: ViewerIssues)

    pendingResize <- variable [value := False]

    viewerTile     <- do
      renderAction <- getRenderAction
      renderTile' renderId renderIssues True renderAction (backgroundSeed initialModel (width, height)) (width, height) model
    currentTile    <- variable [value := viewerTile]
    savedTileImage <- variable [value := Nothing]
    -- The same, ready to draw (making a bitmap from an image is too slow
    -- to do at every paint, which can come at every mouse movement).
    savedTileBitmap <- variable [value := Nothing]
    -- Whether the saved image is from the current tile yet, rather than
    -- still the one from before the view last changed.
    savedTileIsCurrent <- newIORef True
    -- When the current render started, and a drag's next orientation for
    -- the globe, not shown yet: a drag's steps are coalesced, as
    -- parameter changes are, so that each render gets to show something
    -- before the next replaces it (see the globe-drag timer below).
    lastViewRender <- newIORef =<< getCurrentTime
    -- (a globe's or the disk's view, being dragged: see the timer below)
    pendingGlobe <- newIORef (Nothing :: Maybe ModelShape)
    -- A drag panning the view (where it started, the view then, and
    -- whether it's moved yet); and a pan's or a pinch's latest view, not
    -- rendered yet: the picture follows the pointer at once (moved, or
    -- scaled), and the renders follow as the globe's do (see the view
    -- timer below). When the last pinch event came (a pinch is over, and a
    -- step in the history, when they stop).
    panDrag <- newIORef (Nothing :: Maybe (Point, Model, Bool))
    pendingView <- newIORef (Nothing :: Maybe Model)
    lastPinch <- newIORef (Nothing :: Maybe UTCTime)
    -- The view the current tile renders, and the one the saved image
    -- shows (which the picture is drawn moved from, to the view wanted).
    currentTileModel <- newIORef =<< get model value
    savedTileModel <- newIORef =<< get model value
    -- The last tile whose render was done (and its view): a new view's
    -- render starts from its picture, moved, rather than from a render
    -- stopped partway (whose coarse and finished pieces would be carried
    -- along together). Forgotten when the parameters change.
    lastCompleteTile <- newIORef (Nothing :: Maybe (Tile, Model))
    -- When the current render started, until it's worth showing (see
    -- 'captureRender'): every render, as the web app's, so a quick one
    -- comes whole, with no coarse passes shown.
    parameterRenderHeld <- newIORef (Nothing :: Maybe UTCTime)
    lastTileImage  <- variable [value := Nothing]
    animate        <- variable [value := Nothing]
    useSmoothing <- variable [value := True]

    let startAnimatingFrom oldModel = do
            alreadyAnimating <- isJust <$> get animate value
            unless alreadyAnimating $ do
              now <- getCurrentTime
              img <- get savedTileBitmap value
              set lastTileImage [value := img]
              set animate [value := Just (now, oldModel, img)]

    -- Putting a value in this MVar is used to trigger a Refresh event
    -- on the currently active tool.
    needToSendRefreshEvent <- newEmptyMVar
    -- ...and this one, on the viewer's overlay (if it has one), which
    -- refreshes whatever tool is selected; it starts out wanting one.
    needToRefreshOverlay <- newMVar ()

    let changeViewTo newModel = do
          -- (it replaces any pan's or pinch's view still to render)
          writeIORef pendingView Nothing
          modifyIORef' history (historyAppend newModel)
          jumpViewTo newModel

        jumpViewTo = jumpViewWith AnimatedChange

        -- Go to a view, animating the change or not (a drag turning the
        -- globe doesn't: its steps keep the last image up until the new
        -- render's pixels arrive), or re-render the same view for new
        -- parameters.
        jumpViewWith how newModel0 = do
          let animated = how == AnimatedChange
          -- (with a group, the new view reduced, so that its numbers stay
          -- small however far it's moved; the old one by the same element,
          -- so the move between them, and its animation, are the same)
          (oldModel, newModel@(Model newCenter (newPixelSize, newPixelHeight) newShape)) <-
            (get model value >>= \old0 -> case modelShape newModel0 of
              HyperbolicShape _ _ v -> viewReductionFor theViewer v <&> \r ->
                let fix m = case modelShape m of
                      HyperbolicShape hm d u -> m { modelShape = HyperbolicShape hm d (r u) }
                      _ -> m
                in (fix old0, fix newModel0)
              _ -> pure (old0, newModel0))
          Size { sizeW = w, sizeH = h } <- get f clientSize
          set model [ value := newModel ]
          setValue' vCenter    newCenter
          setValue' vPixelSize newPixelSize
          setValue' vAspect (newPixelHeight / newPixelSize)
          case newShape of
            FlatShape -> setValue' vView PlaneView
            GlobeShape r -> setValue' vRotation r >> setValue' vView SphereView
            HyperbolicShape _ display v -> do
              setValue' vIsometry v
              setValue' vView (case display of { DiskDisplay -> DiskView; HalfPlaneDisplay -> HalfPlaneView })
          oldTile <- get currentTile value
          -- (the last tile that was done, to start from: see 'lastCompleteTile')
          oldComplete <- tileComplete oldTile
          when oldComplete $ writeIORef lastCompleteTile (Just (oldTile, oldModel))
          when (how == ParameterChange) $ writeIORef lastCompleteTile Nothing
          (seedTile, seedModel) <- if oldComplete then pure (oldTile, oldModel)
                                   else fromMaybe (oldTile, oldModel) <$> readIORef lastCompleteTile
          cancelTile oldTile
          renderAction <- getRenderAction
          smoothing <- get useSmoothing value
          -- (a new view's render starts from the old image, moved to where
          -- its points are in the new view, so that what it hasn't drawn
          -- yet shows what the old image did there: during an animation,
          -- where the new image is drawn in the old one's moving frame,
          -- that's the same picture in the same place. A parameter
          -- change's starts from scratch. Either way, on the sphere, the
          -- background off the globe is already in place. A view change
          -- that doesn't change the view, such as a globe drag's first
          -- step, keeps the image as it is.)
          let seed | how == ParameterChange = backgroundSeed newModel (w, h)
                   | otherwise = reprojectedSeed seedTile seedModel newModel (w, h)
                                   <|> backgroundSeed newModel (w, h)
          newViewerTile <- renderTile' renderId renderIssues smoothing renderAction seed (w, h) model
          set currentTile [ value := newViewerTile ]
          writeIORef currentTileModel newModel
          now <- getCurrentTime
          writeIORef lastViewRender now
          -- (every render is held back until it's worth showing)
          writeIORef parameterRenderHeld (Just now)
          when animated (startAnimatingFrom oldModel)
          -- Until the new render's first update, the saved image is the
          -- old one; that only matters if it's of a different view, since
          -- then it'd be drawn in the wrong place. (A parameter change
          -- re-renders the same view, and hiding the saved image until
          -- each render's first update would hide most of them while a
          -- parameter is dragged.)
          when (animated && newModel /= oldModel) (writeIORef savedTileIsCurrent False)
          void $ tryPutMVar needToSendRefreshEvent ()
          void $ tryPutMVar needToRefreshOverlay ()
          triggerRepaint


    -- Convert back and forth between viewport coordinates and coordinates
    -- in the underlying model (e.g. viewport coordinates to C-plane coordinates in
    -- the dynamical system or parameter plane)
    -- (On the sphere, the screen plane is the globe's: see 'screenToModel'.)
    let viewToScreen pt = do
            Size { sizeW = w, sizeH = h } <- get f clientSize
            let dim = (w,h)
                fullViewRect = rectangle (Viewport (0,0)) (Viewport dim)
            modelRect <- modelToRect @(Double,Double) dim <$> get model value
            pure (convertRect fullViewRect modelRect $ Viewport (pointX pt, pointY pt))
        viewToModel pt = do
          m <- get model value
          screenToModel m <$> viewToScreen pt
        -- whether a point of the window shows the model (on the sphere,
        -- whether it's on the globe)
        showsModelAt pt = showsModel <$> get model value <*> viewToScreen pt
        -- show the model point under the pointer, or "…" off the globe
        showPointerAt pt m = showsModelAt pt >>= \over -> setPointerLocation (if over then Just m else Nothing)
        -- (where a point of the screen plane is in the window, in pixels
        -- but not rounded to them, and the box of the window)
        screenToViewD mdl pt = do
          Size { sizeW = w, sizeH = h } <- get f clientSize
          let fullViewRect = rectangle (0, 0) (fromIntegral w, fromIntegral h) :: Rectangle (Double, Double)
              modelRect = modelToRect @(Double,Double) (w, h) mdl
          pure (convertRect modelRect fullViewRect pt)
        viewBox = do
          Size { sizeW = w, sizeH = h } <- get f clientSize
          pure (Box 0 0 (fromIntegral w) (fromIntegral h))
        screenToView mdl pt = toViewPoint <$> screenToViewD mdl pt
        -- A grouped viewer's tools' drawings, in each tile on the screen
        -- but the polygon's own (which the usual painting draws).
        paintTileCopies mdl box dc = when (isJust vGroup) $ do
          Size { sizeW = cw, sizeH = ch } <- get f clientSize
          copies <- overlayProjections theViewer (modelProjection mdl) (modelCenter mdl) (fst (modelPixelDim mdl)) (cw, ch)
          forM_ (drop 1 copies) $ \proj ->
            paintToolLayer proj (screenToViewD mdl) box (modelPixelDim mdl) vDrawCmds dc
        -- Recenter the disk's view on the model point under a point of the
        -- window: the hyperbolic translation taking it to the window's
        -- middle (a step in the history, animated).
        recenterDiskOn pt = do
          m <- get model value
          case modelShape m of
            HyperbolicShape hm display v -> do
              s <- viewToScreen pt
              changeViewTo m { modelShape = HyperbolicShape hm display (dragOnDisplay display v s (modelCenter m)) }
            _ -> pure ()

    -- Set paint handlers
    -- Everything is drawn through one graphics context (a wxGCDC around
    -- the paint DC), so that the animation's transforms and fades, and
    -- see-through colors, apply to all of it. (On macOS and GTK a paint DC
    -- draws through a graphics context anyway, but on Windows it's GDI,
    -- which ignores both: there the animation jumped, its first frame
    -- was drawn offset by half the window, and see-through colors were
    -- opaque.)
    set p [ on paintRaw := \paintDC r _dirty -> withGraphicsDC paintDC $ \dc gc -> do
            get animate value >>= \case
              Nothing -> do
                -- Normal paint. Draw current rendering, then layer
                -- tool imagery on top.
                viewRect <- windowGetViewRect f
                curTile <- get currentTile value
                let (w, h) = tileRect curTile
                -- (the view wanted: a pan's, ahead of the renders; the
                -- saved image drawn moved to it from its own, on grey)
                mdl <- readIORef pendingView >>= maybe (get model value) pure
                savedMdl <- readIORef savedTileModel
                -- (the saved image's pixel at (u, v) is at w/2 + ox + s (u - w/2),
                -- and the same down, in the view wanted: on the plane, or on
                -- a globe turned the same way)
                let sameKind = modelShape savedMdl == modelShape mdl
                    (sx, sy) = modelCenter savedMdl
                    (tx, ty) = modelCenter mdl
                    (spx, spy) = modelPixelDim savedMdl
                    (tpx, tpy) = modelPixelDim mdl
                    (ox, oy) = ((sx - tx) / tpx, (ty - sy) / tpy)
                    (scx, scy) = (spx / tpx, spy / tpy)
                    moved = sameKind && (abs ox >= 0.5 || abs oy >= 0.5 || abs (scx - 1) > 1e-9 || abs (scy - 1) > 1e-9)
                when moved $ paintUndrawn dc viewRect
                get savedTileBitmap value >>= \case
                  Nothing -> pure ()
                  Just bmp
                    | moved && (abs (scx - 1) > 1e-9 || abs (scy - 1) > 1e-9) -> do
                        let (cxv, cyv) = (fromIntegral w / 2, fromIntegral h / 2)
                        graphicsContextPushState gc
                        graphicsContextTranslate gc (cxv + ox) (cyv + oy)
                        graphicsContextScale gc (sz scx scy)
                        graphicsContextTranslate gc (negate cxv) (negate cyv)
                        drawCenteredBitmapAt (0, 0) bmp dc viewRect (w, h)
                        graphicsContextPopState gc
                    | moved -> drawCenteredBitmapAt (round ox, round oy) bmp dc viewRect (w, h)
                    | otherwise -> drawCenteredBitmapAt (0, 0) bmp dc viewRect (w, h)

                box <- viewBox
                -- (with a group, the drawings in each other tile on the
                -- screen too; see 'Actor.Viewer.overlayProjections')
                paintTileCopies mdl box dc
                get currentTool value >>= \case
                  Left NavTool | isNothing (modelRotation mdl) -> paintToolLayerWithDragBox (modelProjection mdl) (screenToViewD mdl) box (modelPixelDim mdl) vDrawCmds lastClick draggedTo dc r viewRect
                  Left MoviePathTool -> do
                    paintToolLayer (modelProjection mdl) (screenToViewD mdl) box (modelPixelDim mdl) vDrawCmds dc
                    -- (on the sphere, the parts of the path on the visible side)
                    pts <- fmap catMaybes . mapM (traverse (screenToView mdl) . projectToScreen (modelProjection mdl))
                             . reverse =<< readIORef moviePath
                    paintMoviePath dc pts
                  _  -> paintToolLayer (modelProjection mdl) (screenToViewD mdl) box (modelPixelDim mdl) vDrawCmds dc

              Just (startTime, oldModel, oldImage) -> do
                -- Animated paint. Zoom and blend smoothly between
                -- the new and old images.
                now <- getCurrentTime
                let speedD = 6 :: Double
                    dt = realToFrac (diffUTCTime now startTime) :: Double
                    blend = min 255 (round (speedD * 255 * dt) :: Integer)
                    t = min 1.0 (speedD * dt)
                when (blend >= 255) (set animate [value := Nothing])
                curTile <- get currentTile value
                let (w, h) = tileRect curTile

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

                -- (grey where neither image has anything)
                paintUndrawn dc =<< windowGetViewRect f
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

                  let drawOld = restoringContext $ do
                        zoom (1.0, 1.0) (viewCenterX, viewCenterY) $
                          case oldImage of
                            Nothing -> pure ()
                            Just im -> do
                              drawBitmap dc im (WX.pt
                                               (round $ negate viewCenterX)
                                               (round $ negate viewCenterY))
                                False []
                      zoomRatioX = fst (modelPixelDim newModel) / fst (modelPixelDim oldModel)
                      zoomRatioY = snd (modelPixelDim newModel) / snd (modelPixelDim oldModel)
                      drawNew = restoringContext $ do
                        zoom (zoomRatioX, zoomRatioY)
                             (viewCenterX + dx, viewCenterY + dy)
                          $ get savedTileBitmap value >>= \case
                                Nothing -> pure ()
                                Just im -> do
                                  drawBitmap dc im (WX.pt
                                                (round $ negate viewCenterX)
                                                (round $ negate viewCenterY)) False []
                  -- The old image, moved, then the new one fading in on top,
                  -- once there is one (until the new render's first update,
                  -- the saved image is still the old view). Where the new
                  -- render hasn't drawn yet, it shows the old image moved
                  -- into its view (see 'reprojectedSeed'), so the two agree
                  -- there.
                  newImageReady <- readIORef savedTileIsCurrent
                  withLayer 1 drawOld
                  when newImageReady $ do
                    withLayer (min 1 t) drawNew
                    -- Where the old image doesn't reach, there's nothing to
                    -- fade from: the new image shows there at once. (In
                    -- this context's coordinates, the old image is the
                    -- tile's own rectangle; the window is where the
                    -- transforms above take it from.)
                    let sx = fst (modelPixelDim oldModel) / fst (modelPixelDim midModel)
                        sy = snd (modelPixelDim oldModel) / snd (modelPixelDim midModel)
                        toLocal (px', py') = ( (px' - viewCenterX) / sx + viewCenterX + dx * t'
                                             , (py' - viewCenterY) / sy + viewCenterY + dy * t' )
                        (lx0, ly0) = toLocal (0, 0)
                        (lx1, ly1) = toLocal (fromIntegral w, fromIntegral h)
                    forM_ (rectanglesOutside (min lx0 lx1, min ly0 ly1, max lx0 lx1, max ly0 ly1)
                                             (0, 0, fromIntegral w, fromIntegral h)) $ \(x, y, rw, rh) ->
                      restoringContext $ do
                        graphicsContextClipByRectangle gc (Rect x y rw rh)
                        withLayer 1 drawNew
                box <- viewBox
                paintTileCopies midModel box dc
                paintToolLayer (modelProjection midModel) (screenToViewD midModel) box (modelPixelDim midModel) vDrawCmds dc
            paintHud dc
          ]

    lastKnownMouse <- newIORef Nothing
    -- A drag turning the globe: the orientation and screen point it
    -- started from, and whether it has moved yet.
    globeDrag <- newIORef (Nothing :: Maybe (Rotation, (Double, Double), Bool))
    -- (a drag moving the disk's view: the view it started from, where
    -- on the screen plane it started, and whether it's moved yet)
    diskDrag <- newIORef (Nothing :: Maybe ((HyperbolicModel, HyperbolicDisplay, Isometry), (Double, Double), Bool))
    -- (the pointer's last place on the screen plane, to zoom about)
    lastKnownScreen <- newIORef Nothing

    -- Set click and drag event handlers
    -- The viewer's single-key shortcuts (`<`, `=`, a tool's letter, ...)
    -- reach it only while it has the keyboard focus, so clicking on it
    -- takes the focus. That's already so on macOS, where clicking a
    -- window gives the focus back to whichever part of it last had it,
    -- but not on GTK, where these shortcuts otherwise stop working once
    -- the focus has been anywhere else.
    let focusWhenClicked = \case
          MouseLeftDown{}  -> focusOn p
          MouseRightDown{} -> focusOn p
          _ -> pure ()
    -- The Animate along path tool: a drag draws a path, and letting go asks
    -- how to make a movie along it (just after this event, from a
    -- one-shot timer, rather than from a modal dialog inside it).
    let moviePathMouse event = get currentTool value >>= \case
          Left MoviePathTool -> case event of
            MouseLeftDown pt' _ -> do
              z <- viewToModel pt'
              writeIORef moviePath [z]
              writeIORef drawingMoviePath True
              triggerRepaint
              propagateEvent
              pure True
            MouseLeftDrag pt' _ -> do
              drawing <- readIORef drawingMoviePath
              when drawing $ do
                z <- viewToModel pt'
                showPointerAt pt' z
                Model _ (px, _) _ <- get model value
                readIORef moviePath >>= \case
                  (x, y) : _ | sqrt ((fst z - x) ^ (2 :: Int) + (snd z - y) ^ (2 :: Int)) < 2 * px -> pure ()
                  _ -> modifyIORef' moviePath (z :) >> triggerRepaint
              propagateEvent
              pure True
            MouseLeftUp _ _ -> do
              drawing <- readIORef drawingMoviePath
              writeIORef drawingMoviePath False
              pts <- reverse <$> readIORef moviePath
              when (drawing && length pts >= 2) $ do
                asker <- timer host [ interval := 1 ]
                set asker [ on command := do
                              set asker [ enabled := False ]
                              title <- getDynamic vTitle
                              forM_ animatedVariable $ \var ->
                                makeMovieDialog f movieProject title (PathSource title var pts) ]
              propagateEvent
              pure True
            _ -> pure False
          _ -> pure False

    -- A drag of the tool's (or a box to zoom into), with the pointer here.
    let mouseDrag pt = do
          mpt <- viewToModel pt
          showPointerAt pt mpt
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
            Left MoviePathTool -> pure ()
            Right tool -> do
               get lastClick value >>= \case
                 Nothing -> pure ()
                 Just (Viewport vstart) -> do
                   zstart <- viewToModel (uncurry Point vstart)
                   z <- viewToModel pt
                   Model _ pxDims _ <- get model value
                   handle <- getToolEventHandler tool
                   case handle pxDims (Drag z zstart) of
                     Just action -> do
                       void (runToolHandler tool action)
                       vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                     Nothing -> dragAction
               propagateEvent

    set p [ on mouse   := \event -> focusWhenClicked event >> hudMouse event >>= \handled -> unless handled $ moviePathMouse event >>= \handledPath -> unless handledPath $ get lastClick value >>= \lastClickNow -> get currentTool value >>= \currentToolNow -> readIORef globeDrag >>= \globeDragNow -> readIORef diskDrag >>= \diskDragNow -> readIORef panDrag >>= \panNow -> case event of

              MouseLeftDown pt modifiers | isNoShiftAltControlDown modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                -- (on the sphere, Navigate turns the globe from here; on
                -- the plane, a drag pans, unless the tool takes drags)
                m <- get model value
                s0 <- viewToScreen pt
                writeIORef globeDrag ((\r -> (r, s0, False)) <$> modelRotation m)
                writeIORef diskDrag (case modelShape m of
                                       HyperbolicShape hm display v -> Just ((hm, display, v), s0, False)
                                       _ -> Nothing)
                writeIORef panDrag (if modelIsFlat m then Just (pt, m, False) else Nothing)
                propagateEvent

              -- (Shift and drag: a box to zoom into, on the plane)
              MouseLeftDown pt modifiers | isShiftOnly modifiers -> do
                set lastClick [value := Just $ Viewport (pointX pt, pointY pt)]
                writeIORef panDrag Nothing
                propagateEvent

              MouseLeftDrag pt modifiers | isShiftOnly modifiers -> do
                set draggedTo [value := Just $ Viewport (pointX pt, pointY pt)]
                getDragBox lastClick draggedTo >>= \box -> when (isJust box) triggerRepaint
                propagateEvent

              MouseLeftUp _ _ | Nothing <- lastClickNow ->
                -- No click in progress (e.g. a tool failed partway through
                -- one), so there's nothing to finish.
                propagateEvent

              MouseLeftUp _ modifiers
                | isNoShiftAltControlDown modifiers
                , Left NavTool <- currentToolNow
                , Just _ <- globeDragNow -> do
                    -- A drag turned the globe: one step in the history.
                    -- (A click does nothing on the sphere, for now.)
                    readIORef pendingGlobe >>= \case
                      Nothing -> pure ()
                      Just sh -> do
                        writeIORef pendingGlobe Nothing
                        m <- get model value
                        jumpViewWith UnanimatedChange m { modelShape = sh }
                    case globeDragNow of
                      Just (_, _, True) -> get model value >>= \m -> modifyIORef' history (historyAppend m)
                      _ -> pure ()
                    writeIORef globeDrag Nothing
                    set draggedTo [value := Nothing]
                    set lastClick [value := Nothing]
                    propagateEvent

              MouseLeftUp pt modifiers
                | isNoShiftAltControlDown modifiers
                , Left NavTool <- currentToolNow
                , Just (_, _, moved) <- diskDragNow -> do
                    writeIORef diskDrag Nothing
                    if moved
                      then do
                        -- A drag moved the disk's view: its last step, and
                        -- one step in the history.
                        readIORef pendingGlobe >>= \case
                          Nothing -> pure ()
                          Just sh -> do
                            writeIORef pendingGlobe Nothing
                            m <- get model value
                            jumpViewWith UnanimatedChange m { modelShape = sh }
                        get model value >>= \m -> modifyIORef' history (historyAppend m)
                      else recenterDiskOn pt
                    set draggedTo [value := Nothing]
                    set lastClick [value := Nothing]
                    propagateEvent

              -- A drag panned the view: its last step, and one step in the
              -- history.
              MouseLeftUp _ _ | Just (_, _, True) <- panNow -> do
                readIORef pendingView >>= mapM_ (\m -> do
                  writeIORef pendingView Nothing
                  jumpViewWith UnanimatedChange m)
                get model value >>= \m -> modifyIORef' history (historyAppend m)
                writeIORef panDrag Nothing
                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftUp pt modifiers | isNoShiftAltControlDown modifiers || isShiftOnly modifiers -> do
                writeIORef panDrag Nothing
                dragBox <- getDragBox lastClick draggedTo
                onModel <- showsModelAt pt
                let recenterAction = do
                      -- Completed a click, recenter to the clicked point.
                      oldModel <- get model value
                      if isJust (modelIsometry oldModel) then recenterDiskOn pt else do
                        newCenter <- viewToModel pt
                        changeViewTo oldModel { modelCenter = toCoords newCenter }
                ptz <- viewToModel pt
                toolClickAction <- if not onModel then pure (pure ()) else get currentTool value <&> \case
                  Left NavTool -> recenterAction
                  Left MoviePathTool -> pure ()
                  Left DebugTool -> do
                    (x, y) <- viewToModel pt
                    Model _ (dx, dy) _ <- get model value
                    debugAt (x, y) (dx, dy)
                  Right tool -> do
                    Model _ pxDims _ <- get model value
                    handle <- getToolEventHandler tool
                    case handle pxDims (Click ptz) of
                      Nothing -> recenterAction
                      Just action -> do
                        void (runToolHandler tool action)
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
                            -- (on the screen plane: the model's own, on the plane)
                            newCenter <- viewToScreen (viewportToPoint $ rectCenter box)
                            let (px, py) = modelPixelDim oldModel
                                (boxW, boxH) = dimensions box
                                oldArea = fromIntegral (w * h)
                                newArea = boxW * boxH
                                scale = sqrt (newArea / oldArea)
                            if scale < 0.001
                              then toolClickAction
                              else changeViewTo oldModel { modelCenter = toCoords newCenter
                                                         , modelPixelDim = (px * scale, py * scale) }
                      get currentTool value >>= \case
                        Right tool -> do
                          get lastClick value >>= \case
                            Nothing -> pure ()
                            Just (Viewport vstart) -> do
                              zstart <- viewToModel (uncurry Point vstart)
                              z <- viewToModel pt
                              Model _ pxDims _ <- get model value
                              handle <- getToolEventHandler tool
                              case handle pxDims (DragDone z zstart) of
                                Just action -> do
                                  void (runToolHandler tool action)
                                  vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
                                Nothing -> finishDrag
                        Left NavTool -> finishDrag
                        Left DebugTool -> pure ()
                        Left MoviePathTool -> pure ()

                set draggedTo [value := Nothing]
                set lastClick [value := Nothing]
                propagateEvent

              MouseLeftDrag pt modifiers
                | isNoShiftAltControlDown modifiers
                , Left NavTool <- currentToolNow
                , Just (r0, s0, _) <- globeDragNow -> do
                    -- Turn the globe, trackball-style: the point of the
                    -- sphere under the pointer stays under it.
                    s1 <- viewToScreen pt
                    writeIORef globeDrag (Just (r0, s0, True))
                    writeIORef pendingGlobe (Just (GlobeShape (trackballRotation r0 s0 s1)))
                    propagateEvent

              MouseLeftDrag pt modifiers
                | isNoShiftAltControlDown modifiers
                , Left NavTool <- currentToolNow
                , Just (v0@(hModel, hDisplay, iso0), s0@(x0, y0), moved) <- diskDragNow -> do
                    -- Move the disk's view by the hyperbolic translation
                    -- that keeps the model point under the pointer there.
                    -- (It's a drag once it's moved a little: until then, a
                    -- click, which recenters.)
                    s1@(x1, y1) <- viewToScreen pt
                    m <- get model value
                    let (px, _) = modelPixelDim m
                        moving = moved || abs (x1 - x0) + abs (y1 - y0) > 3 * px
                    when moving $ do
                      writeIORef diskDrag (Just (v0, s0, True))
                      writeIORef pendingGlobe (Just (HyperbolicShape hModel hDisplay (dragOnDisplay hDisplay iso0 s0 s1)))
                    propagateEvent

              MouseLeftDrag pt modifiers
                | isNoShiftAltControlDown modifiers
                , Just (p0, m0, moved) <- panNow -> do
                    pans <- case currentToolNow of
                      Left NavTool -> pure True
                      Right tool -> do
                        -- (the tool's, if it handles drags; else, a pan)
                        handle <- getToolEventHandler tool
                        let z = modelCenter m0
                        pure (isNothing (handle (modelPixelDim m0) (Drag z z))
                              && isNothing (handle (modelPixelDim m0) (DragDone z z)))
                      Left _ -> pure False
                    let (dx, dy) = (pointX pt - pointX p0, pointY pt - pointY p0)
                        -- (it's a drag once it's moved a few points: until then, a click)
                        moving = moved || abs dx + abs dy > 3
                    if not pans then writeIORef panDrag Nothing >> mouseDrag pt else when moving $ do
                      let (cx, cy) = modelCenter m0
                          (px, py) = modelPixelDim m0
                      writeIORef panDrag (Just (p0, m0, True))
                      writeIORef pendingView (Just m0 { modelCenter = (cx - fromIntegral dx * px, cy + fromIntegral dy * py) })
                      triggerRepaint
                    propagateEvent

              MouseLeftDrag pt modifiers | isNoShiftAltControlDown modifiers -> mouseDrag pt

              MouseMotion pt modifiers | isNoShiftAltControlDown modifiers -> do
                mpt <- viewToModel pt
                writeIORef lastKnownMouse (Just mpt)
                writeIORef lastKnownScreen . Just =<< viewToScreen pt
                showPointerAt pt mpt
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

    -- Pinching a trackpad zooms (see UI.Magnify): live, as a pan moves,
    -- about the pointer (on the sphere, about the globe's center).
    onPinch p $ \magnification (x, y) -> do
      base <- readIORef pendingView >>= maybe (get model value) pure
      Size { sizeW = w, sizeH = h } <- get f clientSize
      let factor = 1 / (1 + max (-0.5) (min 0.5 magnification))
          (px, py) = modelPixelDim base
          (cx, cy) = modelCenter base
          newCenter = case modelRotation base of
            Just _ -> (cx, cy)
            Nothing ->
              let mx = cx + (fromIntegral x - fromIntegral w / 2) * px
                  my = zoomPivotY base (cy - (fromIntegral y - fromIntegral h / 2) * py)
              in (mx + (cx - mx) * factor, my + (cy - my) * factor)
      writeIORef pendingView (Just base { modelCenter = newCenter, modelPixelDim = (px * factor, py * factor) })
      getCurrentTime >>= writeIORef lastPinch . Just
      triggerRepaint

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
            -- (the pointer's place on the screen plane: the model's own,
            -- on the plane)
            readIORef lastKnownScreen >>= \case
              Nothing -> propagateEvent
              Just (pmx, pmy0) -> do
                isAnimating <- isJust <$> get animate value
                unless isAnimating $ do
                  let (cmx, cmy) = modelCenter oldModel
                      pmy = zoomPivotY oldModel pmy0
                      cmx' = pmx + (cmx - pmx) * scale
                      cmy' = pmy + (cmy - pmy) * scale

                  -- (on the sphere, zooming about the globe's center)
                  changeViewTo oldModel { modelPixelDim = (px * scale, py * scale)
                                        , modelCenter = if isJust (modelRotation oldModel)
                                                          then modelCenter oldModel else (cmx', cmy') }

        _ -> propagateEvent


    -- Take what the current render has drawn so far, if it's drawn more
    -- since last time, as the image to show. A render of new parameters
    -- isn't taken until it's worth showing: once it has finished (a quick
    -- script's, straight away), or after a tenth of a second (then as it
    -- is: a medium script's coarse passes, refining, or a slow one's
    -- pieces arriving over grey). Until then the last image stays up,
    -- rather than a burst of grey or a flash of a coarse pass.
    lastCapture <- newIORef =<< getCurrentTime
    let captureRender = do
          curTile <- get currentTile value
          held <- readIORef parameterRenderHeld >>= \case
            Nothing -> pure False
            Just started -> do
              finished <- tileFinished curTile
              now <- getCurrentTime
              let held' = not finished && diffUTCTime now started < 0.1
              unless held' (writeIORef parameterRenderHeld Nothing)
              pure held'
          unless held $ ifModified curTile $ do
            viewRect <- windowGetViewRect f
            tileImage <- generateTileImage curTile viewRect
            saved <- imageCopy tileImage
            set savedTileImage [value := Just saved]
            bmp <- bitmapCreateFromImage saved (-1)
            set savedTileBitmap [value := Just bmp]
            readIORef currentTileModel >>= writeIORef savedTileModel
            writeIORef savedTileIsCurrent True
            triggerRepaint

    -- Add a timer which takes the render's latest picture when it's
    -- changed, ~30Hz, as the web app's patches come. (Taking one is cheap:
    -- about 0.2 ms for a 600x600 viewer, measured.)
    _ <- wxTimer host [ interval := 33
                   , enabled := True
                   , on command := do
                       curTile <- get currentTile value
                       captureRender
                       -- Report the current render's issues, if any. The
                       -- previous message stays up until the render either
                       -- finds an issue or finishes, so that the bar doesn't
                       -- flicker away at the start of every render.
                       (_, issues) <- readIORef renderIssues
                       finished <- tileFinished curTile
                       when (finished || issues /= mempty) $
                         setStatusMessage (fromMaybe "" (describeViewerIssues issues))
                   ]

    -- Add a timer which will check for re-rendering requests from off the
    -- main UI thread (new parameters), ~60Hz. They're coalesced, as a
    -- globe drag's steps are: a new render replaces the current one once
    -- it has finished, or once every part of it shows something and it
    -- has had a tenth of a second, or after half a second regardless (so
    -- a slow script doesn't fall far behind a parameter being dragged).
    -- Until then, the latest request waits.
    _ <- wxTimer host [ interval := 16
                   , enabled := True
                   , on command := do
                       -- (also showing a held render as soon as it's worth
                       -- it; other renders are taken by the 30Hz timer)
                       readIORef parameterRenderHeld >>= \held -> when (isJust held) captureRender
                       -- (while a movie is recorded, the request waits:
                       -- see 'movieRecording')
                       needRefresh <- not <$> isEmptyMVar offThreadRefresh
                       recordingMovie <- movieRecording pmMovies
                       setMoviePaused recordingMovie
                       when (needRefresh && not recordingMovie) $ do
                         curTile <- get currentTile value
                         finished <- tileFinished curTile
                         covered <- tileCovered curTile
                         started <- readIORef lastViewRender
                         now <- getCurrentTime
                         let elapsed = diffUTCTime now started
                         when (finished || (covered && elapsed >= 0.1) || elapsed >= 0.5) $ do
                           void (tryTakeMVar offThreadRefresh)
                           -- (what the render being replaced has drawn, first)
                           captureRender
                           jumpViewWith ParameterChange =<< get model value
                   ]

    -- The globe-drag timer: show a drag's latest orientation once the
    -- current render has finished, or has had a tenth of a second (as
    -- long as a parameter change's render gets), whichever comes first.
    _ <- wxTimer host [ interval := 16
                   , enabled := True
                   , on command := readIORef pendingGlobe >>= \case
                       Nothing -> pure ()
                       Just sh -> do
                         finished <- tileFinished =<< get currentTile value
                         started <- readIORef lastViewRender
                         now <- getCurrentTime
                         when (finished || diffUTCTime now started >= 0.1) $ do
                           writeIORef pendingGlobe Nothing
                           m <- get model value
                           jumpViewWith UnanimatedChange m { modelShape = sh }
                   ]

    -- A pinch is over a third of a second after its last event (once its
    -- view's rendering): a step in the history.
    _ <- wxTimer host [ interval := 50
                   , enabled := True
                   , on command := readIORef lastPinch >>= mapM_ (\t -> do
                       now <- getCurrentTime
                       pending <- isJust <$> readIORef pendingView
                       when (not pending && diffUTCTime now t >= 0.3) $ do
                         writeIORef lastPinch Nothing
                         get model value >>= \m -> modifyIORef' history (historyAppend m))
                   ]

    -- The view timer: render a pan's or a pinch's latest view (which the
    -- picture already shows, moved or scaled) once the current render has
    -- finished, or has had a tenth of a second, as the globe's turns are.
    _ <- wxTimer host [ interval := 16
                   , enabled := True
                   , on command := readIORef pendingView >>= \case
                       Nothing -> pure ()
                       Just m -> do
                         finished <- tileFinished =<< get currentTile value
                         started <- readIORef lastViewRender
                         now <- getCurrentTime
                         when (finished || diffUTCTime now started >= 0.1) $ do
                           writeIORef pendingView Nothing
                           jumpViewWith UnanimatedChange m
                   ]

    -- Animation timer. At ~65Hz, check if we are animating between
    -- two views. If so, step the animation and repaint.
    _ <- wxTimer host [ interval := 16
                   , enabled := True
                   , on command := get animate value >>= \case
                       Nothing -> pure ()
                       -- (with the render's latest, so that a view change's
                       -- animation shows what's been drawn as it's drawn: at
                       -- most every 33ms, since taking it isn't free)
                       Just _  -> do
                         now <- getCurrentTime
                         previous <- readIORef lastCapture
                         when (diffUTCTime now previous >= 0.033) $ do
                           writeIORef lastCapture now
                           captureRender
                         triggerRepaint
                   ]

    -- onResizeTimer is a one-shot timer that fires 100ms after the
    -- frame has been resized. If another resize event comes in during
    -- that interval, the timer is reset to 100ms. When the timer fires,
    -- we kick off a new rendering task to build the contents of the
    -- window. Using a timer lets us avoid starting hundreds of rendering
    -- tasks while the user adjusts their window size.
    onResizeTimer <- wxTimer host [ interval := 100
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
                                  -- (on the half-plane, keeping the real
                                  -- axis along the window's bottom edge)
                                  when (tileSize /= (w, h)) $
                                    jumpViewTo . keepAxisAtBottom h =<< get model value
                      ]

    -- Add a timer which checks if the tool layer should receive a refresh
    -- event: when the view changed, or the tool's own fields did. Changes
    -- the refresh handler itself makes to those fields don't count, unless
    -- the tool says they should (refreshCanUpdate=true, e.g. to animate).
    _ <- wxTimer host [ interval := 50
                   , enabled := True
                   , on command := do
                       -- (the overlay, too, waits for a movie's values to
                       -- be put back, so it goes with the picture)
                       recordingMovie <- movieRecording pmMovies
                       needOverlay <- if recordingMovie then pure False
                                      else isJust <$> tryTakeMVar needToRefreshOverlay
                       when needOverlay $ do
                         Model _ pxDims _ <- get model value
                         newCancelFlag >>= \flag -> vRefreshOverlay flag pxDims
                         vDrawCmdsChanged >>= \tf -> when tf triggerRepaint
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
              refreshHud
              propagateEvent ]

    -- wxHaskell doesn't seem to expose an "on move" event, so we'll just set up
    -- a timer to update the viewer's position every so often.
    _ <- wxTimer host [ interval := 200, enabled := True
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
    -- ...and, on the sphere, when the shading preference does (it's read
    -- live, like the globe's orientation)
    stopShadingListening <- watchDynamic sphereShading $ \_ -> do
      m <- get model value
      when (isJust (modelRotation m)) requestRefresh
    -- (the overlay's refresh is only queued here: a value is being set)
    stopOverlayListening <- vListenOverlay (void (tryPutMVar needToRefreshOverlay ()))
    -- Hooks `windowOnDestroy`, not `on closing` -- `host` is torn down
    -- with `windowDestroy` (along with its window, or by itself for a
    -- rebuild), which never fires `closing` at all; see the note on
    -- 'UI.Widgets.wxWatchDynamic'. (The render under way stops, too.)
    onOwnDestroy host $ do
      forgetViewer >> stopListening' >> stopOverlayListening >> stopShadingListening
      cancelTile =<< get currentTile value

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
        goHome = do
         Size _ hWin <- get f clientSize
         changeViewTo
          =<< (get model value <&> \m -> case modelShape m of
                 -- (the original view of the kind it shows now)
                 FlatShape -> let (c, px) = vOriginalPlaneView
                              in Model c (px, px * vOriginalAspect) FlatShape
                 GlobeShape _ -> Model (0, 0) (vOriginalSpherePixelSize, vOriginalSpherePixelSize)
                                       (GlobeShape vOriginalRotation)
                 -- (the half-plane with its real axis along the window's
                 -- bottom edge: see 'displayScreen')
                 HyperbolicShape hm display _ ->
                   let (c, px) = displayScreen display hWin
                                   ((case display of { DiskDisplay -> fst; HalfPlaneDisplay -> snd }) vOriginalHyperbolicPixelSizes)
                   in Model c (px, px) (HyperbolicShape hm display vOriginalIsometry))

        jumpToView = do
          oldModel <- get model value
          -- (on the sphere, the point the globe faces)
          let (oldX, oldY) = case modelShape oldModel of
                FlatShape -> modelCenter oldModel
                GlobeShape r -> sphereToPlane (rotateBy r (0, 0, 1))
                -- (in the disk, the point in the middle of the disk)
                -- (in the hyperbolic plane, the model point in the middle of
                -- the window)
                HyperbolicShape hm display v ->
                  fromMaybe (0, 0) (screenToModelH hm display v (modelCenter oldModel))
              (oldPx, oldPy) = modelPixelDim oldModel
          Size _ hWin <- get f clientSize
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
                        -- (keeping the aspect ratio)
                        -- (on the sphere, the point to face)
                        onDone ((\(x,y,dz) -> case modelShape oldModel of
                                   FlatShape -> Just (Model (x, y) (dz, dz * oldPy / oldPx) FlatShape)
                                   GlobeShape _ -> Just (Model (0, 0) (dz, dz) (GlobeShape (rotationFacing (x, y))))
                                   -- (in the hyperbolic plane, the point in the
                                   -- middle of the window; the half-plane's
                                   -- real axis along its bottom edge)
                                   HyperbolicShape hm display _
                                     | inModel hm (x, y) ->
                                         let middle = case display of
                                               DiskDisplay -> (0, 0)
                                               HalfPlaneDisplay -> axisAtBottom hWin dz (0, 0)
                                         in Just (Model middle (dz, dz)
                                                     (HyperbolicShape hm display (viewShowing hm display (x, y) middle)))
                                     | otherwise -> Nothing) =<<
                                 ((,,) <$> mx <*> my <*> mp)) ]
          case result of
            Nothing -> pure ()
            Just newModel -> changeViewTo newModel

        -- A real viewer's aspect ratio (the height of a pixel over its
        -- width), keeping the center and the width of a pixel.
        setAspectRatio = do
          oldModel <- get model value
          let (px, py) = modelPixelDim oldModel
          d <- dialog f [ text := "Aspect ratio..." ]
          aPanel <- panel d []
          aOk <- button aPanel [ text := "Ok" ]
          aCancel <- button aPanel [ text := "Cancel" ]
          aSquare <- button aPanel [ text := "Square pixels" ]
          aEntry <- textEntry aPanel [ text := printf "%g" (py / px) ]
          set d [ layout := margin 10 $ fill $ container aPanel $ column 5
                  [ row 5 [ label "pixel height / pixel width = ", expand $ widget aEntry ]
                  , row 5 [ margin 5 $ widget aSquare, hglue
                          , margin 5 $ widget aCancel, margin 5 $ widget aOk ] ] ]
          result <- showModal d $ \onDone -> do
            set aCancel [ on command := onDone Nothing ]
            set aSquare [ on command := onDone (Just 1) ]
            set aOk [ on command := do
                        txt <- get aEntry text
                        case parseConstant RealType txt of
                          Right a | a > 0, not (isInfinite a) -> onDone (Just a)
                          _ -> errorDialog d "Aspect ratio"
                                 "The aspect ratio should be a positive number, like 2 or 1/2." ]
          forM_ result $ \a -> changeViewTo oldModel { modelPixelDim = (px, px * a) }

    -------------------------------------------------------
    -- Menus
    -------------------------------------------------------

    -- Viewer menu
    menuItem hamburgerMenu [ text := "Last view\t<", on command := goBack ]
    menuItem hamburgerMenu [ text := "Next view\t>", on command := goForward ]
    menuItem hamburgerMenu [ text := "Jump to view...\t@", on command := jumpToView ]
    menuItem hamburgerMenu [ text := "Original view\t^", on command := goHome ]
    menuLine hamburgerMenu

    -- The colorizers this viewer can use, with the one it uses checked.
    -- Picking another rebuilds the project, replacing this window. One
    -- with parameters uses the viewer's variables of the same names, if
    -- it has them all; if not, it asks for the arguments first (and its
    -- item says so, with "..."). "Arguments..." changes the arguments of
    -- the one in use.
    pmColorizers title0 >>= mapM_ (\(available, ViewerColorizer{..}) -> do
      colorizerMenu <- menuPane [ text := "Colorizer" ]
      let nameOf = choiceName
          argsOf = choiceArguments
          missingFrom = missingArguments colorizerScope
          useColorizer chosen = pmSetColorizer title0 chosen >>= \case
            Left err -> errorDialog f "Couldn't use that colorizer" err
            Right () -> pure ()
          -- Ask for a colorizer's arguments, one field per parameter,
          -- starting from the ones written. A field left empty means the
          -- parameter's default, or else the viewer's variable of its
          -- name; arguments that come to that, at the end, are left out
          -- (all of them: the choice is just the colorizer's name).
          askArguments c written =
            askColorizerArguments f (colorizerName c) (argumentFields colorizerScope c written)
              >>= mapM_ (useColorizer . choiceWithArguments c)
      forM_ available $ \c@Colorizer{..} -> do
        let inUse = nameOf colorizerInUse == colorizerName
            missing = missingFrom c
            itemText | inUse = colorizerInUse
                     | null missing = colorizerName
                     | otherwise = colorizerName ++ "..."
        colorizerItem <- menuItem colorizerMenu [ text := itemText, checkable := True, checked := inUse
                                                , help := fromMaybe "" colorizerDescription ]
        set colorizerItem [ on command := do
          -- (Clicking a checkable item toggles it: put it back, in
          -- case the project isn't rebuilt after all.)
          set colorizerItem [ checked := inUse ]
          unless inUse $
            if null missing
              then useColorizer colorizerName
              else askArguments c
                     (if nameOf colorizerDefault == colorizerName then argsOf colorizerDefault else "") ]
      -- The arguments of the colorizer in use, if it has parameters.
      let inUseColorizer = find ((== nameOf colorizerInUse) . colorizerName) available
          inUseParams = maybe [] colorizerParams inUseColorizer
      menuLine colorizerMenu
      void $ menuItem colorizerMenu
        [ text := "Arguments..."
        , enabled := not (null inUseParams)
        , on command := mapM_ (\c -> askArguments c (argsOf colorizerInUse)) inUseColorizer ]
      -- (not really about this viewer's colorizer, since every viewer's
      -- autocolor groups are the project's; but kept here so that the
      -- viewer menu stays short)
      void $ menuItem colorizerMenu
        [ text := "Autocolor settings...", on command := pmEditAutocolor ]
      void (menuSub hamburgerMenu colorizerMenu [ text := "Colorizer" ]))
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
    -- Capturing what the viewer shows: a picture, or a movie of it as
    -- the project's values change (see "UI.MovieMaker").
    captureMenu <- menuPane [ text := "Capture" ]
    menuItem captureMenu [ text := "Image..."
                         , on command := takePicture f theViewer ]
    menuItem captureMenu [ text := "Movie..."
                         , on command := do
                             title <- getDynamic vTitle
                             makeMovieDialog f movieProject title ScriptSource ]
    void (menuSub hamburgerMenu captureMenu [ text := "Capture" ])

    -- (a complex viewer's pixels are always square; it can be shown on
    -- the plane or on the sphere instead)
    case vCoordinate of
      RealCoordinates{} -> void $
        menuItem hamburgerMenu [ text := "Aspect ratio...", on command := setAspectRatio ]
      -- (a viewer of the hyperbolic plane shows it as the disk or the
      -- half-plane, keeping its view)
      ComplexCoordinate{} | isJust (hyperbolicDisplay vOriginalView) -> do
        let onHalfPlane m = case modelShape m of { HyperbolicShape _ HalfPlaneDisplay _ -> True; _ -> False }
        halfItem <- menuItem hamburgerMenu [ text := "Show as the half-plane", checkable := True ]
        get model value >>= \m -> set halfItem [ checked := onHalfPlane m ]
        set halfItem [ on command := do
          m <- get model value
          case modelShape m of
            HyperbolicShape hm display v -> do
              Size _ hWin <- get f clientSize
              let display' = case display of { DiskDisplay -> HalfPlaneDisplay; HalfPlaneDisplay -> DiskDisplay }
                  (c, px) = displayScreen display' hWin
                              ((case display' of { DiskDisplay -> fst; HalfPlaneDisplay -> snd }) vOriginalHyperbolicPixelSizes)
              changeViewTo (Model c (px, px) (HyperbolicShape hm display' v))
            _ -> pure ()
          get model value >>= \m' -> set halfItem [ checked := onHalfPlane m' ] ]
        modifyIORef' beforeMenu (>> (get model value >>= \m -> set halfItem [ checked := onHalfPlane m ]))
      ComplexCoordinate{} -> do
        onSphere <- isJust . modelRotation <$> get model value
        sphereItem <- menuItem hamburgerMenu [ text := "Show on the sphere", checkable := True
                                             , checked := onSphere ]
        set sphereItem [ on command := do
          m <- get model value
          -- keeping the point in the middle of the view there (see
          -- 'Actor.Viewer.switchView'), each at its script's zoom
          case modelRotation m of
            Nothing -> changeViewTo (Model (0, 0) (vOriginalSpherePixelSize, vOriginalSpherePixelSize)
                                           (GlobeShape (rotationFacing (modelCenter m))))
            Just r -> do
              let c@(cx, cy) = sphereToPlane (rotateBy r (0, 0, 1))
                  finite a = not (isNaN a || isInfinite a) && abs a < 1e300
                  px = snd vOriginalPlaneView
              changeViewTo (Model (if finite cx && finite cy then c else (0, 0)) (px, px) FlatShape)
          isJust . modelRotation <$> get model value >>= \b -> set sphereItem [ checked := b ] ]
        -- (whatever else changes the view: Last view and Next view, say)
        modifyIORef' beforeMenu (>> (isJust . modelRotation <$> get model value
                                       >>= \b -> set sphereItem [ checked := b ]))

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
    menuLine hamburgerMenu
    showToolPicker  <- menuItem hamburgerMenu [ text := "Show tool picker"
                                              , checkable := True, checked := True ]
    showPtrPosition <- menuItem hamburgerMenu [ text := "Show pointer position"
                                              , checkable := True, checked := False ]
    menuLine hamburgerMenu
    projectMenu <- menuPane [ text := "Project" ]
    menuItem projectMenu [ text := "Save\tCtrl+S", enabled := True, on command := pmSave ]
    menuItem projectMenu [ text := "Save as...\tCtrl+Shift+S", enabled := True, on command := pmSaveAs ]
    menuLine projectMenu
    menuItem projectMenu [ text := "Edit project script...\tCtrl+E"
                         , enabled := True
                         , on command := pmEditScript
                         ]
    menuItem projectMenu [ text := "Project info...\tCtrl+I", on command := pmEditInfo (objectCast f) ]
    menuItem projectMenu [ text := "Use this as preview"
                           , on command := renderPreview False theViewer >>= \case
                               Left err -> errorDialog f "Couldn't make a preview" err
                               Right png -> pmSetPreview png ]
    menuLine projectMenu
    menuItem projectMenu [ text := "Close project\tCtrl+W", on command := pmClose ]
    void (menuSub hamburgerMenu projectMenu [ text := "Project" ])

    menuItem hamburgerMenu [ text := "Show configuration\t=", on command := raiseConfigWindow ]

    set showToolPicker [ on command := do
                           isChecked <- get showToolPicker checked
                           set shouldShowToolPicker [ value := isChecked ]
                           refreshHud
                       ]

    set showPtrPosition [ on command := do
                            isChecked <- get showPtrPosition checked
                            set shouldShowPointerLocation [ value := isChecked ]
                            refreshHud
                        ]

    -- Build the configuration window and the event handlers for each tool
    -- (remembering each tool's settings window, by the tool's name)
    toolWindows <- newIORef (Map.empty :: Map.Map String (Frame ()))
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
          onOwnDestroy host (void (windowDestroy tf))
          modifyIORef' toolWindows (Map.insert n tf)
          let appAction = \case
                OpenAutocolorSettings -> pmEditAutocolor
              buttonPress txt = do
                void $ runToolEventHandler Tool{..} (ButtonPressed txt)
                vDrawCmdsChanged >>= \yn -> when yn triggerRepaint
          (stopListening, tlo) <- generateWxLayout appAction buttonPress tf (coContents tcfg)
          set tf [ layout := margin 10 $ fill $ tlo ]
          -- Hooks `windowOnDestroy`, not `on closing` -- see the note
          -- on 'UI.Widgets.wxWatchDynamic'.
          onOwnDestroy tf stopListening
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

    -- Emptying the window for the viewer's next build, when the project
    -- is rebuilt (see 'releaseViewerFrame'): remembering which tool was
    -- selected (and where its settings window was), and putting the
    -- window's own handlers back to doing nothing until the next build
    -- sets its own.
    let selectedTool = get currentTool value >>= \case
          Left NavTool -> pure SelectedNavigate
          Left DebugTool -> pure SelectedDebug
          Left MoviePathTool -> pure SelectedMoviePath
          Right Tool{..} -> do
            name <- getDynamic (tiName toolInfo) <&> fromRight "--"
            settings <- Map.lookup name <$> readIORef toolWindows
            at <- forM settings $ \tf -> get tf visible >>= \case
              True -> Just <$> get tf position
              False -> pure Nothing
            pure (SelectedTool name (join at))
        releaseFrame = do
          tool <- selectedTool
          set f [ on resize := propagateEvent
                , on activate := \_ -> propagateEvent ]
          setStatusMessage ""
          void (windowDestroy host)
          viewHistory <- readIORef history
          pure KeptFrame { kfFrame = f, kfResizable = canResize
                         , kfStatus = statusText, kfTool = tool
                         , kfHistory = viewHistory }
    forgetFrame <- registerViewerFrame wxLayout title0 releaseFrame
    onOwnDestroy f forgetFrame

    -- Select the tool the viewer had selected before the project was
    -- rebuilt, if it still has it (its settings window coming back where
    -- it was). A new window starts with its "Select ..." tool, if it has
    -- one (clicking picks the parameter it shows), and otherwise with
    -- Navigate.
    case spare of
      Nothing -> forM_ vSelectTool activateTool
      Just kept -> case kfTool kept of
        SelectedNavigate -> pure ()
        SelectedDebug -> activateDebugTool
        SelectedMoviePath -> when (isJust animatedVariable) activateMoviePathTool
        SelectedTool name at -> do
          let named Tool{..} = (== Right name) <$> getDynamic (tiName toolInfo)
          filterM named tools >>= \case
            tool : _ -> do
              settings <- Map.lookup name <$> readIORef toolWindows
              forM_ ((,) <$> at <*> settings) $ \(pt, tf) -> set tf [ position := pt ]
              activateTool tool
            [] -> pure ()

    -- (A reused window keeps its menu bar: it's the project's, not the
    -- viewer's, and making another would leave the old one's watcher
    -- running.)
    unless reused (addMenuBar f [])
    focusOn p
    pure (void $ windowClose f True)

-- | The model point under a point of the screen plane. On the sphere, a
-- point off the globe counts as the nearest point of its rim.
screenToModel :: Model -> (Double, Double) -> (Double, Double)
screenToModel m s@(x, y) = case modelShape m of
  FlatShape -> s
  GlobeShape r -> fromMaybe s (screenToPlane r s')
  HyperbolicShape model DiskDisplay v -> fromMaybe s (screenToModelH model DiskDisplay v s')
  HyperbolicShape model HalfPlaneDisplay v -> fromMaybe s (screenToModelH model HalfPlaneDisplay v (x, max y 1e-12))
  where
    d = sqrt (x * x + y * y)
    s' = if d >= 1 then (0.999999 * x / d, 0.999999 * y / d) else s

-- | Whether a point of the screen plane shows the model: on the sphere,
-- whether it's on the globe.
showsModel :: Model -> (Double, Double) -> Bool
showsModel m (x, y) = case modelShape m of
  FlatShape -> True
  HyperbolicShape _ HalfPlaneDisplay _ -> y > 0
  _ -> x * x + y * y <= 1

-- | How a model's points get onto its screen plane.
modelProjection :: Model -> Projection
modelProjection m = case modelShape m of
  FlatShape -> FlatProjection
  GlobeShape r -> GlobeProjection r
  HyperbolicShape model display v -> HyperbolicProjection model display v

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
    , modelShape = case (modelShape m1, modelShape m2) of
        (GlobeShape r1, GlobeShape r2) -> GlobeShape (slerp r1 r2 t)
        (HyperbolicShape model d1 v1, HyperbolicShape _ d2 v2) | d1 == d2 ->
          HyperbolicShape model d1 (interpolateIsometry v1 v2 t)
        (_, sh2) -> sh2
    }
  where
    interp p q = p + t * (q - p)
    interp2 (p1,p2) (q1,q2) = (interp p1 q1, interp p2 q2)
    interpolate f = interp2 (f m1) (f m2)
    logInterp p q = p * (q/p) ** t
    logInterp2  (p1,p2) (q1,q2) = (logInterp p1 q1, logInterp p2 q2)
    logInterpolate f = logInterp2 (f m1) (f m2)

-- | What a new render shows until it's drawn: the old render's image,
-- moved to where its points are in the new view, so that a render that
-- hasn't got far (while a parameter is dragged, or the globe turned)
-- shows the old picture rather than grey; the pieces it covers are drawn
-- last, without the coarse passes (see 'Task.Block.progressivePlan'), so that the
-- parts that are new to the view are drawn first. On the plane, each new pixel
-- takes the old pixel at the same model point (the same pixel, if the
-- view hasn't moved); on the sphere, the old pixel at the same point of
-- the sphere, if it was on the old globe's visible side. Anything else
-- starts grey; so does everything, when the window's size or the kind
-- of view has changed.
reprojectedSeed :: Tile -> Model -> Model -> (Int, Int) -> Maybe (Ptr Word8 -> Ptr Word8 -> IO (Int -> Int -> Bool))
reprojectedSeed oldTile old new (w, h)
  | tileRect oldTile /= (w, h) = Nothing
  | shapeKind (modelShape old) /= shapeKind (modelShape new) = Nothing
  | otherwise = Just $ \dest finals -> withSynchedTileBuffer oldTile $ \src -> do
      -- (piece by piece, to tell which pieces it all came from the old image)
      let n = subBlockSize
          pieces = [ (bx, by) | by <- [0 .. (h - 1) `div` n], bx <- [0 .. (w - 1) `div` n] ]
      uncovered <- fmap concat . forM pieces $ \(bx, by) -> do
        missed <- fmap or . forM [ (i, j) | j <- [by * n .. min h ((by + 1) * n) - 1]
                                           , i <- [bx * n .. min w ((bx + 1) * n) - 1] ] $ \(i, j) -> do
          let screen = pixelToScreen new (i, j)
              from = case (modelShape old, modelShape new) of
                (GlobeShape ro, GlobeShape rn) -> do
                  p <- screenToSphere rn screen
                  sphereToScreen ro p >>= screenToPixel old
                (HyperbolicShape model dd vo, HyperbolicShape _ _ vn) -> do
                  p <- screenToModelH model dd vn screen
                  modelToScreenH model dd vo p >>= screenToPixel old
                _ -> screenToPixel old screen
              d = 3 * (j * w + i)
          case from of
            Just (i', j') -> do
              forM_ [0, 1, 2] $ \k ->
                peekByteOff src (3 * (j' * w + i') + k) >>= \(b :: Word8) -> pokeByteOff dest (d + k) b
              pure False
            -- (off the globe, the background, which is all there is to draw)
            Nothing
              | showsModel new screen -> do
                  pokeUndrawn dest d
                  pure True
              | otherwise -> do
                  pokeBackground dest finals d
                  pure False
        pure [ (bx, by) | missed ]
      let missing = Set.fromList uncovered
      pure (\bx by -> (bx, by) `Set.notMember` missing)
  where
    shapeKind :: ModelShape -> Int
    shapeKind = \case { FlatShape -> 0; GlobeShape _ -> 1
                      ; HyperbolicShape _ DiskDisplay _ -> 2; HyperbolicShape _ HalfPlaneDisplay _ -> 3 }
    pixelToScreen m = pixelSample m (w, h)
    -- the pixel sampled nearest a point of a view's screen plane, if it's
    -- in the window
    screenToPixel m (x, y) =
      let (cx, cy) = modelCenter m
          (px, py) = modelPixelDim m
          i = round ((x - cx) / px + fromIntegral w / 2)
          j = round ((cy - y) / py + fromIntegral h / 2)
      in if i >= 0 && i < w && j >= 0 && j < h then Just (i, j) else Nothing

-- | Zooming about a point (with the pointer there), on the half-plane:
-- about the point of the real axis below it instead, so that the axis
-- stays where it is, along the window's bottom edge (zooming about a
-- point of the axis moves the half-plane by an isometry).
zoomPivotY :: Model -> Double -> Double
zoomPivotY m y = case modelShape m of
  HyperbolicShape _ HalfPlaneDisplay _ -> 0
  _ -> y

-- | A model, its window resized to @h@ pixels high: on the half-plane,
-- keeping the real axis along the window's bottom edge.
keepAxisAtBottom :: Int -> Model -> Model
keepAxisAtBottom h m = case modelShape m of
  HyperbolicShape _ HalfPlaneDisplay _ -> m { modelCenter = axisAtBottom h (snd (modelPixelDim m)) (modelCenter m) }
  _ -> m

-- | What a new sphere view's render shows until it's drawn, when it
-- starts from scratch: grey on the globe, and the background off it,
-- since that's all there is to draw there (so the pieces wholly off the
-- globe are done). On the plane, the usual grey.
backgroundSeed :: Model -> (Int, Int) -> Maybe (Ptr Word8 -> Ptr Word8 -> IO (Int -> Int -> Bool))
backgroundSeed m (w, h) = case modelShape m of
  FlatShape -> Nothing
  -- (the globe, or the disk: a unit disk at the screen plane's origin; or
  -- the half-plane)
  _ -> Just $ \dest finals -> do
    -- Row by row: the globe is one span of a row (left as it is, not drawn
    -- yet), with the background on either side of it.
    let n = subBlockSize
        span' j =
          let (cx, cy) = modelCenter m
              (px, py) = modelPixelDim m
              y = cy - (fromIntegral j - fromIntegral h / 2) * py
              half = if y * y > 1 then -1 else sqrt (1 - y * y)
              -- the pixels whose sample points (see 'pixelSample') are
              -- within half of the globe's middle (the globe is centered
              -- on the screen plane's origin), so on the globe just where
              -- the render will say they are
              lo = ceiling ((negate half - cx) / px + fromIntegral w / 2) :: Int
              hi = floor ((half - cx) / px + fromIntegral w / 2) :: Int
          in case modelShape m of
               -- (the half-plane: a row is all on it, above the real axis, or all off)
               HyperbolicShape _ HalfPlaneDisplay _ -> if y > 0 then (0, w - 1) else (w, w - 1)
               _ -> if half < 0 then (w, w - 1) else (max 0 lo, min (w - 1) hi)
        spans = [ span' j | j <- [0 .. h - 1] ]
    forM_ (zip [0 ..] spans) $ \(j, (lo, hi)) -> do
      forM_ [0 .. lo - 1] $ \i -> pokeBackground dest finals (3 * (j * w + i))
      forM_ [max lo (hi + 1) .. w - 1] $ \i -> pokeBackground dest finals (3 * (j * w + i))
    -- a piece is done if none of its rows reach its columns with the globe
    let onGlobe bx by = or [ lo <= hi && hi >= bx * n && lo < (bx + 1) * n
                           | (lo, hi) <- take n (drop (by * n) spans) ]
    pure (\bx by -> not (onGlobe bx by))

-- | Where a render samples a pixel, on a view's screen plane: its top
-- left corner (the renderer steps from the window's top left corner by
-- whole pixels). The seeds must agree with the render about which pixels
-- are on the globe, or a coarse pass's square can take the background's
-- color from a pixel the seed thought was on the globe.
pixelSample :: Model -> (Int, Int) -> (Int, Int) -> (Double, Double)
pixelSample m (w, h) (i, j) =
  let (cx, cy) = modelCenter m
      (px, py) = modelPixelDim m
  in ( cx + (fromIntegral i - fromIntegral w / 2) * px
     , cy - (fromIntegral j - fromIntegral h / 2) * py )

-- | The parts of the first rectangle outside the second (each as left,
-- top, right, bottom; the parts as left, top, width, height).
rectanglesOutside :: (Double, Double, Double, Double) -> (Double, Double, Double, Double)
                  -> [(Double, Double, Double, Double)]
rectanglesOutside (lx0, ly0, lx1, ly1) (ox0, oy0, ox1, oy1) = filter sized
  [ (lx0, ly0, lx1 - lx0, min oy0 ly1 - ly0)                       -- above
  , (lx0, max oy1 ly0, lx1 - lx0, ly1 - max oy1 ly0)               -- below
  , (lx0, my0, min ox0 lx1 - lx0, my1 - my0)                       -- left
  , (max ox1 lx0, my0, lx1 - max ox1 lx0, my1 - my0) ]             -- right
  where
    my0 = max ly0 oy0
    my1 = min ly1 oy1
    sized (_, _, rw, rh) = rw > 0 && rh > 0

-- | Mark a pixel at an offset in an RGB8 buffer as not drawn yet (grey).
pokeUndrawn :: Ptr Word8 -> Int -> IO ()
pokeUndrawn dest d = forM_ [0, 1, 2] $ \k -> pokeByteOff dest (d + k) (128 :: Word8)

-- | Grey, where a render hasn't drawn yet.
paintUndrawn :: DC d -> Rect -> IO ()
paintUndrawn dc r = drawRect dc r [ brush := brushSolid (rgb (128 :: Word8) 128 128), pen := penTransparent ]

-- | Put the color off the globe at a pixel's offset in an RGB8 buffer,
-- marking the pixel as final in the seed's mask (a byte per pixel), so
-- that the coarse passes don't paint over it.
pokeBackground :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
pokeBackground dest finals d = do
  let (r, g, b) = colorToRGB sphereBackground
  pokeByteOff finals (d `div` 3) (1 :: Word8)
  pokeByteOff dest d r
  pokeByteOff dest (d + 1) g
  pokeByteOff dest (d + 2) b

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
            -> (ViewerArgs -> IO ViewerIssues)
            -> Maybe (Ptr Word8 -> Ptr Word8 -> IO (Int -> Int -> Bool))
            -> (Int, Int)
            -> w Model
            -> IO Tile
renderTile' renderId renderIssues smooth action seed dim model = do
    iD <- atomicModifyIORef' renderId (\x -> (x + 1, x + 1))
    -- A new render starts out with no issues; each block adds its own,
    -- unless a newer render has started since.
    atomicWriteIORef renderIssues (iD, mempty)
    Model viewCenter pixelDim _ <- get model value
    let action' args = do
            issues <- action args
            unless (issues == mempty) $
              atomicModifyIORef' renderIssues $ \(i, old) ->
                ((i, if i == iD then old <> issues else old), ())
            pure issues
        -- (once a newer render has started, this one stops, between pieces)
        stale = (/= iD) <$> readIORef renderId
    renderTile smooth seed action' stale dim (viewCenter, pixelDim)

-- | 'drawCenteredBitmap', moved by an offset (in the window's units).
drawCenteredBitmapAt :: (Int, Int) -> Bitmap () -> DC d -> Rect -> (Int,Int) -> IO ()
drawCenteredBitmapAt (dx, dy) bmp dc windowRect (width, height) = do
    let Point { pointX = fWidth, pointY = fHeight } = rectBottomRight windowRect
    let (x0, y0) = ( (fWidth  + width ) `div` 2 - width  ,
                     (fHeight + height) `div` 2 - height )
    drawBitmap dc bmp (WX.pt (x0 + dx) (y0 + dy)) False []

-- | How a viewer goes to a view ('jumpViewWith'): animating the change,
-- without animating it (a drag turning the globe), or re-rendering the
-- same view for new parameters (not animated, and not shown until it's
-- worth showing: see 'captureRender').
data ViewChange = AnimatedChange | UnanimatedChange | ParameterChange
  deriving Eq

-- | What's at a place on a viewer's HUD.
data HudSpot = ToolPickerSpot | MenuSpot | WarningSpot String

-- | Paint through a graphics context: a wxGCDC around a paint DC, and
-- its graphics context (for transforms and layers).
withGraphicsDC :: DC a -> (GCDC () -> GraphicsContext () -> IO ()) -> IO ()
withGraphicsDC paintDC action = do
  dc <- gcdcCreate (objectCast paintDC)
  gc <- gcdcGetGraphicsContext dc
  action dc gc `finally` gcdcDelete dc

viewportToPoint :: Viewport -> Point
viewportToPoint (Viewport (x,y)) = Point { pointX = x, pointY = y }


-- | Shift, and no other modifier (for a box to zoom into).
isShiftOnly :: Modifiers -> Bool
isShiftOnly m = shiftDown m && not (altDown m || controlDown m || metaDown m)

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
  :: Projection
  -> ((Double, Double) -> IO Px)
  -> Box
  -> (Double, Double)
  -> IO [[DrawCommand]]
  -> Var (Maybe Viewport)
  -> Var (Maybe Viewport)
  -> DC d
  -> Rect
  -> Rect
  -> IO ()
paintToolLayerWithDragBox proj screenToView viewBox pxDim getDrawCommands lastClick draggedTo dc _ _ = dcEncapsulate dc $ do
    paintToolLayer proj screenToView viewBox pxDim getDrawCommands dc
    dragBox <- getDragBox lastClick draggedTo
    case dragBox of
        Nothing  -> return ()
        Just box -> do
            let boxPts = map viewportToPoint (rectPoints box)
            drawBox dc (rgba @Word8 0 128 255 128) white boxPts

-- | Draw the Animate along path tool's path: a white line with a dark edge, so
-- that it shows up over anything, from a green dot at its start to a
-- red one at its end.
paintMoviePath :: DC d -> [Point] -> IO ()
paintMoviePath _ [] = pure ()
paintMoviePath dc pts@(firstPoint : _) = dcEncapsulate dc $ do
  let segments = zip pts (drop 1 pts)
  set dc [ pen := penColored (rgba @Word8 0 0 0 160) 5 ]
  forM_ segments $ \(a, b) -> line dc a b []
  set dc [ pen := penColored white 2 ]
  forM_ segments $ \(a, b) -> line dc a b []
  let dot c pt' = circle dc pt' 5 [ pen := penColored black 1, brush := brushSolid c ]
  dot green firstPoint
  dot red (last pts)

-- | Read draw commands for each tool layer and paint them into the
-- given device context. Model points get onto the screen plane by the
-- projection, and from there into the window by @screenToView@: on the
-- sphere, each shape is drawn where it is on the globe (a line or a
-- circle of the plane, sampled, as it's curved there), only on the
-- visible side.
paintToolLayer
  :: Projection
  -> ((Double, Double) -> IO Px)
  -> Box
  -> (Double, Double)
  -> IO [[DrawCommand]]
  -> DC d
  -> IO ()
paintToolLayer proj screenToView box pxDim getDrawCommands dc = dcEncapsulate dc $ do

    -- A shape is worked out in pixels unrounded, and drawn as it always
    -- was if it stays near the window; one reaching further is first cut
    -- down to the part near the window, since wxWidgets's coordinates are
    -- 32-bit integers, which wrap around (see "Actor.Viewer.Clip").
    let onGlobe = proj /= FlatProjection
        cut = cutBox box
        toView p = traverse screenToView (projectToScreen proj p)
        runsOf pts = mapM (mapM screenToView) (visibleRuns proj pts)
        polylinePx attrs pts
          | not (all finitePx pts) = pure ()
          | near box pts = polyline dc (map toViewPoint pts) attrs
          | otherwise = forM_ (clipPolyline cut pts) $ \r -> polyline dc (map toViewPoint r) attrs
        polygonPx attrs pts
          | not (all finitePx pts) = pure ()
          | near box pts = polygon dc (map toViewPoint pts) attrs
          | otherwise = case clipPolygon cut pts of
              clipped@(_ : _ : _ : _) -> polygon dc (map toViewPoint clipped) attrs
              _ -> pure ()
        nearPoint p = finitePx p && near box [p]
        -- a straight segment of the screen plane
        straightLine pt1 pt2 = do
          p <- screenToView pt1
          q <- screenToView pt2
          when (finitePx p && finitePx q) $
            if near box [p, q]
              then line dc (toViewPoint p) (toViewPoint q) [ brush := brushTransparent ]
              else forM_ (clipSegment cut p q) $ \(p', q') ->
                     line dc (toViewPoint p') (toViewPoint q') [ brush := brushTransparent ]
        -- a closed outline on the globe: filled if it's all visible,
        -- else its visible runs
        outline fil pts = runsOf pts >>= \case
          [r] | fil, length r == length pts -> polygonPx [] r
          rs -> forM_ rs (polylinePx [ brush := brushTransparent ])

    cmdss <- getDrawCommands
    let initialColor = rgba 255 255 255 (255 :: Word8)

    let withFill fil action
          | fil = action []
          | otherwise = action [ brush := brushTransparent ]

    forM_ cmdss $ \cmds -> do
      -- Reset the pen and brush for the next drawing layer
      set dc [ pen := penColored initialColor 2
             , brush := brushSolid initialColor
             , textColor := initialColor ]

      forM_ (filter finiteDrawCommand cmds) $ \case

        DrawPoint _ pt -> toView pt >>= \case
          Just p | nearPoint p -> do
            pc <- get dc penColor
            oldBrush <- get dc brush
            set dc [ brush := brushSolid pc ]
            withFill True (circle dc (toViewPoint p) 2)
            set dc [ brush := oldBrush ]
          _ -> pure ()

        DrawLine _ pt1 pt2
          | onGlobe -> runsOf (sampleSegment 64 pt1 pt2) >>= mapM_ (polylinePx [])
          | otherwise -> straightLine pt1 pt2

        -- (in the hyperbolic plane, the shortest path there; in the
        -- plane, a straight segment)
        DrawGeodesic _ pt1 pt2
          | onGlobe -> runsOf (sampleGeodesic proj 64 pt1 pt2) >>= mapM_ (polylinePx [])
          | otherwise -> straightLine pt1 pt2

        -- (a marker: a circle on the screen, its radius in pixels)
        DrawCircle _ fil ScreenPixels r pt -> toView pt >>= \case
          Just p | nearPoint p -> withFill fil (circle dc (toViewPoint p) (round (abs r)))
          _ -> pure ()
        DrawCircle _ fil ModelUnits r pt | onGlobe -> outline fil (sampleCircle 96 pt r)
        DrawCircle _ fil ModelUnits r pt -> do
          -- A circle in model space: an ellipse, when a real viewer's
          -- pixels aren't square.
          c@(cx, cy) <- screenToView pt
          let rx = r / fst pxDim
              ry = r / snd pxDim
          when (finitePx c && finitePx (rx, ry)) $
            if near box [(cx - rx, cy - ry), (cx + rx, cy + ry)]
              then do
                let Point icx icy = toViewPoint c
                    irx = round rx
                    iry = round ry
                withFill fil (ellipse dc (rect (WX.pt (icx - irx) (icy - iry)) (sz (2 * irx) (2 * iry))))
              else case ellipseNear cut c (abs rx) (abs ry) of
                Nothing -> pure ()
                -- (the window is all inside it)
                Just [] -> when fil (polygonPx [] (boxCorners cut))
                Just edge
                  | fil -> polygonPx [] (edge ++ [c])
                  | otherwise -> polylinePx [ brush := brushTransparent ] edge

        DrawRect _ fil (x1, y1) (x2, y2) | onGlobe -> do
          let corners = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]
          outline fil (concat (zipWith (\a b -> init (sampleSegment 32 a b)) corners (drop 1 corners)) ++ [(x1, y1)])
        DrawRect _ fil pt1 pt3 -> do
          let pt2 = (fst pt1, snd pt3)
              pt4 = (fst pt3, snd pt1)
          boxpts <- mapM screenToView [pt1, pt2, pt3, pt4]
          if fil || near box boxpts
            then withFill fil (\attrs -> polygonPx attrs boxpts)
            else polylinePx [ brush := brushTransparent ] (boxpts ++ take 1 boxpts)

        Clear {} -> pure () -- clear operations should be handled upstream,
                            -- by truncating the draw command list
        SetStroke _ c ->
          set dc [ pen := penColored (fsColorToWxColor c) 2
                 , textColor := fsColorToWxColor c ]

        SetFill _ c ->
          set dc [ brush := brushSolid (fsColorToWxColor c) ]

        Write _ txt pt -> toView pt >>= \case
          Just p | nearPoint p -> do
            let Point x0 y0 = toViewPoint p
            Size tw th <- getTextExtent dc txt
            let pt' = Point (x0 - tw `div` 2) (y0 - th `div` 2)
            withFill False (drawText dc txt pt')
          _ -> pure ()

-- | A point of the window, in whole pixels, as wxWidgets takes it.
toViewPoint :: Px -> Point
toViewPoint (x, y) = point (floor x) (floor y)

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
