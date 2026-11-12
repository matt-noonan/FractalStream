{-# LANGUAGE OverloadedStrings #-}
{- |
Module       : Main
Description  : The engine of FractalStream's web app

The core and the wasm backend, as a reactor module run by the page's
worker (@web/src/engine.ts@). The worker calls these exports:

* @fs_library()@: post the built-in library.
* @fs_open(section, entry)@: build that project; post its panel and
  viewers, then each viewer's frames as they're rendered.
* @fs_open_file(name, bytes)@: the same, for a project file's contents.
* @fs_save(views)@: post the open project as a file (its script, changed
  values, and so on, as the desktop app saves it), with the page's view
  state (JSON: see "web-properties" below).
* @fs_rotate(viewer, fx, fy, tx, ty)@: turn a sphere viewer's globe,
  trackball-style, as the pointer drags from one point of the screen to
  another (in units of the sphere's radius, from its center, y up).

As the desktop app does (and unlike fsrun, which is for reproducible
runs), a project opens with the configuration values (@values:@) and the
tables (@tables:@) it was saved with, and is saved with its own.
* @fs_set(name, text)@: set a configuration field (by name) from its text.
* @fs_view(viewer, cx, cy, pixelSize, width, height)@: move, zoom, or
  resize a viewer (all at once, for one render).
* @fs_resize(viewer, width, height)@: resize a viewer.
* @fs_smoothing(on)@: whether viewers are smoothed (each pixel the average
  of four samples, as fsrun's captures are), for every viewer from now on.
* @fs_select_tool(viewer, name)@: select a viewer's tool (@""@: none, to
  navigate), as the desktop app does: the old one's deactivated (and its
  timers stopped), the new one activated (and refreshed, if it asks to be)
  and its timers started; its panel (if it has one) is posted.
* @fs_event(viewer, kind, x1, y1, x2, y2)@: a pointer event for the
  viewer's tool, at points of the screen plane (a model point, on the
  plane; on the sphere, in units of its radius): kind 0 a click (at x1,
  y1), 1 a double click, 2 a drag (from x1, y1 to x2, y2) so far, 3 a drag
  finished. A tool's drag handler is given the pointer's position, then
  where it started, as on the desktop.
* @fs_button(viewer, label)@: a button of the viewer's tool's panel.
* @fs_set_tool(viewer, name, text)@: set a field of the viewer's tool's
  panel; the tool is refreshed.
* @fs_stop(viewer, navigate)@: stop the viewer's running handler (its
  tool's, or its overlay's refresh), with no effect. A stopped tool stays
  selected, unless @navigate@: then the viewer navigates again (without
  running the tool's @on deactivated:@ handler, which may well be slow
  too). A stopped overlay is turned off until the project's reopened.

A tool's handler, or an overlay's refresh, that's still running after a
quarter of a second is announced (@running@), and its end too
(@runDone@), so the page can show it's running, and offer to stop it.

After a tool's handler runs (and after each frame, with the viewer's
overlay refreshed for its view), the viewer's drawings are posted, when
they've changed (or always, on the sphere, where they move with it): see
"Drawing".

and the engine posts JSON messages (@globalThis.fractalstreamPost@),
frames (@globalThis.fractalstreamPostFrame@: RGB pixels, with the view they
show), and saved files (@globalThis.fractalstreamPostFile@: a name and the
contents), which the worker forwards to the page.

Messages:

> { "type": "library", "sections": [ { "name", "entries": [ { "title", "description", "preview" } ] } ] }
> { "type": "project", "title", "fileName", "library": <whether it's from the library>, "panel": <layout> or null, "viewers": [ { "id", "title", "width", "height", "sphere", "tools": [ { "name", "shortcut", "help" } ] } ], "views": <the file's web-properties> or null }
> { "type": "field", "key", "error": <message> or null, "color": <#rrggbb, for a color field> }
> { "type": "fieldText", "key", "text", "color": <as above> }   (a tool's handler changed it)

where a field's key is its name, in the project's panel, and
@tool:<viewer>:<name>@ in a tool's.
> { "type": "tool", "viewer", "name": <the selected tool> or null, "panel": <its layout> or null, "handles": [ "click" | "doubleClick" | "drag" | "dragDone" ] }
> { "type": "eventDone", "viewer" }   (an fs_event has been handled)
> { "type": "running", "viewer", "what": "tool" | "overlay", "name": <the tool's> }
> { "type": "runDone", "viewer" }
> { "type": "toolError", "viewer", "message" }   (a tool's handler failed: the viewer navigates again)
> { "type": "drawing", "viewer", "layers": [ [ <shape> ] ] }   (see "Drawing")
> { "type": "viewerError", "viewer", "message" }   (a render failed; the next frame clears it)
> { "type": "error", "message" }

The page's view state is saved in the project file under @web-properties:@
(as the desktop app's window layout is under @wx-properties:@), as the page
gives it to @fs_save@, and given back to it when the project is opened.

where a layout is @{ "kind": "vertical" | "horizontal", "children" }@,
@{ "kind": "panel", "title", "body" }@, @{ "kind": "tabs", "tabs": [ {
"label", "body" } ] }@, @{ "kind": "plain", "text" }@, or a field: @{
"kind": "text" | "check" | "select" | "color", "label", "name", "text",
"options" }@ (options for a selection; and for a color, @"color"@: its value, as
@#rrggbb@), or a button, @{ "kind": "button", "label" }@ (in a tool's
panel, pressing it is @fs_button@).
-}
module Main (main) where

import FractalStream.Prelude

import Actor.Ensemble (Ensemble(..), runEnsemble, parseEnsembleFromBytes, ensembleFields, FieldOwner(..),
                       encodeEnsemble, readProjectMetadata, changedValues, applySavedValues, ensembleTables)
import Actor.SavedValues (readSavedValues, writeSavedValues)
import Actor.SavedTables (readSavedTables, writeSavedTables, captureTables, applySavedTables)
import qualified Data.Aeson.KeyMap as KM
import Actor.Metadata (ProjectMetadata(..))
import Actor.Layout
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), RenderedBuffer(..), renderViewerBuffer, onParameterChanges, refreshOverlayWith, viewProjection)
import Actor.Event (ViewKind(..), Event(..), ToolDispatcher, EventArgument_, HandlerRun(..), HandlerCancelled(..),
                    CancelFlag, newCancelFlag, cancelRun)
import Actor.Viewer.Sphere (trackballRotation, screenToPlane)
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Configuration (Configuration(..))
import Actor.Ensemble (buildToolContext, buildEnsembleWithColorizers, carryOverProjectState)
import Language.Environment (SomeContext(..), Context(..))
import Drawing (drawingJSON)
import Editor (analysisJSON, symbolsJSON, errorJSON)
import Data.Color (colorToRGB)
import Text.Printf (printf)
import Backend (withBackend, Backend)
import Data.DynamicValue
import FractalStream.Library (LibrarySection(..), LibraryEntry(..), librarySections)

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, SomeAsyncException, try, catch, displayException, fromException, throwIO, finally)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Wasm.Prim (JSString(..), JSVal, toJSString, fromJSString)
import System.FilePath (takeBaseName)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- Talking to the page

foreign import javascript unsafe "globalThis.fractalstreamPost($1)"
  postRaw :: JSString -> IO ()

-- | A viewer's frame: its RGB pixels, size, and the view they show (the
-- center, and a pixel's width and height).
foreign import javascript unsafe
  "globalThis.fractalstreamPostFrame($1, new Uint8Array(__exports.memory.buffer, $2, $3).slice(), $4, $5, $6, $7, $8, $9)"
  postFrame :: Int -> Ptr Word8 -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> IO ()

-- | A saved project file: its name, and its contents.
foreign import javascript unsafe
  "globalThis.fractalstreamPostFile($1, new Uint8Array(__exports.memory.buffer, $2, $3).slice())"
  postFile :: JSString -> Ptr Word8 -> Int -> IO ()

foreign import javascript unsafe "$1.length"
  byteLength :: JSVal -> IO Int

-- | Copy a Uint8Array into the engine's memory.
foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.length).set($1)"
  copyBytes :: JSVal -> Ptr Word8 -> IO ()

post :: Aeson.Value -> IO ()
post = postRaw . toJSString . Text.unpack . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

postError :: String -> IO ()
postError msg = post (Aeson.object [ "type" .= ("error" :: Text.Text), "message" .= msg ])

-- | Run an export's work, posting any exception as an error.
guarded :: IO () -> IO ()
guarded act = try act >>= \case
  Left (e :: SomeException) -> postError (displayException e)
  Right () -> pure ()

------------------------------------------------------------------------
-- State

-- | The open project: its viewers (by id); what tools are given (the
-- ensemble's configuration); the threads to stop when it's closed.
data Session = Session
  { sViewers :: Map.Map Int ViewerState
  , sContext :: SomeContext EventArgument_
  , sEnsemble :: Ensemble
  , sFileName :: String
  , sColors :: Map.Map String (IO (Maybe String))
    -- ^ each color field's value, as #rrggbb
  , sStop :: IO ()
  , sTitle :: String
  , sLibrary :: Bool
    -- ^ (its title, and whether it's from the library: for a rebuild)
  }

-- | A viewer: its render loop's request signal, and its tool (if one's
-- selected).
data ViewerState = ViewerState
  { vsId :: Int
  , vsViewer :: Viewer
  , vsSignal :: MVar ()
  , vsTool :: IORef (Maybe ActiveTool)
  , vsDraw :: MVar ()
    -- ^ asks its drawing loop to refresh its overlay (and its tool, if
    --   'vsToolRefresh'), and post its drawings
  , vsToolRefresh :: IORef Bool
  , vsRun :: IORef (Maybe (CancelFlag, Bool, IORef Bool))
    -- ^ the running handler's cancel flag, whether it's the overlay's, and
    --   whether it's been stopped to navigate (its own: another handler
    --   may be waiting to run as soon as it's stopped)
  , vsLock :: MVar ()
    -- ^ held while its tool's handler runs: one at a time, as on the
    -- desktop (where they all run on the GUI thread)
  }

data ActiveTool = ActiveTool
  { atName :: String
  , atDispatch :: ToolDispatcher
  , atTimers :: [ThreadId]
  , atFields :: [Field]
  , atColors :: Map.Map String (IO (Maybe String))
  }

-- | Each field's text, by key, as the page was last told it.
textsRef :: IORef (Map.Map String String)
textsRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE textsRef #-}

toolKey :: ViewerState -> String -> String
toolKey vs n = "tool:" ++ show (vsId vs) ++ ":" ++ n

sessionRef :: IORef (Maybe Session)
sessionRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE sessionRef #-}

-- | Whether frames are smoothed (see 'renderViewerBuffer').
smoothingRef :: IORef Bool
smoothingRef = unsafePerformIO (newIORef False)
{-# NOINLINE smoothingRef #-}

backendRef :: IORef (Maybe Backend)
backendRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE backendRef #-}

-- | The backend, made once (after the page's worker has set up the
-- kernel workers, which it reads).
getBackend :: IO Backend
getBackend = readIORef backendRef >>= \case
  Just b -> pure b
  Nothing -> do
    b <- withBackend pure
    writeIORef backendRef (Just b)
    pure b

------------------------------------------------------------------------
-- Exports

foreign export javascript "fs_library" library :: IO ()
foreign export javascript "fs_open" open :: Int -> Int -> IO ()
foreign export javascript "fs_open_file" openFile :: JSString -> JSVal -> IO ()
foreign export javascript "fs_save" save :: JSString -> IO ()
foreign export javascript "fs_rotate" rotate :: Int -> Double -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_set" setField :: JSString -> JSString -> IO ()
foreign export javascript "fs_view" setView :: Int -> Double -> Double -> Double -> Int -> Int -> IO ()
foreign export javascript "fs_resize" resize :: Int -> Int -> Int -> IO ()
foreign export javascript "fs_smoothing" setSmoothing :: Bool -> IO ()
foreign export javascript "fs_select_tool" selectTool :: Int -> JSString -> IO ()
foreign export javascript "fs_event" toolEvent :: Int -> Int -> Double -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_button" toolButton :: Int -> JSString -> IO ()
foreign export javascript "fs_set_tool" setToolField :: Int -> JSString -> JSString -> IO ()
foreign export javascript "fs_stop" stopRun :: Int -> Bool -> IO ()
foreign export javascript "fs_analyze" analyze :: Int -> JSString -> IO ()
foreign export javascript "fs_symbols" symbols :: IO ()
foreign export javascript "fs_rebuild" rebuild :: JSString -> IO ()

main :: IO ()
main = pure ()

library :: IO ()
library = guarded $ post $ Aeson.object
  [ "type" .= ("library" :: Text.Text)
  , "sections" .=
      [ Aeson.object
          [ "name" .= lsName s
          , "entries" .=
              [ Aeson.object
                  [ "title" .= leTitle e
                  , "description" .= leDescription e
                  , "preview" .= fmap (Text.decodeUtf8 . B64.encode) (lePreview e)
                  ]
              | e <- lsEntries s ]
          ]
      | s <- librarySections ]
  ]

open :: Int -> Int -> IO ()
open section entry = guarded $ do
  LibraryEntry{..} <- case drop section librarySections of
    s : _ | e : _ <- drop entry (lsEntries s) -> pure e
    _ -> fail "no such project"
  openProject True leTitle leFileName leContents

openFile :: JSString -> JSVal -> IO ()
openFile name bytes = guarded $ do
  n <- byteLength bytes
  contents <- allocaBytes (max 1 n) $ \p -> copyBytes bytes p >> BS.packCStringLen (castPtr p, n)
  let fileName = fromJSString name
  -- (the project's title, if its metadata gives one; else its file's name)
  let title = either (const (takeBaseName fileName)) (fromMaybe (takeBaseName fileName) . mdTitle)
                     (readProjectMetadata contents)
  openProject False title fileName contents

save :: JSString -> IO ()
save views = guarded $ readIORef sessionRef >>= \case
  Nothing -> postError "there's no project to save"
  Just Session{..} -> do
    -- (the page's view state, under its own key)
    case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack (fromJSString views))) of
      Left err -> postError ("couldn't save the view: " ++ err)
      Right (v :: Aeson.Value) -> modifyValue (ensembleFrontendProperties sEnsemble) (KM.insert "web-properties" v)
    -- (the values it was left with; and its tables, if its file had them)
    changedValues sEnsemble >>= modifyValue (ensembleFrontendProperties sEnsemble) . writeSavedValues
    hasTables <- KM.member "tables" <$> getDynamic (ensembleFrontendProperties sEnsemble)
    when hasTables $ do
      tables <- ensembleTables sEnsemble >>= captureTables
      modifyValue (ensembleFrontendProperties sEnsemble) (writeSavedTables tables)
    contents <- encodeEnsemble sEnsemble
    BS.useAsCStringLen contents $ \(p, n) -> postFile (toJSString sFileName) (castPtr p) n

-- | Build a project from a file's contents, and start showing it.
openProject :: Bool -> String -> String -> BS.ByteString -> IO ()
openProject fromLibrary projectTitle fileName contents = do
  ensemble <- parseEnsembleFromBytes fileName contents >>= either fail pure
  -- The values and tables it was saved with, before anything is drawn
  -- (any that no longer fit are skipped, and said so on the console).
  props <- getDynamic (ensembleFrontendProperties ensemble)
  either (\err -> putStrLn ("Ignoring unreadable `values:`: " ++ err))
         (applySavedValues ensemble >=> mapM_ putStrLn) (readSavedValues props)
  either (\err -> putStrLn ("Ignoring unreadable `tables:`: " ++ err))
         (\saved -> ensembleTables ensemble >>= \ts -> applySavedTables ts saved >>= mapM_ putStrLn)
         (readSavedTables props)
  startProject fromLibrary projectTitle fileName False Map.empty ensemble

-- | Show a project, in place of the open one (if any): run it, and post
-- its panel and viewers. On a rebuild (from an edited script), each
-- viewer the new project still has (by its title) gets the tool it had,
-- if it still has one by that name (or none, if it had none).
startProject :: Bool -> String -> String -> Bool -> Map.Map String (Maybe String) -> Ensemble -> IO ()
startProject fromLibrary projectTitle fileName rebuilt toolsBefore ensemble = do
  close
  backend <- getBackend

  -- Run the project, collecting its panel and viewers.
  panelRef <- newIORef Nothing
  viewersRef <- newIORef []
  contextRef <- newIORef (SomeContext EmptyContext)
  let ui = UI
        { newEnsemble = pure ()
        , makeLayout = \_ title lo -> writeIORef panelRef (Just (title, lo)) >> pure (pure ())
        , makeViewer = \_ _ _ ctx _ viewer -> do
            modifyIORef' viewersRef (viewer :)
            writeIORef contextRef ctx
            pure (pure ())
        }
  runEnsemble backend ui ensemble
  viewers <- zip [0 ..] . reverse <$> readIORef viewersRef

  -- A render loop for each viewer: it renders whenever asked (once for
  -- however many asks came while it was busy), and posts the frame.
  loops <- forM viewers $ \(k, viewer) -> do
    signal <- newMVar ()   -- (the first frame)
    tool <- newIORef Nothing
    lock <- newMVar ()
    draw <- newEmptyMVar
    toolRefresh <- newIORef False
    running <- newIORef Nothing
    let vs = ViewerState k viewer signal tool draw toolRefresh running lock
    tid <- forkIO (renderLoop vs)
    drawTid <- forkIO (drawingLoop vs)
    unwatch <- onParameterChanges viewer (requestRender signal)
    -- (as the desktop app does, the overlay's refreshed when any
    -- configuration value changes, whether or not the viewer re-renders)
    unlisten <- vListenOverlay viewer (void (tryPutMVar draw ()))
    pure ((k, vs), (tid, killThread drawTid >> unwatch >> unlisten >> stopTimers vs))
  let stop = forM_ (map snd loops) $ \(tid, unwatch) -> killThread tid >> unwatch
  context <- readIORef contextRef

  fields <- projectFieldTexts ensemble
  writeIORef textsRef fields
  colorsRef <- newIORef Map.empty
  panel <- readIORef panelRef >>= traverse (\(title, lo) -> do
    body <- layoutJSON id colorsRef fields lo
    pure (Aeson.object [ "kind" .= ("panel" :: Text.Text), "title" .= title, "body" .= body ]))
  colors <- readIORef colorsRef
  writeIORef sessionRef (Just (Session (Map.fromList (map fst loops)) context ensemble fileName colors stop
                                       projectTitle fromLibrary))
  viewerInfo <- forM viewers $ \(k, viewer) -> do
    title <- getDynamic (vTitle viewer)
    Dimensions (w, h) <- getDynamic (vSize viewer)
    sphere <- (== SphereView) <$> getDynamic (vView viewer)
    tools <- getDynamic (vTools viewer) >>= mapM (\t -> do
      let ToolInfo{..} = toolInfo t
      name <- getDynamic (source tiName)
      shortcut <- getDynamic tiShortcut
      help <- getDynamic tiShortHelp
      pure (Aeson.object [ "name" .= name, "shortcut" .= shortcut, "help" .= help ]))
    pure (Aeson.object [ "id" .= k, "title" .= title, "width" .= w, "height" .= h, "sphere" .= sphere
                       , "tools" .= tools ])
  views <- KM.lookup "web-properties" <$> getDynamic (ensembleFrontendProperties ensemble)
  script <- getDynamic (ensembleSourceScript ensemble)
  post $ Aeson.object
    [ "type" .= ("project" :: Text.Text)
    , "title" .= projectTitle
    , "fileName" .= fileName
    , "library" .= fromLibrary
    , "panel" .= panel
    , "viewers" .= viewerInfo
    , "views" .= views
    , "script" .= script
    , "rebuilt" .= rebuilt
    ]
  forM_ (map (snd . fst) loops) $ \vs -> do
    title <- getDynamic (vTitle (vsViewer vs))
    names <- getDynamic (vTools (vsViewer vs)) >>= mapM (getDynamic . source . tiName . toolInfo)
    case Map.lookup title toolsBefore of
      -- (its tool before the rebuild, if it still has it)
      Just (Just name) | name `elem` names -> activateTool vs name
      Just Nothing -> pure ()
      -- (as the desktop app does: a viewer with a "Select" tool starts with it)
      _ -> forM_ (vSelectTool (vsViewer vs)) $ \t ->
             getDynamic (source (tiName (toolInfo t))) >>= activateTool vs

-- | Stop the open project's render loops (if any).
close :: IO ()
close = readIORef sessionRef >>= mapM_ (\s -> sStop s >> writeIORef sessionRef Nothing)

requestRender :: MVar () -> IO ()
requestRender signal = void (tryPutMVar signal ())

renderLoop :: ViewerState -> IO ()
renderLoop vs@ViewerState{ vsId = k, vsViewer = viewer, vsSignal = signal } = forever $ do
  takeMVar signal
  smooth <- readIORef smoothingRef
  try (renderViewerBuffer smooth viewer) >>= \case
    -- (being stopped, when another project is opened, isn't an error: the
    -- loop just ends)
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> postError (displayException e)
    Right (Left err) -> post $ Aeson.object
      [ "type" .= ("viewerError" :: Text.Text), "viewer" .= k, "message" .= err ]
    Right (Right RenderedBuffer{..}) -> do
      withForeignPtr rbPixels $ \p ->
        postFrame k p (3 * rbWidth * rbHeight) rbWidth rbHeight
                  (fst rbCenter) (snd rbCenter) (fst rbPixelSize) (snd rbPixelSize)
      -- (the overlay and the tool, for this view, as the desktop app
      -- refreshes them with each render)
      writeIORef (vsToolRefresh vs) True
      void (tryPutMVar (vsDraw vs) ())

-- | Refresh a viewer's overlay, and its tool after a frame, and post its
-- drawings, whenever asked (once for however many asks came while it was
-- busy: a slow handler doesn't fall behind).
drawingLoop :: ViewerState -> IO ()
drawingLoop vs = forever $ do
  takeMVar (vsDraw vs)
  withMVar (vsLock vs) $ \_ ->
    withProgress vs Nothing (\flag -> refreshOverlayWith flag (vsViewer vs))
  toolToo <- atomicModifyIORef' (vsToolRefresh vs) (False,)
  when toolToo (void (fire vs Refresh))
  postDrawings vs

setField :: JSString -> JSString -> IO ()
setField name text = guarded $ readIORef sessionRef >>= \case
  Nothing -> pure ()
  Just Session{..} -> do
    let n = fromJSString name
    fields <- ensembleFields sEnsemble
    result <- case [ f | (ProjectConfig, f) <- fields, fieldName f == n ] of
      f : _ -> setFieldText f (fromJSString text)
      [] -> pure (Left ("no field named " ++ n))
    color <- maybe (pure Nothing) id (Map.lookup n sColors)
    post $ Aeson.object $
      [ "type" .= ("field" :: Text.Text)
      , "key" .= n
      , "error" .= either Just (const Nothing) result
      ] ++ [ "color" .= c | Just c <- [color] ]

withViewer :: Int -> ((Viewer, MVar ()) -> IO ()) -> IO ()
withViewer k act = withViewerState k (\vs -> act (vsViewer vs, vsSignal vs))

withViewerState :: Int -> (ViewerState -> IO ()) -> IO ()
withViewerState k act = guarded $ readIORef sessionRef >>= \case
  Nothing -> pure ()
  Just Session{..} -> mapM_ act (Map.lookup k sViewers)

setView :: Int -> Double -> Double -> Double -> Int -> Int -> IO ()
setView k cx cy pixelSize w h = withViewer k $ \(viewer, signal) -> do
  setValue (vSize viewer) (Dimensions (max 1 w, max 1 h))
  setValue (vCenter viewer) (cx, cy)
  setValue (vPixelSize viewer) pixelSize
  requestRender signal

setSmoothing :: Bool -> IO ()
setSmoothing on = guarded $ do
  before <- atomicModifyIORef' smoothingRef (on,)
  when (before /= on) $ readIORef sessionRef >>=
    mapM_ (\Session{..} -> mapM_ (requestRender . vsSignal) (Map.elems sViewers))

rotate :: Int -> Double -> Double -> Double -> Double -> IO ()
rotate k fx fy tx ty = withViewer k $ \(viewer, signal) -> do
  r <- getDynamic (vRotation viewer)
  setValue (vRotation viewer) (trackballRotation r (fx, fy) (tx, ty))
  requestRender signal

resize :: Int -> Int -> Int -> IO ()
resize k w h = withViewer k $ \(viewer, signal) -> do
  setValue (vSize viewer) (Dimensions (max 1 w, max 1 h))
  requestRender signal

------------------------------------------------------------------------
-- Tools

selectTool :: Int -> JSString -> IO ()
selectTool k name = withViewerState k $ \vs -> case fromJSString name of
  "" -> deactivateTool vs >> postTool vs Nothing Aeson.Null
  n -> activateTool vs n

-- | Select a viewer's tool by name, as the desktop app does.
activateTool :: ViewerState -> String -> IO ()
activateTool vs@ViewerState{..} name = do
  deactivateTool vs
  readIORef sessionRef >>= mapM_ (\Session{ sContext = ctx } -> do
    tools <- getDynamic (vTools vsViewer)
    named <- mapM (\t -> (, t) <$> getDynamic (source (tiName (toolInfo t)))) tools
    case lookup name named of
      Nothing -> postError ("no tool named " ++ name)
      Just tool -> do
        mcfg <- getDynamic (toolConfig tool)
        built <- buildToolContext ctx mcfg >>= \case
          Left err -> pure (Left err)
          Right tctx -> do
            layer <- getDynamic (toolDrawLayer tool)
            script <- getDynamic (dyn (toolScript tool))
            try (vBuildToolHandler vsViewer layer tctx script) <&> \case
              Left (e :: SomeException) -> Left (displayException e)
              Right r -> r
        case built of
          Left err -> toolFailed vs ("couldn't select " ++ name ++ ": " ++ err)
          Right dispatch -> do
            -- its panel
            (panel, fields, colors) <- case mcfg of
              Nothing -> pure (Aeson.Null, [], Map.empty)
              Just Configuration{..} -> do
                fs <- layoutFields coContents
                texts <- Map.fromList <$> mapM (\f -> (fieldName f,) <$> fieldText f) fs
                modifyIORef' textsRef (Map.union (Map.mapKeys (toolKey vs) texts))
                colorsRef <- newIORef Map.empty
                body <- layoutJSON (toolKey vs) colorsRef texts coContents
                title <- getDynamic coTitle <&> fromRight name
                cs <- readIORef colorsRef
                -- (titled, unless its layout already is)
                let titled = case coContents of
                      Panel{} -> body
                      _ -> Aeson.object [ "kind" .= ("panel" :: Text.Text), "title" .= title, "body" .= body ]
                pure (titled, fs, cs)
            writeIORef vsTool (Just (ActiveTool name dispatch [] fields colors))
            postTool vs (Just name) panel
            activated <- fire vs Activated
            refreshFirst <- getDynamic (toolRefreshOnActivate tool)
            ok <- if activated && refreshFirst then fire vs Refresh else pure activated
            -- (its timers, unless it's already failed)
            when ok $ do
              timers <- getDynamic (toolTimers tool)
              tids <- forM timers $ \(timer, ms) -> forkIO $ forever $ do
                threadDelay (1000 * max 10 ms)
                void (fire vs (Timer timer))
                postDrawings vs
              modifyIORef' vsTool (fmap (\t -> t { atTimers = tids }))
            postDrawings vs)

-- | Deselect a viewer's tool (if one's selected), stopping its timers.
deactivateTool :: ViewerState -> IO ()
deactivateTool vs = readIORef (vsTool vs) >>= \case
  Nothing -> pure ()
  Just _ -> do
    stopTimers vs
    void (fire vs Deactivated)
    writeIORef (vsTool vs) Nothing
    postDrawings vs

stopTimers :: ViewerState -> IO ()
stopTimers vs = readIORef (vsTool vs) >>= mapM_ (mapM_ killThread . atTimers)

postTool :: ViewerState -> Maybe String -> Aeson.Value -> IO ()
postTool vs name panel = do
  -- (which pointer events it handles: the rest navigate, as on the desktop)
  handles <- readIORef (vsTool vs) <&> \case
    Nothing -> []
    Just ActiveTool{..} ->
      [ k | (k :: Text.Text, ev) <- [ ("click", Click (0, 0)), ("doubleClick", DoubleClick (0, 0))
                                    , ("drag", Drag (0, 0) (0, 0)), ("dragDone", DragDone (0, 0) (0, 0)) ]
          , isJust (atDispatch (1, 1) ev) ]
  post $ Aeson.object
    [ "type" .= ("tool" :: Text.Text), "viewer" .= vsId vs, "name" .= name, "panel" .= panel
    , "handles" .= handles ]

-- | A tool's handler failed: the viewer navigates again (without running
-- the tool's Deactivated handler, which may well fail too).
toolFailed :: ViewerState -> String -> IO ()
toolFailed vs message = do
  stopTimers vs
  writeIORef (vsTool vs) Nothing
  post $ Aeson.object [ "type" .= ("toolError" :: Text.Text), "viewer" .= vsId vs, "message" .= message ]
  postTool vs Nothing Aeson.Null

-- | Run the viewer's tool's handler for an event, if it has one; False if
-- it failed.
fire :: ViewerState -> Event -> IO Bool
fire vs@ViewerState{..} ev = readIORef vsTool >>= \case
  Nothing -> pure True
  Just ActiveTool{..} -> do
    px <- getDynamic (vPixelSize vsViewer)
    aspect <- getDynamic (vAspect vsViewer)
    case atDispatch (px, px * aspect) ev of
      Nothing -> pure True
      -- (once it has the lock, only if the tool's still selected: it may
      -- have been dropped meanwhile, by stopping another handler)
      -- (stopped: no effect; and back to navigating, if asked, before the
      -- lock goes to another handler waiting for it)
      Just run -> try (withMVar vsLock $ \_ -> readIORef vsTool >>= \case
                    Just ActiveTool{ atName = current } | current == atName ->
                      withProgress vs (Just atName) (startHandlerRun run >=> id) `catch` \(Stopped toNavigate) -> do
                        when toNavigate $ do
                          stopTimers vs
                          writeIORef vsTool Nothing
                          postTool vs Nothing Aeson.Null
                        throwIO (Stopped toNavigate)
                    _ -> pure ()) >>= \case
        Left (e :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | Just (Stopped _) <- fromException e -> pure False
          | otherwise -> False <$ toolFailed vs (atName ++ ": " ++ displayException e)
        Right () -> True <$ postFieldChanges

-- | Run a viewer's handler (its tool's, named, or its overlay's), with
-- a cancel flag for 'stopRun'; and if it's still running after a quarter
-- of a second, say so, and say when it's done.
withProgress :: ViewerState -> Maybe String -> (CancelFlag -> IO a) -> IO a
withProgress ViewerState{..} name act = do
  flag <- newCancelFlag
  toNavigate <- newIORef False
  writeIORef vsRun (Just (flag, isNothing name, toNavigate))
  -- (0 running, 1 announced, 2 done: changed, and posted, under the lock,
  -- so the page hears of them in order)
  stage <- newMVar (0 :: Int)
  _ <- forkIO $ do
    threadDelay 250000
    modifyMVar_ stage $ \case
      0 -> do
        post $ Aeson.object [ "type" .= ("running" :: Text.Text), "viewer" .= vsId
                            , "what" .= (if isJust name then "tool" else "overlay" :: Text.Text)
                            , "name" .= name ]
        pure 1
      st -> pure st
  (act flag `catch` \HandlerCancelled -> readIORef toNavigate >>= throwIO . Stopped) `finally` do
    writeIORef vsRun Nothing
    modifyMVar_ stage $ \st -> do
      when (st == 1) $ post (Aeson.object [ "type" .= ("runDone" :: Text.Text), "viewer" .= vsId ])
      pure 2

-- | A handler was stopped ('stopRun'), and whether to navigate again.
newtype Stopped = Stopped Bool
  deriving Show
instance Exception Stopped

stopRun :: Int -> Bool -> IO ()
stopRun k navigate = withViewerState k $ \ViewerState{..} -> readIORef vsRun >>= mapM_ (\(flag, overlay, toNavigate) -> do
  if overlay then vStopOverlay vsViewer else writeIORef toNavigate navigate
  cancelRun flag)

-- | Tell the page of each field whose text has changed since it was last
-- told (by a tool's handler, setting it), in the project's panel or a
-- tool's.
postFieldChanges :: IO ()
postFieldChanges = readIORef sessionRef >>= mapM_ (\Session{..} -> do
  fields <- ensembleFields sEnsemble
  project <- forM [ f | (ProjectConfig, f) <- fields ] $ \f ->
    (fieldName f, fieldName f, Map.lookup (fieldName f) sColors,) <$> fieldText f
  tools <- fmap concat . forM (Map.elems sViewers) $ \vs -> readIORef (vsTool vs) >>= \case
    Nothing -> pure []
    Just ActiveTool{..} -> forM atFields $ \f ->
      (toolKey vs (fieldName f), fieldName f, Map.lookup (fieldName f) atColors,) <$> fieldText f
  old <- readIORef textsRef
  forM_ (project ++ tools) $ \(key, _, color, text) -> when (Map.lookup key old /= Just text) $ do
    c <- maybe (pure Nothing) id color
    post $ Aeson.object $ [ "type" .= ("fieldText" :: Text.Text), "key" .= key, "text" .= text ]
                          ++ [ "color" .= x | Just x <- [c] ]
  writeIORef textsRef (Map.union (Map.fromList [ (key, text) | (key, _, _, text) <- project ++ tools ]) old))

-- | A point of the screen plane, as a point of the model (on the sphere,
-- if it's on the globe).
toModel :: Viewer -> (Double, Double) -> IO (Maybe (Double, Double))
toModel viewer p = getDynamic (vView viewer) >>= \case
  PlaneView -> pure (Just p)
  SphereView -> getDynamic (vRotation viewer) <&> \r -> screenToPlane r p

toolEvent :: Int -> Int -> Double -> Double -> Double -> Double -> IO ()
toolEvent k kind x1 y1 x2 y2 = withViewerState k $ \vs -> do
  a <- toModel (vsViewer vs) (x1, y1)
  b <- toModel (vsViewer vs) (x2, y2)
  let event = case (kind, a, b) of
        (0, Just p, _) -> Just (Click p)
        (1, Just p, _) -> Just (DoubleClick p)
        -- (a drag handler is given where the pointer is, then where it
        -- started, as the desktop app gives it)
        (2, Just p, Just q) -> Just (Drag q p)
        (3, Just p, Just q) -> Just (DragDone q p)
        _ -> Nothing
  forM_ event $ \ev -> fire vs ev >> postDrawings vs
  -- (done with it: the page sends drags no faster than they're handled)
  post $ Aeson.object [ "type" .= ("eventDone" :: Text.Text), "viewer" .= k ]

toolButton :: Int -> JSString -> IO ()
toolButton k label = withViewerState k $ \vs -> do
  void (fire vs (ButtonPressed (fromJSString label)))
  postDrawings vs

setToolField :: Int -> JSString -> JSString -> IO ()
setToolField k name text = withViewerState k $ \vs -> readIORef (vsTool vs) >>= mapM_ (\ActiveTool{..} -> do
  let n = fromJSString name
  result <- case [ f | f <- atFields, fieldName f == n ] of
    f : _ -> setFieldText f (fromJSString text)
    [] -> pure (Left ("no field named " ++ n))
  color <- maybe (pure Nothing) id (Map.lookup n atColors)
  post $ Aeson.object $
    [ "type" .= ("field" :: Text.Text), "key" .= toolKey vs n
    , "error" .= either Just (const Nothing) result ]
    ++ [ "color" .= c | Just c <- [color] ]
  -- (as the desktop app does, a tool hears that its values changed)
  when (isRight result) $ void (fire vs Refresh)
  postDrawings vs)

-- | Post the viewer's drawings, if they've changed (or always, on the
-- sphere, where they turn with the globe).
postDrawings :: ViewerState -> IO ()
postDrawings ViewerState{..} = do
  changed <- vDrawCmdsChanged vsViewer
  sphere <- (== SphereView) <$> getDynamic (vView vsViewer)
  when (changed || sphere) $ do
    layers <- vDrawCmds vsViewer
    proj <- viewProjection (vView vsViewer) (vRotation vsViewer)
    post $ Aeson.object
      [ "type" .= ("drawing" :: Text.Text), "viewer" .= vsId, "layers" .= drawingJSON proj layers ]

------------------------------------------------------------------------
-- The script editor

-- | Color an edited script, check it, and say how to write it with
-- mathematical symbols (see "Editor"), for the editor's request.
analyze :: Int -> JSString -> IO ()
analyze reqId text = guarded $ do
  choices <- readIORef sessionRef >>= maybe (pure Map.empty) (getDynamic . ensembleColorizerChoices . sEnsemble)
  post (analysisJSON reqId choices (fromJSString text))

symbols :: IO ()
symbols = guarded (post symbolsJSON)

-- | Build the project again from an edited script, as the desktop app's
-- Rebuild does: with the same colorizers, and everything about it that
-- isn't in its script carried over (values, tables, autocolor, ...). If
-- it doesn't build, the project stays as it was, and the error is posted:
--
-- > { "type": "rebuildError", "error": { "from", "to", "message" } }
rebuild :: JSString -> IO ()
rebuild text = guarded $ readIORef sessionRef >>= \case
  Nothing -> postError "there's no project to rebuild"
  Just Session{..} -> do
    let src = fromJSString text
    choices <- getDynamic (ensembleColorizerChoices sEnsemble)
    buildEnsembleWithColorizers choices src >>= \case
      Left err -> post $ Aeson.object [ "type" .= ("rebuildError" :: Text.Text), "error" .= errorJSON src err ]
      Right new -> do
        toolsBefore <- fmap Map.fromList . forM (Map.elems sViewers) $ \vs -> do
          title <- getDynamic (vTitle (vsViewer vs))
          (title,) . fmap atName <$> readIORef (vsTool vs)
        carryOverProjectState sEnsemble new
        startProject sLibrary sTitle sFileName True toolsBefore new

------------------------------------------------------------------------
-- The panel

-- | Each project field's text, by name.
projectFieldTexts :: Ensemble -> IO (Map.Map String String)
projectFieldTexts ens = do
  fields <- ensembleFields ens
  Map.fromList <$> forM [ f | (ProjectConfig, f) <- fields ] (\f -> (fieldName f,) <$> fieldText f)

-- | A layout, as JSON, with each field's text; and noting, in @colors@, how
-- to get each color field's value (as #rrggbb).
layoutJSON :: (String -> String) -> IORef (Map.Map String (IO (Maybe String))) -> Map.Map String String -> Layout -> IO Aeson.Value
layoutJSON keyOf colors texts = go
  where
    kind :: Text.Text -> (Aeson.Key, Aeson.Value)
    kind k = "kind" .= k
    label l = getDynamic l <&> \(Label s) -> s
    field k l name extra = do
      lbl <- label l
      getDynamic name >>= \case
        Left _ -> pure Aeson.Null
        Right n -> pure $ Aeson.object $
          [ kind k, "label" .= lbl, "name" .= n, "key" .= keyOf n, "text" .= Map.findWithDefault "" n texts ] ++ extra
    go = \case
      Vertical xs -> children "vertical" xs
      Horizontal xs -> children "horizontal" xs
      Panel t lo -> do
        title <- label t
        body <- go =<< getDynamic lo
        pure (Aeson.object [ kind "panel", "title" .= title, "body" .= body ])
      Tabbed items -> do
        tabs <- forM' items $ \TabItem{..} -> do
          l <- label tiLabel
          body <- go =<< getDynamic tiBody
          pure (Aeson.object [ "label" .= l, "body" .= body ])
        pure (Aeson.object [ kind "tabs", "tabs" .= tabs ])
      PlainText t -> getDynamic t <&> \s -> Aeson.object [ kind "plain", "text" .= s ]
      Button t -> getDynamic t <&> \l -> Aeson.object [ kind "button", "label" .= l ]
      ActionButton{} -> pure Aeson.Null
      HiddenTable{} -> pure Aeson.Null
      CheckBox l name _ -> field "check" l name []
      ColorPicker l name col -> do
        let hex = getDynamic col <&> either (const Nothing) (\c ->
              let (r, g, b) = colorToRGB c in Just (printf "#%02x%02x%02x" r g b))
        getDynamic name >>= either (const (pure ())) (\n -> modifyIORef' colors (Map.insert n hex))
        current <- hex
        field "color" l name [ "color" .= current ]
      Selection l name _ options -> do
        opts <- getDynamic options
        field "select" l name [ "options" .= opts ]
      TextBox l UIVariable{..} -> field "text" l exprName []
    children k xs = do
      cs <- forM' xs go
      pure (Aeson.object [ kind k, "children" .= cs ])
    forM' v f = getDynamic v >>= mapM f
