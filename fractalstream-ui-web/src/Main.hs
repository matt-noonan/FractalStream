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
* @fs_save()@: post the open project as a file (its script, changed
  values, and so on, as the desktop app saves it).

As the desktop app does (and unlike fsrun, which is for reproducible
runs), a project opens with the configuration values (@values:@) and the
tables (@tables:@) it was saved with, and is saved with its own.
* @fs_set(name, text)@: set a configuration field (by name) from its text.
* @fs_view(viewer, cx, cy, pixelSize, width, height)@: move, zoom, or
  resize a viewer (all at once, for one render).
* @fs_resize(viewer, width, height)@: resize a viewer.
* @fs_smoothing(on)@: whether viewers are smoothed (each pixel the average
  of four samples, as fsrun's captures are), for every viewer from now on.

and the engine posts JSON messages (@globalThis.fractalstreamPost@),
frames (@globalThis.fractalstreamPostFrame@: RGB pixels, with the view they
show), and saved files (@globalThis.fractalstreamPostFile@: a name and the
contents), which the worker forwards to the page.

Messages:

> { "type": "library", "sections": [ { "name", "entries": [ { "title", "description", "preview" } ] } ] }
> { "type": "project", "title", "fileName", "library": <whether it's from the library>, "panel": <layout> or null, "viewers": [ { "id", "title", "width", "height" } ] }
> { "type": "field", "name", "error": <message> or null }
> { "type": "error", "message" }

where a layout is @{ "kind": "vertical" | "horizontal", "children" }@,
@{ "kind": "panel", "title", "body" }@, @{ "kind": "tabs", "tabs": [ {
"label", "body" } ] }@, @{ "kind": "plain", "text" }@, or a field: @{
"kind": "text" | "check" | "select" | "color", "label", "name", "text",
"options" }@ (options for a selection).
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
import Actor.Viewer (Viewer(..), RenderedBuffer(..), renderViewerBuffer, onParameterChanges)
import Backend (withBackend, Backend)
import Data.DynamicValue
import FractalStream.Library (LibrarySection(..), LibraryEntry(..), librarySections)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, SomeAsyncException, try, displayException, fromException, throwIO)
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

-- | The open project: its viewers (by id), each with its render loop's
-- request signal; the threads to stop when it's closed.
data Session = Session
  { sViewers :: Map.Map Int (Viewer, MVar ())
  , sEnsemble :: Ensemble
  , sFileName :: String
  , sStop :: IO ()
  }

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
foreign export javascript "fs_save" save :: IO ()
foreign export javascript "fs_set" setField :: JSString -> JSString -> IO ()
foreign export javascript "fs_view" setView :: Int -> Double -> Double -> Double -> Int -> Int -> IO ()
foreign export javascript "fs_resize" resize :: Int -> Int -> Int -> IO ()
foreign export javascript "fs_smoothing" setSmoothing :: Bool -> IO ()

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

save :: IO ()
save = guarded $ readIORef sessionRef >>= \case
  Nothing -> postError "there's no project to save"
  Just Session{..} -> do
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
  close
  backend <- getBackend

  -- Run the project, collecting its panel and viewers.
  panelRef <- newIORef Nothing
  viewersRef <- newIORef []
  let ui = UI
        { newEnsemble = pure ()
        , makeLayout = \_ title lo -> writeIORef panelRef (Just (title, lo)) >> pure (pure ())
        , makeViewer = \_ _ _ _ _ viewer -> modifyIORef' viewersRef (viewer :) >> pure (pure ())
        }
  runEnsemble backend ui ensemble
  viewers <- zip [0 ..] . reverse <$> readIORef viewersRef

  -- A render loop for each viewer: it renders whenever asked (once for
  -- however many asks came while it was busy), and posts the frame.
  loops <- forM viewers $ \(k, viewer) -> do
    signal <- newMVar ()   -- (the first frame)
    tid <- forkIO (renderLoop k viewer signal)
    unwatch <- onParameterChanges viewer (requestRender signal)
    pure ((k, (viewer, signal)), (tid, unwatch))
  let stop = forM_ (map snd loops) $ \(tid, unwatch) -> killThread tid >> unwatch
  writeIORef sessionRef (Just (Session (Map.fromList (map fst loops)) ensemble fileName stop))

  fields <- projectFieldTexts ensemble
  panel <- readIORef panelRef >>= traverse (\(title, lo) -> do
    body <- layoutJSON fields lo
    pure (Aeson.object [ "kind" .= ("panel" :: Text.Text), "title" .= title, "body" .= body ]))
  viewerInfo <- forM viewers $ \(k, viewer) -> do
    title <- getDynamic (vTitle viewer)
    Dimensions (w, h) <- getDynamic (vSize viewer)
    pure (Aeson.object [ "id" .= k, "title" .= title, "width" .= w, "height" .= h ])
  post $ Aeson.object
    [ "type" .= ("project" :: Text.Text)
    , "title" .= projectTitle
    , "fileName" .= fileName
    , "library" .= fromLibrary
    , "panel" .= panel
    , "viewers" .= viewerInfo
    ]

-- | Stop the open project's render loops (if any).
close :: IO ()
close = readIORef sessionRef >>= mapM_ (\s -> sStop s >> writeIORef sessionRef Nothing)

requestRender :: MVar () -> IO ()
requestRender signal = void (tryPutMVar signal ())

renderLoop :: Int -> Viewer -> MVar () -> IO ()
renderLoop k viewer signal = forever $ do
  takeMVar signal
  smooth <- readIORef smoothingRef
  try (renderViewerBuffer smooth viewer) >>= \case
    -- (being stopped, when another project is opened, isn't an error: the
    -- loop just ends)
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> postError (displayException e)
    Right (Left err) -> postError err
    Right (Right RenderedBuffer{..}) ->
      withForeignPtr rbPixels $ \p ->
        postFrame k p (3 * rbWidth * rbHeight) rbWidth rbHeight
                  (fst rbCenter) (snd rbCenter) (fst rbPixelSize) (snd rbPixelSize)

setField :: JSString -> JSString -> IO ()
setField name text = guarded $ readIORef sessionRef >>= \case
  Nothing -> pure ()
  Just Session{..} -> do
    let n = fromJSString name
    fields <- ensembleFields sEnsemble
    result <- case [ f | (ProjectConfig, f) <- fields, fieldName f == n ] of
      f : _ -> setFieldText f (fromJSString text)
      [] -> pure (Left ("no field named " ++ n))
    post $ Aeson.object
      [ "type" .= ("field" :: Text.Text)
      , "name" .= n
      , "error" .= either Just (const Nothing) result
      ]

withViewer :: Int -> ((Viewer, MVar ()) -> IO ()) -> IO ()
withViewer k act = guarded $ readIORef sessionRef >>= \case
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
    mapM_ (\Session{..} -> mapM_ (requestRender . snd) (Map.elems sViewers))

resize :: Int -> Int -> Int -> IO ()
resize k w h = withViewer k $ \(viewer, signal) -> do
  setValue (vSize viewer) (Dimensions (max 1 w, max 1 h))
  requestRender signal

------------------------------------------------------------------------
-- The panel

-- | Each project field's text, by name.
projectFieldTexts :: Ensemble -> IO (Map.Map String String)
projectFieldTexts ens = do
  fields <- ensembleFields ens
  Map.fromList <$> forM [ f | (ProjectConfig, f) <- fields ] (\f -> (fieldName f,) <$> fieldText f)

layoutJSON :: Map.Map String String -> Layout -> IO Aeson.Value
layoutJSON texts = go
  where
    kind :: Text.Text -> (Aeson.Key, Aeson.Value)
    kind k = "kind" .= k
    label l = getDynamic l <&> \(Label s) -> s
    field k l name extra = do
      lbl <- label l
      getDynamic name >>= \case
        Left _ -> pure Aeson.Null
        Right n -> pure $ Aeson.object $
          [ kind k, "label" .= lbl, "name" .= n, "text" .= Map.findWithDefault "" n texts ] ++ extra
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
      Button{} -> pure Aeson.Null
      ActionButton{} -> pure Aeson.Null
      HiddenTable{} -> pure Aeson.Null
      CheckBox l name _ -> field "check" l name []
      ColorPicker l name _ -> field "color" l name []
      Selection l name _ options -> do
        opts <- getDynamic options
        field "select" l name [ "options" .= opts ]
      TextBox l UIVariable{..} -> field "text" l exprName []
    children k xs = do
      cs <- forM' xs go
      pure (Aeson.object [ kind k, "children" .= cs ])
    forM' v f = getDynamic v >>= mapM f
