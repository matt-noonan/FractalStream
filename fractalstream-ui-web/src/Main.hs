{-# LANGUAGE OverloadedStrings #-}
{- |
Module       : Main
Description  : The engine of FractalStream's web app

The core and the wasm backend, as a reactor module run by the page's
worker (@web/src/engine.ts@). The worker calls these exports:

* @fs_library()@: post the built-in library.
* @fs_open(section, entry)@: build that project; post its panel and
  viewers, then each viewer's frames as they're rendered.
* @fs_set(name, text)@: set a configuration field (by name) from its text.
* @fs_view(viewer, cx, cy, pixelSize)@: move or zoom a viewer.
* @fs_resize(viewer, width, height)@: resize a viewer.

and the engine posts JSON messages (@globalThis.fractalstreamPost@), and
frames (@globalThis.fractalstreamPostFrame@: RGB pixels, with the view they
show), which the worker forwards to the page.

Messages:

> { "type": "library", "sections": [ { "name", "entries": [ { "title", "description", "preview" } ] } ] }
> { "type": "project", "title", "panel": <layout> or null, "viewers": [ { "id", "title" } ] }
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

import Actor.Ensemble (Ensemble(..), runEnsemble, parseEnsembleFromBytes, ensembleFields, FieldOwner(..))
import Actor.Layout
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), RenderedBuffer(..), renderViewerBuffer, onParameterChanges)
import Backend (withBackend, Backend)
import Data.DynamicValue
import FractalStream.Library (LibrarySection(..), LibraryEntry(..), librarySections)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try, displayException)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString(..), toJSString, fromJSString)
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
  , sStop :: IO ()
  }

sessionRef :: IORef (Maybe Session)
sessionRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE sessionRef #-}

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
foreign export javascript "fs_set" setField :: JSString -> JSString -> IO ()
foreign export javascript "fs_view" setView :: Int -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_resize" resize :: Int -> Int -> Int -> IO ()

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
  close
  LibraryEntry{..} <- case drop section librarySections of
    s : _ | e : _ <- drop entry (lsEntries s) -> pure e
    _ -> fail "no such project"
  ensemble <- parseEnsembleFromBytes leFileName leContents >>= either fail pure
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
  writeIORef sessionRef (Just (Session (Map.fromList (map fst loops)) ensemble stop))

  fields <- projectFieldTexts ensemble
  panel <- readIORef panelRef >>= traverse (\(title, lo) -> do
    body <- layoutJSON fields lo
    pure (Aeson.object [ "kind" .= ("panel" :: Text.Text), "title" .= title, "body" .= body ]))
  viewerInfo <- forM viewers $ \(k, viewer) -> do
    title <- getDynamic (vTitle viewer)
    pure (Aeson.object [ "id" .= k, "title" .= title ])
  post $ Aeson.object
    [ "type" .= ("project" :: Text.Text)
    , "title" .= leTitle
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
  try (renderViewerBuffer False viewer) >>= \case
    Left (e :: SomeException) -> postError (displayException e)
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

setView :: Int -> Double -> Double -> Double -> IO ()
setView k cx cy pixelSize = withViewer k $ \(viewer, signal) -> do
  setValue (vCenter viewer) (cx, cy)
  setValue (vPixelSize viewer) pixelSize
  requestRender signal

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
