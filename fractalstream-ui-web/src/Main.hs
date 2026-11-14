{-# LANGUAGE OverloadedStrings #-}
{- |
Module       : Main
Description  : The engine of FractalStream's web app

The core and the wasm backend, as a reactor module run by the page's
worker (@web/src/engine.ts@). The worker calls these exports:

* @fs_library()@: post the built-in library.
* @fs_open(section, entry)@: build that project; post its panel and
  viewers, then each viewer's frames as they're rendered. A starting
  point (in the library's "Starting points") opens as a new, untitled
  project, without the starting point's title and description, and with
  its script editor open, as on the desktop.
* @fs_open_file(name, bytes)@: the same, for a project file's contents.
  A project whose script doesn't build still opens, with no viewers, for
  its script to be fixed (@"edit"@), as on the desktop. An old
  FractalStream 1.0 save file isn't opened: the engine says so
  (@ancient@), and keeps it for @fs_translate@.
* @fs_translate()@: translate that old file into a new project (with no
  file of its own yet), and open it, with its script editor and the
  translation's notes.
* @fs_pointer(viewer, x, y)@: what the viewer's "Show pointer position"
  shows for a screen-plane point (as for @fs_event@): posted as
  @pointer@.
* @fs_save(views, tables, withDraft, draft)@: post the open project as a
  file (its script, changed values, and so on, as the desktop app saves
  it), with the page's view state (JSON: see "web-properties" below); and
  what its tables hold (@tables@ 1), or not (0), or as its file had them
  (2: for a script that doesn't build, whose tables aren't there to save).
  If @withDraft@, the script saved is @draft@ (what's typed in the script
  editor, not rebuilt yet), as the desktop app saves its editor's.
  The project has no changes to save after this (@unsaved@).
* @fs_close()@: close the open project (the page has asked about its
  unsaved changes).
* @fs_share(views, withDraft, draft)@: what a link to the open project
  shows (@share@), with the page's view state (as for @fs_save@): for a
  library project whose script hasn't changed, its library id and its
  state (its values, views, colorizers and autocolor: see
  'projectStateKeys'); else its whole file (with what's typed in the
  script editor, if @withDraft@), less its tables and its preview.
* @fs_open_link(id, state)@: open a library project (by its id, as the
  library posts it) with that state (JSON), from a link.
* @fs_details()@: post the open project's title, description and
  preview (@details@), for the page's Edit project details.
* @fs_set_details(title, description)@: set them (an empty one: none; a
  project with no title of its own is called by its file's name); the
  project has changes to save.
* @fs_use_as_preview(viewer)@: make what the viewer shows the project's
  preview, as the desktop's "Use this as preview" does; posted as
  @details@ (or an error).
* @fs_unsaved(on)@: say whether the project has changes to save (the
  page says so again when a saved file couldn't be written).
* @fs_table_summary()@: post the open project's tables that hold anything
  (@tableSummary@), so that the page can ask whether to save them, as the
  desktop does.
* @fs_disk_drag(viewer, fx, fy, tx, ty)@: move a disk viewer's view by the
  hyperbolic translation that keeps the point under the pointer, as it's
  dragged from one point of the screen plane to another (the disk's
  drawing, in its radii, y up).
* @fs_rotate(viewer, fx, fy, tx, ty)@: turn a sphere viewer's globe,
  trackball-style, as the pointer drags from one point of the screen to
  another (in units of the sphere's radius, from its center, y up).

As the desktop app does (and unlike fsrun, which is for reproducible
runs), a project opens with the configuration values (@values:@) and the
tables (@tables:@) it was saved with, and is saved with its own.
* @fs_set(name, text)@: set a configuration field (by name) from its text.
* @fs_view(viewer, cx, cy, pixelSize, width, height, left, top, right,
  bottom)@: move, zoom, or resize a viewer (all at once, for one render).
  The rectangle (in the new view's pixels, the right and bottom not
  included; empty for none) is where the page still shows the last
  picture, moved into the new view: the render's pieces there skip the
  coarse passes (see "Frames" below).
* @fs_resize(viewer, width, height)@: resize a viewer.
* @fs_density(d)@: how many canvas pixels the page draws a CSS pixel
  with: tools and overlays are given a CSS pixel's size, as their
  @pixel_size@.
* @fs_smoothing(on)@: whether viewers are smoothed (each pixel the average
  of four samples, as fsrun's captures are) to begin with: each new
  viewer's choice, until @fs_viewer_smoothing(viewer, on)@ changes it
  ("Use subpixel smoothing", per viewer, as on the desktop; not saved).
* @fs_sphere_shading(s)@: how strongly sphere viewers shade the globe,
  from 0 to 1 (a preference: the page's, as the desktop's
  @sphere-shading@ setting); they draw again.
* @fs_navigate(viewer, what)@: 0 the last view, 1 the next one (in the
  viewer's history), 2 its original view (the script's, of the kind it
  shows now: plane or sphere), 3 a complex viewer on the sphere instead of
  the plane, or back. The engine posts the view it goes to (@view@).
* @fs_record(viewer)@: the viewer's view, as it is now, is a step in its
  history (the page says so at the end of a gesture: a drag, a zoom).
* @fs_aspect(viewer, text)@: a real viewer's aspect ratio (a pixel's
  height over its width, as typed: @2@, @1/2@), keeping its center and a
  pixel's width, as the desktop does: answered with @aspect@ (an error,
  or none). Saved in @web-properties:@ (@aspect@), when it isn't the
  script's.
* @fs_autocolor_watch(on)@: whether the page's autocolor editor is open:
  while it is, the project's autocolor groups are posted (@autocolor@)
  whenever they change (as renders add points, say), a few times a second
  at most.
* @fs_autocolor_edit(edit)@: an edit to a group (JSON: @{ "op", "group",
  ... }@): @locked@ and @blend@ (with @on@), @tolerance@ (with its
  @text@), @miss@ (a @color@, #rrggbb), @color@ (a point's: @x@, @y@,
  @color@), @move@ (a point, @x@ and @y@, to @text@), @delete@ (@x@, @y@),
  @add@ (@text@), @clear@. A point or tolerance that can't be read is said
  so (@autocolorError@), as the desktop's window does.
* @fs_capture_image(viewer, multiplier, smooth)@: a picture of the viewer,
  as the desktop's Capture > Image... makes it: its view, at its size (in
  CSS pixels) times the multiplier, smoothed or not, as a PNG, posted to
  the page to save (@fractalstreamPostDownload@).
* Movies, as the desktop's Capture > Movie... makes them from a script:
  @fs_movie_template(viewer)@ posts a script to start with
  (@movieTemplate@); @fs_movie_check(id, script)@ posts whether it's
  one (@movieCheck@, with the id); @fs_movie_start(viewer, script, frames,
  fps, loop, smooth, drawings)@ records it: each frame, the script runs
  (with @t@ from 0 to 1) and a copy of the viewer (at its size, made even,
  as H.264 needs) is rendered and posted (@fractalstreamPostMovieFrame@),
  no more than two ahead of the page's @fs_movie_ack(frame)@ (the page
  encodes them); then @movieDone@. @fs_movie_stop()@ stops it. The
  project's values are put back afterwards, and the viewers don't redraw
  while it's recorded (the script changes the values every frame).
  @fs_movie_path_start(viewer, drawnIn, path, frames, fps, loop, smooth,
  drawings, corner)@ records one as the desktop's "Animate along path"
  tool makes it: the coordinate of the viewer @drawnIn@ (a configuration
  variable: see "animates", in a viewer's info) moves along the path (a
  JSON list of [x, y] points, in its screen plane, as the page draws it),
  at a steady speed, back to the start if it's to loop; and, unless the
  corner is -1, @drawnIn@ is shown as an inset in that corner (0 top
  left, 1 top right, 2 bottom left, 3 bottom right), marking the point.
* @fs_jump(viewer, x, y, pixelSize)@: go to a view: centered on (x, y)
  (on the sphere, the globe turned to face it), with a pixel this size
  (a CSS pixel's). @fs_where(viewer)@ posts where it is now, so (@where@).
* @fs_colorizer(viewer, name)@: use another colorizer for the viewer,
  rebuilding the project, as the desktop does; one whose parameters the
  viewer can't fill in asks for its arguments first (@colorizerArguments@,
  answered with @fs_colorizer_answers(viewer, name, answers)@, a JSON list
  of strings). @fs_colorizer_arguments(viewer)@ asks for the arguments of
  the one in use.
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
a viewer's render, as it goes (@globalThis.fractalstreamPostPatch@: see
"Frames" below), and saved files (@globalThis.fractalstreamPostFile@: a name and the
contents), which the worker forwards to the page.

Messages:

> { "type": "library", "sections": [ { "name", "startingPoints", "entries": [ { "title", "description", "preview", "id" } ] } ] }
> { "type": "project", "title", "fileName", "library": <whether it's from the library>, "new": <whether it's a new project, with no file of its own yet (a starting point's, or a translation)>, "edit": <whether to open its script editor>, "notes": [ <a translation's notes> ], "panel": <layout> or null, "viewers": [ { "id", "title", "width", "height", "sphere", "disk", "halfPlane", "tools": [ { "name", "shortcut", "help" } ] } ], "views": <the file's web-properties> or null }
> { "type": "share", "library": <a library project's id> or null, "state": <its state> or null, "file": <the whole project file> or null, "fileName" }
> { "type": "details", "title", "description", "preview": <a PNG, base64-encoded> or null, "fileName", "fromViewer": <whether it's from "Use this as preview"> }
> { "type": "unsaved", "on" }   (whether the open project has changes to save, as the desktop app counts them, whenever that changes)
> { "type": "ancient", "name" }   (an old FractalStream file, to translate with fs_translate, if the user wants)
> { "type": "view", "viewer", "cx", "cy", "px", "py", "sphere", "disk", "halfPlane" }   (the engine moved the view: a pixel's size in the canvas's pixels)
> { "type": "where", "viewer", "x", "y", "pixelSize" }
> { "type": "aspect", "viewer", "error": <why it isn't one> or null }
> { "type": "movieTemplate", "viewer", "script" }
> { "type": "movieCheck", "id", "error": { "from", "to", "message" } or null }
> { "type": "movieDone", "error": <why it failed> or null, "stopped", "issues": <what the script ran into> or null }
> { "type": "tableSummary", "built": <whether the script builds>, "tables": [ { "name", "size": [ <each dimension> ], "bytes" } ] }
> { "type": "autocolor", "groups": [ { "id", "locked", "blend", "tolerance": <text>, "miss": <#rrggbb>,
>   "entries": [ { "x", "y", "point": <text>, "color": <#rrggbb> } ] } ] }
> { "type": "autocolorError", "message" }
> { "type": "colorizerArguments", "viewer", "name", "fields": [ { "param", "note", "text" } ] }

A viewer's info has, too: "complex" (whether it can show the sphere),
"smooth", and "colorizer": null, or { "inUse", "arguments": <whether the
one in use has parameters>, "items": [ { "name", "label", "description",
"inUse", "asks": <whether choosing it asks for arguments> } ] }.
> { "type": "pointer", "viewer", "text" }
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

Frames: a viewer's image is rendered progressively, as the desktop app's
are ('renderProgressively'), in 'webPieceSize' pieces: coarse passes
first, over what the page doesn't show yet, then each piece at full
resolution, then (smoothed) at twice that. As pieces are drawn, the
engine posts patches (no more often than 'patchInterval'): the render's
number (per viewer), the view and size of its image, the rectangles drawn
since the last patch and their pixels; the last patch says the render's
done. Every render holds its first patch back for 'renderHold' (as the
desktop app holds a render of new parameters): one that's done by then
comes whole, in one patch, so a quick render shows no coarse passes and
looks just as a whole frame would. A render that's no longer wanted
(another one's been asked for) is dropped where it is, and its patches
stop.

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

import Actor.Ensemble (Ensemble(..), runEnsemble, loadProjectBytes, LoadedProject(..), ensembleFields, FieldOwner(..), ensembleCommonDefs,
                       encodeEnsemble, readProjectMetadata, changedValues, applySavedValues, ensembleTables, watchValues,
                       projectStateKeys, withProjectKeys, rewriteProjectFile, projectScript)
import Actor.SavedValues (readSavedValues, writeSavedValues)
import Actor.SavedTables (readSavedTables, writeSavedTables, captureTables, applySavedTables, TableSummary(..), summarizeTables)
import qualified Data.Aeson.KeyMap as KM
import Actor.Metadata (ProjectMetadata(..), noMetadata)
import Language.Legacy.Binary (mightBeAncientFS, decodeAncientFS)
import Language.Legacy.Translate (Translation(..), translateLegacyScript)
import Actor.Layout
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), RenderedBuffer(..), renderProgressively, Progressive(..), ImagePiece(..), onParameterChanges, viewProjection,
                     pointerText)
import Actor.Event (ViewKind(..), hyperbolicDisplay, hyperbolicModel, Event(..), ToolDispatcher, EventArgument_, HandlerRun(..), HandlerCancelled(..),
                    CancelFlag, newCancelFlag, cancelRun)
import Actor.Viewer.Sphere (sphereShading, trackballRotation, screenToPlane, Rotation(..), rotationFacing, rotateBy, sphereToPlane)
import Actor.Viewer.Disk (Isometry(..), HyperbolicDisplay(..), normalizeIsometry, inModel,
                          screenToModelH, dragOnDisplay, displayScreen, axisAtBottom, viewShowing)
import Actor.Viewer.ViewModel (Model(..), ModelShape(..), modelRotation, modelIsometry, History(..), historyBack, historyForward, historyAppend)
import Actor.Colorizer (Colorizer(..), ViewerColorizer(..), choiceName, choiceArguments, missingArguments,
                        argumentFields, choiceWithArguments)
import Actor.Event (Coordinate(..), showCoordinate)
import Data.List (find)
import Language.Value.Parser (parseConstant)
import Language.Type (TypeProxy(RealType, ComplexType))
import Actor.Autocolor (AutocolorGroup(..), AutocolorGroupId(..), autocolorGroups, showAutocolorPoint,
                        setAutocolorLocked, setAutocolorBlendMisses, setAutocolorTolerance, setAutocolorMissColor,
                        setAutocolorEntryColor, moveAutocolorEntry, deleteAutocolor, addAutocolorEntry,
                        clearAutocolorGroup)
import Data.Color (Color, rgbToColor)
import Data.Char (isHexDigit, digitToInt, isSpace, isAlphaNum, toLower)
import Actor.Viewer.Preview (renderPreview)
import Actor.Movie (MovieSettings(..), defaultMovieSettings, movieFrameTimes, scriptAnimation, checkMovieScript,
                    movieScriptTemplate, complexVariables, withFieldsRestored, renderViewerImage, Animation,
                    pathAnimation, pathPoint, canAnimate, MovieInset(..), prepareInset, withInset)
import Actor.Viewer (cloneViewer, withRenderedViewerBuffer, describeViewerIssues)
import Language.Code.Parser (noCommonDefs)
import Data.PNG (bufferToPng)
import Codec.Picture (Image(..))
import qualified Data.Vector.Storable as VS
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
import FractalStream.Metadata (gitTag, gitDirty)
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
import System.IO.Error (isUserError, ioeGetErrorString)
import GHC.Clock (getMonotonicTime)
import qualified Data.Set as Set
import Foreign.Marshal.Array (withArrayLen)
import Task.Concurrent (renderConcurrency)

------------------------------------------------------------------------
-- Talking to the page

foreign import javascript unsafe "globalThis.fractalstreamPost($1)"
  postRaw :: JSString -> IO ()

-- | A patch of a viewer's render: the viewer, the render's number, its
-- image's width and height and view (center, and a pixel's width and
-- height), the image's RGB pixels (read from, for the rectangles), the
-- rectangles drawn (x, y, width, height each), and whether the render's
-- done. (The worker copies the rectangles' pixels out at once.)
foreign import javascript unsafe
  "globalThis.fractalstreamPostPatch($1, $2, $3, $4, $5, $6, $7, $8, new Uint8Array(__exports.memory.buffer, $9, 3 * $3 * $4), new Int32Array(__exports.memory.buffer, $10, 4 * $11).slice(), $12)"
  postPatch :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Double
            -> Ptr Word8 -> Ptr Int32 -> Int -> Bool -> IO ()

-- | A file for the page to save as it is (a picture): its name, and its
-- contents.
foreign import javascript unsafe
  "globalThis.fractalstreamPostDownload($1, new Uint8Array(__exports.memory.buffer, $2, $3).slice())"
  postDownload :: JSString -> Ptr Word8 -> Int -> IO ()

-- | A movie's frame: its number (from 1), how many there are, its width
-- and height, and its RGB pixels.
foreign import javascript unsafe
  "globalThis.fractalstreamPostMovieFrame($1, $2, $3, $4, new Uint8Array(__exports.memory.buffer, $5, 3 * $3 * $4))"
  postMovieFrame :: Int -> Int -> Int -> Int -> Ptr Word8 -> IO ()

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
  -- (a 'fail' says its message, without "user error (...)" around it)
  Left (e :: SomeException)
    | Just io <- fromException e, isUserError io -> postError (ioeGetErrorString io)
    | otherwise -> postError (displayException e)
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
  , sOpening :: Opening
    -- ^ (its title, and how it was opened: for a rebuild)
  }

-- | How a project was opened: from a file, from the library (or one of
-- its starting points, as a new project), or as the translation of an old
-- FractalStream file (with the translation's notes).
data Opening = FromFile | FromLibrary | FromStartingPoint | Translated [String]
  deriving Eq

-- | An old FractalStream file, waiting for @fs_translate@: its name and
-- contents.
ancientRef :: IORef (Maybe (String, BS.ByteString))
ancientRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE ancientRef #-}

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
  , vsCovered :: IORef (Maybe (Int, Int, Int, Int))
    -- ^ where the page shows the last picture, for the next render (see
    --   'setView'): Nothing, once anything else (new parameters) asks for
    --   a render
  , vsRenders :: IORef Int
    -- ^ its renders' number (the latest's)
  , vsPictureIso :: IORef (Maybe Isometry)
    -- ^ in the disk, the view of the last render done, which is the picture
    --   the page shows: a render takes the page's 'vsCovered' only if the
    --   view's still that (a drag moves the whole picture)
  , vsSmooth :: IORef Bool
    -- ^ whether it's smoothed ("Use subpixel smoothing")
  , vsHistory :: IORef (Maybe History)
    -- ^ its views, for "Last view" and "Next view" (none yet, until a
    --   step is recorded)
  }

-- | What a viewer keeps through a rebuild of the project (by its title):
-- its tool, view (and size), smoothing, and history.
data Carried = Carried
  { cTool :: Maybe String
  , cModel :: Model
  , cSize :: Dimensions
  , cSmooth :: Bool
  , cHistory :: Maybe History
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

-- | Whether the open project has changes to save, as the desktop app
-- counts them: a configuration value or a table changed, a rebuild (from
-- an edited script, or another colorizer), an autocolor edit; or it's a
-- translation, not yet saved. (Not where its windows are, nor its views.)
-- The page is told when it changes ("unsaved"), to ask before closing it.
unsavedRef :: IORef Bool
unsavedRef = unsafePerformIO (newIORef False)
{-# NOINLINE unsavedRef #-}

-- | Set whether the project has changes to save (the page sets it again
-- when writing a saved file fails), and tell the page.
setUnsaved :: Bool -> IO ()
setUnsaved u = do
  writeIORef unsavedRef u
  post (Aeson.object [ "type" .= ("unsaved" :: Text.Text), "on" .= u ])

-- | Note a change to save (telling the page only the first time). Not
-- while a movie's recorded: it changes the values each frame, and puts
-- them back afterwards (as the desktop has it).
markUnsaved :: IO ()
markUnsaved = readIORef recordingRef >>= \recording -> unless recording $ do
  was <- atomicModifyIORef' unsavedRef (\u -> (True, u))
  unless was (setUnsaved True)

-- | Whether frames are smoothed (see 'renderViewerBuffer').
-- | How many canvas pixels the page draws a CSS pixel with (2, on a
-- high-DPI screen). A viewer's pixel size is a canvas pixel's; its code,
-- and its tools and overlays, are given a CSS pixel's ('cssPixelDims',
-- 'renderViewerBufferAt'), so that what a script sizes in @pixel_size@
-- comes out as big as on the desktop, whose pixels are the screen's
-- logical ones.
densityRef :: IORef Double
densityRef = unsafePerformIO (newIORef 1)
{-# NOINLINE densityRef #-}

-- | A viewer's pixel's width and height, in a CSS pixel (see 'densityRef').
cssPixelDims :: Viewer -> IO (Double, Double)
cssPixelDims viewer = do
  d <- readIORef densityRef
  px <- getDynamic (vPixelSize viewer)
  aspect <- getDynamic (vAspect viewer)
  pure (px * d, px * d * aspect)

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
foreign export javascript "fs_translate" translateAncient :: IO ()
foreign export javascript "fs_pointer" pointer :: Int -> Double -> Double -> IO ()
foreign export javascript "fs_save" save :: JSString -> Int -> Bool -> JSString -> IO ()
foreign export javascript "fs_close" closeProject :: IO ()
foreign export javascript "fs_share" share :: JSString -> Bool -> JSString -> IO ()
foreign export javascript "fs_open_link" openLink :: JSString -> JSString -> IO ()
foreign export javascript "fs_details" details :: IO ()
foreign export javascript "fs_set_details" setDetails :: JSString -> JSString -> IO ()
foreign export javascript "fs_use_as_preview" useAsPreview :: Int -> IO ()
foreign export javascript "fs_unsaved" setUnsaved :: Bool -> IO ()
foreign export javascript "fs_table_summary" tableSummary :: IO ()
foreign export javascript "fs_rotate" rotate :: Int -> Double -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_disk_drag" diskDrag :: Int -> Double -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_set" setField :: JSString -> JSString -> IO ()
foreign export javascript "fs_view" setView :: Int -> Double -> Double -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign export javascript "fs_resize" resize :: Int -> Int -> Int -> IO ()
foreign export javascript "fs_smoothing" setSmoothing :: Bool -> IO ()
foreign export javascript "fs_density" setDensity :: Double -> IO ()
foreign export javascript "fs_sphere_shading" setSphereShading :: Double -> IO ()
foreign export javascript "fs_viewer_smoothing" setViewerSmoothing :: Int -> Bool -> IO ()
foreign export javascript "fs_navigate" navigateTo :: Int -> Int -> IO ()
foreign export javascript "fs_record" record :: Int -> IO ()
foreign export javascript "fs_jump" jump :: Int -> Double -> Double -> Double -> IO ()
foreign export javascript "fs_capture_image" captureImage :: Int -> Double -> Bool -> IO ()
foreign export javascript "fs_movie_template" movieTemplate :: Int -> IO ()
foreign export javascript "fs_movie_check" movieCheck :: Int -> JSString -> IO ()
foreign export javascript "fs_movie_start" movieStart :: Int -> JSString -> Int -> Int -> Bool -> Bool -> Bool -> IO ()
foreign export javascript "fs_movie_path_start" moviePathStart :: Int -> Int -> JSString -> Int -> Int -> Bool -> Bool -> Bool -> Int -> IO ()
foreign export javascript "fs_movie_ack" movieAck :: Int -> IO ()
foreign export javascript "fs_movie_stop" movieStop :: IO ()
foreign export javascript "fs_autocolor_watch" autocolorWatch :: Bool -> IO ()
foreign export javascript "fs_autocolor_edit" autocolorEdit :: JSString -> IO ()
foreign export javascript "fs_aspect" setAspect :: Int -> JSString -> IO ()
foreign export javascript "fs_where" whereIs :: Int -> IO ()
foreign export javascript "fs_colorizer" chooseColorizer :: Int -> JSString -> IO ()
foreign export javascript "fs_colorizer_arguments" colorizerArguments :: Int -> IO ()
foreign export javascript "fs_colorizer_answers" colorizerAnswers :: Int -> JSString -> JSString -> IO ()
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
    -- (which FractalStream this is, as the desktop's About box says it:
    -- its tag, and the commits since, as git describes it)
  , "version" .= (gitTag ++ (if gitDirty then "-dirty" else ""))
  , "sections" .=
      [ Aeson.object
          [ "name" .= lsName s
          , "startingPoints" .= lsStartingPoints s
          , "entries" .=
              [ Aeson.object
                  [ "title" .= leTitle e
                  , "description" .= leDescription e
                  , "preview" .= fmap (Text.decodeUtf8 . B64.encode) (lePreview e)
                  , "id" .= libraryId s e
                  ]
              | e <- lsEntries s ]
          ]
      | s <- librarySections ]
  ]

-- | A library project's id, for links: its section's name and its file's,
-- as words joined by dashes (@quadratic-family/external-rays@).
libraryId :: LibrarySection -> LibraryEntry -> String
libraryId s e = slug (lsName s) ++ "/" ++ takeBaseName (leFileName e)
  where
    slug = intercalate "-" . words . map (\c -> if isAlphaNum c then toLower c else ' ') . filter (/= '\'')

-- | The library project with this id, and its section, if there is one.
libraryEntryById :: String -> Maybe (LibrarySection, LibraryEntry)
libraryEntryById ident = find (\(s, e) -> libraryId s e == ident) [ (s, e) | s <- librarySections, e <- lsEntries s ]

open :: Int -> Int -> IO ()
open section entry = guarded $ do
  LibraryEntry{..} <- case drop section librarySections of
    s : _ | e : _ <- drop entry (lsEntries s) -> pure e
    _ -> fail "no such project"
  if leStartingPoint
    then openProject FromStartingPoint "Untitled" "Untitled.fs" leContents
    else openProject FromLibrary leTitle leFileName leContents

openFile :: JSString -> JSVal -> IO ()
openFile name bytes = guarded $ do
  n <- byteLength bytes
  contents <- allocaBytes (max 1 n) $ \p -> copyBytes bytes p >> BS.packCStringLen (castPtr p, n)
  let fileName = fromJSString name
  if mightBeAncientFS contents
    then do
      -- (the page asks whether to translate it)
      writeIORef ancientRef (Just (fileName, contents))
      post (Aeson.object [ "type" .= ("ancient" :: Text.Text), "name" .= fileName ])
    else openProject FromFile (titleOf fileName contents) fileName contents

-- | A project's title: its metadata's, if it has one; else its file's name.
titleOf :: String -> BS.ByteString -> String
titleOf fileName contents =
  either (const (takeBaseName fileName)) (fromMaybe (takeBaseName fileName) . mdTitle)
         (readProjectMetadata contents)

-- | Translate the old FractalStream file the page was told about, and open
-- the translation, as the desktop app does: as a new project (so saving
-- it doesn't write over the old file), with its script editor open, and
-- the translation's notes.
translateAncient :: IO ()
translateAncient = guarded $ readIORef ancientRef >>= \case
  Nothing -> postError "there's no old FractalStream file to translate"
  Just (fileName, contents) -> do
    writeIORef ancientRef Nothing
    (title, body) <- either (\err -> fail ("I was not able to decode " ++ show fileName ++ ". " ++ err)) pure
                       (decodeAncientFS contents)
    Translation{..} <- either fail pure (translateLegacyScript title body)
    let translated = Text.encodeUtf8 (Text.pack translatedProject)
        newName = takeBaseName fileName ++ ".fs"
    openProject (Translated translationWarnings) (titleOf newName translated) newName translated

-- | What "Show pointer position" shows for a screen-plane point.
pointer :: Int -> Double -> Double -> IO ()
pointer k x y = withViewerState k $ \vs -> do
  p <- toModel (vsViewer vs) (x, y)
  post $ Aeson.object [ "type" .= ("pointer" :: Text.Text), "viewer" .= k
                      , "text" .= pointerText (vCoordinate (vsViewer vs)) p ]

-- | The open project's tables that hold anything, for the page to ask
-- whether to save them (see 'save').
tableSummary :: IO ()
tableSummary = guarded $ readIORef sessionRef >>= mapM_ (\Session{..} -> do
  tables <- if ensembleBuilt sEnsemble then ensembleTables sEnsemble >>= summarizeTables else pure []
  post $ Aeson.object
    [ "type" .= ("tableSummary" :: Text.Text), "built" .= ensembleBuilt sEnsemble
    , "tables" .= [ Aeson.object [ "name" .= tsName t, "size" .= tsSize t, "bytes" .= tsBytes t ] | t <- tables ] ])

save :: JSString -> Int -> Bool -> JSString -> IO ()
save views withTables withDraft draft = guarded $ readIORef sessionRef >>= \case
  Nothing -> postError "there's no project to save"
  Just Session{..} -> do
    -- (what's typed in the script editor, if it's not been rebuilt, as
    -- the script: as the desktop app saves its editor's)
    when withDraft $ setValue' (ensembleSourceScript sEnsemble) (fromJSString draft)
    keepState sViewers sEnsemble views
    -- (what its tables hold, if the page says so: see 'tableSummary')
    case withTables of
      1 -> do
        tables <- ensembleTables sEnsemble >>= captureTables
        modifyValue (ensembleFrontendProperties sEnsemble) (writeSavedTables tables)
      0 -> modifyValue (ensembleFrontendProperties sEnsemble) (KM.delete "tables")
      _ -> pure ()
    contents <- encodeEnsemble sEnsemble
    setUnsaved False
    BS.useAsCStringLen contents $ \(p, n) -> postFile (toJSString sFileName) (castPtr p) n

-- | Keep the page's view state (under its own key), and the values the
-- project's been left with, in its frontends' properties: to be saved.
keepState :: Map.Map Int ViewerState -> Ensemble -> JSString -> IO ()
keepState viewers ens views = do
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack (fromJSString views))) of
    Left err -> postError ("couldn't save the view: " ++ err)
    Right (v :: Aeson.Value) -> withViews viewers v >>= \v' ->
      modifyValue (ensembleFrontendProperties ens) (KM.insert "web-properties" v')
  changedValues ens >>= modifyValue (ensembleFrontendProperties ens) . writeSavedValues

-- | What a link to the open project shows (see @fs_share@).
share :: JSString -> Bool -> JSString -> IO ()
share views withDraft draft = guarded $ readIORef sessionRef >>= mapM_ (\Session{..} -> do
  keepState sViewers sEnsemble views
  contents <- encodeEnsemble sEnsemble
  script <- getDynamic (ensembleSourceScript sEnsemble)
  let script' = if withDraft then fromJSString draft else script
      -- (a library project, its script as the library has it)
      fromLibrary = case sOpening of
        FromLibrary -> find (\(_, e) -> leFileName e == sFileName && projectScript (leContents e) == Right script')
                            [ (s, e) | s <- librarySections, e <- lsEntries s ]
        _ -> Nothing
      withoutPreview = KM.mapWithKey (\k v -> case v of
        Aeson.Object m | k == "metadata" -> Aeson.Object (KM.delete "preview" m)
        _ -> v)
  case fromLibrary of
    Just (s, e) -> do
      keys <- either fail pure (projectStateKeys contents)
      post $ Aeson.object [ "type" .= ("share" :: Text.Text), "library" .= libraryId s e, "state" .= Aeson.Object keys
                          , "file" .= Aeson.Null, "fileName" .= sFileName ]
    Nothing -> do
      file <- either fail pure (rewriteProjectFile (const script') (withoutPreview . KM.delete "tables") contents)
      post $ Aeson.object [ "type" .= ("share" :: Text.Text), "library" .= Aeson.Null, "state" .= Aeson.Null
                          , "file" .= Text.decodeUtf8 file, "fileName" .= sFileName ])

-- | Open a library project, with a link's state (see @fs_open_link@).
openLink :: JSString -> JSString -> IO ()
openLink ident linkState = guarded $ case libraryEntryById (fromJSString ident) of
  Nothing -> fail ("The library has no project “" ++ fromJSString ident ++ "” (any more), so the link can't be opened.")
  Just (_, LibraryEntry{..}) -> do
    keys <- case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack (fromJSString linkState))) of
      Right (Aeson.Object o) -> pure o
      _ -> fail "The link's project state couldn't be read."
    contents <- either (fail . ("The link's project " ++)) pure (withProjectKeys keys leContents)
    openProject FromLibrary leTitle leFileName contents

-- | Build a project from a file's contents, and start showing it.
--
-- A project whose script doesn't build still opens, as the desktop app
-- does: with no viewers, and its script editor open, to fix it; its
-- viewers come, with their saved values, tables and windows, when it's
-- rebuilt ('carryOverProjectState' puts them in place).
openProject :: Opening -> String -> String -> BS.ByteString -> IO ()
openProject how projectTitle fileName contents = do
  ensemble <- loadProjectBytes fileName contents >>= \case
    Left err -> fail err
    Right (Built ens) -> pure ens
    Right (BrokenScript ens _) -> pure ens
  -- (a starting point's title, description and preview are its own, not
  -- the new project's)
  when (how == FromStartingPoint) $ setValue' (ensembleMetadata ensemble) noMetadata
  -- The values and tables it was saved with, before anything is drawn
  -- (any that no longer fit are skipped, and said so on the console).
  when (ensembleBuilt ensemble) $ do
    props <- getDynamic (ensembleFrontendProperties ensemble)
    either (\err -> putStrLn ("Ignoring unreadable `values:`: " ++ err))
           (applySavedValues ensemble >=> mapM_ putStrLn) (readSavedValues props)
    either (\err -> putStrLn ("Ignoring unreadable `tables:`: " ++ err))
           (\saved -> ensembleTables ensemble >>= \ts -> applySavedTables ts saved >>= mapM_ putStrLn)
           (readSavedTables props)
  startProject how projectTitle fileName False Map.empty ensemble

-- | Show a project, in place of the open one (if any): run it, and post
-- its panel and viewers. On a rebuild (from an edited script), each
-- viewer the new project still has (by its title) gets the tool it had,
-- if it still has one by that name (or none, if it had none).
startProject :: Opening -> String -> String -> Bool -> Map.Map String Carried -> Ensemble -> IO ()
startProject how projectTitle fileName rebuilt carried ensemble = do
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
  -- (a script that doesn't build has nothing to run yet)
  when (ensembleBuilt ensemble) $ runEnsemble backend ui ensemble
  viewers <- zip [0 ..] . reverse <$> readIORef viewersRef

  -- A render loop for each viewer: it renders whenever asked (once for
  -- however many asks came while it was busy), and posts the frame.
  saved <- KM.lookup "web-properties" <$> getDynamic (ensembleFrontendProperties ensemble)
  defaultSmooth <- readIORef smoothingRef
  loops <- forM viewers $ \(k, viewer) -> do
    title <- getDynamic (vTitle viewer)
    -- (what it had before a rebuild; else, on the sphere or the plane, as
    -- its file saved it)
    let before = Map.lookup title carried
    case before of
      Just Carried{..} -> setValue (vSize viewer) cSize >> setViewerModel viewer cModel
      Nothing -> applySavedView viewer saved title
    smooth <- newIORef (maybe defaultSmooth cSmooth before)
    history <- newIORef (cHistory =<< before)
    signal <- newMVar ()   -- (the first frame)
    tool <- newIORef Nothing
    lock <- newMVar ()
    draw <- newEmptyMVar
    toolRefresh <- newIORef False
    running <- newIORef Nothing
    covered <- newIORef Nothing
    renders <- newIORef 0
    pictureIso <- newIORef Nothing
    let vs = ViewerState k viewer signal tool draw toolRefresh running lock covered renders pictureIso smooth history
    tid <- forkIO (renderLoop vs)
    drawTid <- forkIO (drawingLoop vs)
    unwatch <- onParameterChanges viewer $ do
      writeIORef covered Nothing
      requestRender signal
    -- (as the desktop app does, the overlay's refreshed when any
    -- configuration value changes, whether or not the viewer re-renders)
    unlisten <- vListenOverlay viewer (void (tryPutMVar draw ()))
    pure ((k, vs), (tid, killThread drawTid >> unwatch >> unlisten >> stopTimers vs))
  unwatchValues <- newIORef (pure ())
  let stop = do
        forM_ (map snd loops) $ \(tid, unwatch) -> killThread tid >> unwatch
        join (readIORef unwatchValues)
  context <- readIORef contextRef

  fields <- projectFieldTexts ensemble
  writeIORef textsRef fields
  colorsRef <- newIORef Map.empty
  panel <- readIORef panelRef >>= traverse (\(title, lo) -> do
    body <- layoutJSON id colorsRef fields lo
    pure (Aeson.object [ "kind" .= ("panel" :: Text.Text), "title" .= title, "body" .= body ]))
  colors <- readIORef colorsRef
  writeIORef sessionRef (Just (Session (Map.fromList (map fst loops)) context ensemble fileName colors stop
                                       projectTitle how))
  viewerInfo <- forM viewers $ \(k, viewer) -> do
    title <- getDynamic (vTitle viewer)
    Dimensions (w, h) <- getDynamic (vSize viewer)
    sphere <- (== SphereView) <$> getDynamic (vView viewer)
    smooth <- maybe (pure defaultSmooth) (readIORef . vsSmooth) (lookup k (map fst loops))
    colorizer <- colorizerInfo ensemble title
    tools <- getDynamic (vTools viewer) >>= mapM (\t -> do
      let ToolInfo{..} = toolInfo t
      name <- getDynamic (source tiName)
      shortcut <- getDynamic tiShortcut
      help <- getDynamic tiShortHelp
      pure (Aeson.object [ "name" .= name, "shortcut" .= shortcut, "help" .= help ]))
    let complex = case vCoordinate viewer of ComplexCoordinate _ -> True; _ -> False
        -- (what "Animate along path" moves, if it can: the viewer's
        -- coordinate, if it's a configuration variable)
        animates = if canAnimate context (vCoordinate viewer) then Just (showCoordinate (vCoordinate viewer)) else Nothing
    kind <- getDynamic (vView viewer)
    let disk = isJust (hyperbolicDisplay kind)
        halfPlane = kind == HalfPlaneView
    pure (Aeson.object [ "id" .= k, "title" .= title, "width" .= w, "height" .= h, "sphere" .= sphere, "disk" .= disk
                       , "halfPlane" .= halfPlane
                       , "tools" .= tools, "complex" .= complex, "smooth" .= smooth, "colorizer" .= colorizer
                       , "animates" .= animates ])
  let translated = case how of Translated _ -> True; _ -> False
  props0 <- getDynamic (ensembleFrontendProperties ensemble)
  let views = KM.lookup "web-properties" props0
  script <- getDynamic (ensembleSourceScript ensemble)
  post $ Aeson.object
    [ "type" .= ("project" :: Text.Text)
    , "title" .= projectTitle
    , "fileName" .= fileName
    , "library" .= (how == FromLibrary)
    , "new" .= (how == FromStartingPoint || translated)
      -- (a rebuild that works closes the editor, so it's only on opening)
    , "edit" .= (not rebuilt && (how == FromStartingPoint || translated || not (ensembleBuilt ensemble)))
    , "notes" .= (case how of Translated notes | not rebuilt -> notes; _ -> [])
    , "panel" .= panel
    , "viewers" .= viewerInfo
    , "views" .= views
    , "savedTables" .= KM.member "tables" props0
    , "script" .= script
    , "rebuilt" .= rebuilt
    ]
  forM_ (map (snd . fst) loops) $ \vs -> do
    title <- getDynamic (vTitle (vsViewer vs))
    names <- getDynamic (vTools (vsViewer vs)) >>= mapM (getDynamic . source . tiName . toolInfo)
    case cTool <$> Map.lookup title carried of
      -- (its tool before the rebuild, if it still has it)
      Just (Just name) | name `elem` names -> activateTool vs name
      Just Nothing -> pure ()
      -- (as the desktop app does: a viewer with a "Select" tool starts with it)
      _ -> forM_ (vSelectTool (vsViewer vs)) $ \t ->
             getDynamic (source (tiName (toolInfo t))) >>= activateTool vs
  -- Changes to save from here on (not the project's own, as it starts).
  -- A rebuild is one; a newly opened project has none, unless it's a
  -- translation (which isn't anywhere yet but here).
  if rebuilt then markUnsaved else setUnsaved (case how of Translated _ -> True; _ -> False)
  watchValues ensemble markUnsaved >>= writeIORef unwatchValues

-- | Stop the open project's render loops (if any).
close :: IO ()
close = readIORef sessionRef >>= mapM_ (\s -> sStop s >> writeIORef sessionRef Nothing)
-- (an open autocolor editor goes on watching: the next project's groups)

-- | Close the project, for the page's Close project (it's asked about
-- unsaved changes already), back to the library.
closeProject :: IO ()
closeProject = guarded $ do
  close
  setUnsaved False

requestRender :: MVar () -> IO ()
requestRender signal = void (tryPutMVar signal ())

-- | The size of the pieces a viewer's image is rendered in, in its pixels
-- (the canvas's): larger than the desktop's ('Task.Block.subBlockSize'),
-- since each is a trip to a worker and back (about 75 µs, so 1,500 or so
-- pieces of a high-DPI screen's image cost about 12 ms a pass, across 10
-- workers, where 16-pixel pieces would cost 200).
webPieceSize :: Int
webPieceSize = 64

-- | How often a render posts what it's drawn, at most (in seconds).
patchInterval :: Double
patchInterval = 1 / 30

-- | How long a render holds its first patch back (in seconds): a render
-- quicker than this comes whole, with no coarse passes shown.
renderHold :: Double
renderHold = 0.1

renderLoop :: ViewerState -> IO ()
renderLoop vs@ViewerState{ vsId = k, vsViewer = viewer, vsSignal = signal } = forever $ do
  takeMVar signal
  -- (while a movie's recorded, the viewers wait: its script changes the
  -- values every frame, and the values are put back afterwards)
  let whileRecording = readIORef recordingRef >>= \r -> when r (threadDelay 50000 >> whileRecording)
  whileRecording
  smooth <- readIORef (vsSmooth vs)
  -- (the viewer's code sees a CSS pixel's size, as tools do: see 'densityRef')
  density <- readIORef densityRef
  covered0 <- atomicModifyIORef' (vsCovered vs) (Nothing,)
  -- (in the disk, the page's picture shows this view's points only if it
  -- was drawn with this view of the hyperbolic plane)
  isDisk <- isJust . hyperbolicDisplay <$> getDynamic (vView viewer)
  isoNow <- getDynamic (vIsometry viewer)
  pictureIso <- readIORef (vsPictureIso vs)
  let covered | isDisk && pictureIso /= Just isoNow = Nothing
              | otherwise = covered0
  renderId <- atomicModifyIORef' (vsRenders vs) (\n -> (n + 1, n + 1))
  Dimensions (w, h) <- getDynamic (vSize viewer)
  concurrency <- renderConcurrency
  started <- getMonotonicTime
  drawn <- newIORef Set.empty
  lastPatch <- newIORef Nothing
  imageRef <- newIORef Nothing
  -- (whether every piece shows something yet: its first, coarse pass)
  coveredYet <- newIORef False
  let -- (a piece is shown if the page's picture covers all of it)
      shown c r = case covered of
        Nothing -> False
        Just (cl, ct, cr, cb) ->
          let x0 = webPieceSize * c; y0 = webPieceSize * r
          in x0 >= cl && y0 >= ct && min w (x0 + webPieceSize) <= cr && min h (y0 + webPieceSize) <= cb
      patch RenderedBuffer{..} done = do
        rects <- atomicModifyIORef' drawn (Set.empty,)
        let flat = concat [ [x, y, pw, ph] | ImagePiece x y pw ph <- Set.toList rects ]
        withArrayLen (map fromIntegral flat) $ \n rp -> withForeignPtr rbPixels $ \p ->
          postPatch k renderId rbWidth rbHeight (fst rbCenter) (snd rbCenter)
                    (fst rbPixelSize) (snd rbPixelSize) p rp (n `div` 4) done
      pieceDrawn p = readIORef imageRef >>= mapM_ (\image -> do
        modifyIORef' drawn (Set.insert p)
        now <- getMonotonicTime
        due <- readIORef lastPatch <&> \case
          Nothing -> now - started >= renderHold
          Just t -> now - t >= patchInterval
        when due $ writeIORef lastPatch (Just now) >> patch image False)
      -- Whether the render's no longer wanted: another's been asked for
      -- (new parameters, say, as a tool's dragged), and this one's shown
      -- something. It's never dropped before every piece has its first,
      -- coarse pass; and if it's shown nothing yet (it's still within the
      -- hold), what it has shows as it's dropped. Else, while renders take
      -- longer than a drag's steps, nothing would ever show.
      stale = readIORef coveredYet >>= \case
        False -> pure False
        True -> isEmptyMVar signal >>= \case
          True -> pure False
          False -> do
            posted <- isJust <$> readIORef lastPatch
            unless posted $ readIORef imageRef >>= mapM_ (\image -> do
              now <- getMonotonicTime
              writeIORef lastPatch (Just now)
              patch image False)
            pure True
  try (renderProgressively Progressive
        { prPieceSize = webPieceSize, prConcurrency = concurrency, prScale = density
        , prSmooth = smooth, prShown = shown
        , prStale = stale
        , prStarted = writeIORef imageRef . Just, prCovered = writeIORef coveredYet True
        , prDrawn = pieceDrawn } viewer) >>= \case
    -- (being stopped, when another project is opened, isn't an error: the
    -- loop just ends)
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> postError (displayException e)
    Right (Left err) -> post $ Aeson.object
      [ "type" .= ("viewerError" :: Text.Text), "viewer" .= k, "message" .= err ]
    -- (dropped: the next render's on its way)
    Right (Right Nothing) -> pure ()
    Right (Right (Just image)) -> do
      patch image True
      writeIORef (vsPictureIso vs) (if isDisk then Just isoNow else Nothing)
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
    withProgress vs Nothing (\flag -> cssPixelDims (vsViewer vs) >>= vRefreshOverlay (vsViewer vs) flag)
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

setView :: Int -> Double -> Double -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
setView k cx cy pixelSize w h cl ct cr cb = withViewerState k $ \vs -> do
  let viewer = vsViewer vs
  setValue (vSize viewer) (Dimensions (max 1 w, max 1 h))
  setValue (vCenter viewer) (cx, cy)
  setValue (vPixelSize viewer) pixelSize
  -- (where the page still shows the last picture, after anything the
  -- changes above set off)
  writeIORef (vsCovered vs) (if cr > cl && cb > ct then Just (cl, ct, cr, cb) else Nothing)
  requestRender (vsSignal vs)

setDensity :: Double -> IO ()
setDensity d = guarded (writeIORef densityRef (max 1 d))

setSmoothing :: Bool -> IO ()
setSmoothing on = guarded (writeIORef smoothingRef on)

setSphereShading :: Double -> IO ()
setSphereShading s = guarded $ do
  before <- getDynamic sphereShading
  let s' = max 0 (min 1 s)
  when (s' /= before) $ do
    setValue sphereShading s'
    -- (the viewers on the sphere, drawn again: the page doesn't keep
    -- their pictures, which are shaded differently now)
    readIORef sessionRef >>= mapM_ (\Session{..} -> forM_ sViewers $ \vs -> do
      sphere <- (== SphereView) <$> getDynamic (vView (vsViewer vs))
      when sphere $ writeIORef (vsCovered vs) Nothing >> requestRender (vsSignal vs))

setViewerSmoothing :: Int -> Bool -> IO ()
setViewerSmoothing k on = withViewerState k $ \vs -> do
  before <- atomicModifyIORef' (vsSmooth vs) (on,)
  when (before /= on) $ writeIORef (vsCovered vs) Nothing >> requestRender (vsSignal vs)

------------------------------------------------------------------------
-- Views: history, navigating, the sphere or the plane

-- | A viewer's view, as the desktop's history keeps them (a pixel's size
-- in the canvas's pixels).
viewerModel :: Viewer -> IO Model
viewerModel v = do
  c <- getDynamic (vCenter v)
  px <- getDynamic (vPixelSize v)
  aspect <- getDynamic (vAspect v)
  kind <- getDynamic (vView v)
  shape <- case kind of
    PlaneView -> pure FlatShape
    SphereView -> GlobeShape <$> getDynamic (vRotation v)
    DiskView -> HyperbolicShape (hyperbolicModel (vOriginalView v)) DiskDisplay <$> getDynamic (vIsometry v)
    HalfPlaneView -> HyperbolicShape (hyperbolicModel (vOriginalView v)) HalfPlaneDisplay <$> getDynamic (vIsometry v)
  pure (Model c (px, px * aspect) shape)

setViewerModel :: Viewer -> Model -> IO ()
setViewerModel v (Model c (px, py) shape) = do
  setValue (vCenter v) c
  setValue (vPixelSize v) px
  setValue (vAspect v) (py / px)
  case shape of
    FlatShape -> setValue (vView v) PlaneView
    GlobeShape r -> setValue (vRotation v) r >> setValue (vView v) SphereView
    HyperbolicShape _ display iso -> do
      setValue (vIsometry v) iso
      setValue (vView v) (case display of { DiskDisplay -> DiskView; HalfPlaneDisplay -> HalfPlaneView })

-- | Go to a view (rendering it), and tell the page.
goTo :: ViewerState -> Model -> IO ()
goTo vs m = do
  setViewerModel (vsViewer vs) m
  writeIORef (vsCovered vs) Nothing
  requestRender (vsSignal vs)
  let Model (cx, cy) (px, py) _ = m
  post $ Aeson.object [ "type" .= ("view" :: Text.Text), "viewer" .= vsId vs, "cx" .= cx, "cy" .= cy
                      , "px" .= px, "py" .= py, "sphere" .= isJust (modelRotation m)
                      , "disk" .= isJust (modelIsometry m)
                      , "halfPlane" .= case modelShape m of { HyperbolicShape _ HalfPlaneDisplay _ -> True; _ -> False } ]

-- | Go to a view, as a new step in the history (after the view it's in).
changeViewTo :: ViewerState -> Model -> IO ()
changeViewTo vs m = do
  now <- viewerModel (vsViewer vs)
  modifyIORef' (vsHistory vs) (Just . historyAppend m . fromMaybe (History [] now []))
  goTo vs m

record :: Int -> IO ()
record k = withViewerState k $ \vs -> do
  now <- viewerModel (vsViewer vs)
  modifyIORef' (vsHistory vs) (Just . maybe (History [] now []) (historyAppend now))

navigateTo :: Int -> Int -> IO ()
navigateTo k what = withViewerState k $ \vs -> do
  let v = vsViewer vs
  d <- readIORef densityRef
  m <- viewerModel v
  Dimensions (_, hWin) <- getDynamic (vSize v)
  let step f = readIORef (vsHistory vs) >>= \h -> forM_ (h >>= f) $ \(h', m') ->
        writeIORef (vsHistory vs) (Just h') >> goTo vs m'
      -- (the script's views are in CSS pixels; the engine's in the canvas's)
      originalPlane = let (c, px) = vOriginalPlaneView v in Model c (px / d, px / d * vOriginalAspect v) FlatShape
      sphereAt r = Model (0, 0) (vOriginalSpherePixelSize v / d, vOriginalSpherePixelSize v / d) (GlobeShape r)
      -- (a hyperbolic viewer's original view, on a display: the
      -- half-plane with its real axis along the window's bottom edge)
      hyperbolicAt display iso =
        let (c, px) = displayScreen display hWin
                        ((case display of { DiskDisplay -> fst; HalfPlaneDisplay -> snd }) (vOriginalHyperbolicPixelSizes v) / d)
        in Model c (px, px) (HyperbolicShape (hyperbolicModel (vOriginalView v)) display iso)
  case what of
    0 -> step historyBack
    1 -> step historyForward
    -- (the original view of the kind it shows now)
    2 -> changeViewTo vs (case modelShape m of
                            GlobeShape _ -> sphereAt (vOriginalRotation v)
                            HyperbolicShape _ display _ -> hyperbolicAt display (vOriginalIsometry v)
                            FlatShape -> originalPlane)
    -- (the point in the middle of the view stays there, at the script's
    -- zoom for the other kind; see 'Actor.Viewer.switchView')
    -- (a disk viewer shows the hyperbolic plane, and only that)
    -- (a viewer of the hyperbolic plane: the disk or the half-plane, keeping
    -- its view)
    3 | HyperbolicShape _ display iso <- modelShape m ->
          changeViewTo vs (hyperbolicAt (case display of { DiskDisplay -> HalfPlaneDisplay; HalfPlaneDisplay -> DiskDisplay }) iso)
    3 -> case vCoordinate v of
      ComplexCoordinate _ -> changeViewTo vs $ case modelRotation m of
        Nothing -> sphereAt (rotationFacing (modelCenter m))
        Just r -> let c@(cx, cy) = sphereToPlane (rotateBy r (0, 0, 1))
                      finite a = not (isNaN a || isInfinite a) && abs a < 1e300
                      px = snd (vOriginalPlaneView v) / d
                  in Model (if finite cx && finite cy then c else (0, 0)) (px, px) FlatShape
      _ -> pure ()
    _ -> pure ()

-- | Jump to a view: centered on a point (on the sphere, the globe turned
-- to face it), with a pixel of a size (a CSS pixel's).
jump :: Int -> Double -> Double -> Double -> IO ()
jump k x y pixelSize = withViewerState k $ \vs -> do
  d <- readIORef densityRef
  Model _ (px, py) shape <- viewerModel (vsViewer vs)
  Dimensions (_, hWin) <- getDynamic (vSize (vsViewer vs))
  let dz = pixelSize / d
  case shape of
    FlatShape -> changeViewTo vs (Model (x, y) (dz, dz * py / px) FlatShape)
    GlobeShape _ -> changeViewTo vs (Model (0, 0) (dz, dz) (GlobeShape (rotationFacing (x, y))))
    -- (in the hyperbolic plane, the point in the middle of the window; the
    -- half-plane's real axis along its bottom edge)
    HyperbolicShape model display _
      | inModel model (x, y) ->
          let middle = case display of
                DiskDisplay -> (0, 0)
                HalfPlaneDisplay -> axisAtBottom hWin dz (0, 0)
          in changeViewTo vs (Model middle (dz, dz) (HyperbolicShape model display (viewShowing model display (x, y) middle)))
                | otherwise -> postError "That point isn't in the disk."

-- | A real viewer's aspect ratio, from its text, as the desktop's Aspect
-- ratio... sets it: keeping the center and a pixel's width, as a new
-- step in the history.
setAspect :: Int -> JSString -> IO ()
setAspect k txt = withViewerState k $ \vs -> do
  let reply (e :: Maybe String) = post $ Aeson.object
        [ "type" .= ("aspect" :: Text.Text), "viewer" .= k, "error" .= e ]
  case parseConstant RealType (fromJSString txt) of
    Right a | a > 0, not (isInfinite a) -> do
      Model c (px, _) _ <- viewerModel (vsViewer vs)
      changeViewTo vs (Model c (px, px * a) FlatShape)
      reply Nothing
    _ -> reply (Just "The aspect ratio should be a positive number, like 2 or 1/2.")

------------------------------------------------------------------------
-- Autocolor

-- | While the page's autocolor editor is open, the thread posting the
-- groups when they change.
autocolorWatchRef :: IORef (Maybe ThreadId)
autocolorWatchRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE autocolorWatchRef #-}

-- | The open project's autocolor groups, as the editor shows them.
autocolorJSON :: IO (Maybe Aeson.Value)
autocolorJSON = readIORef sessionRef >>= traverse (\Session{..} -> do
  st <- getDynamic (ensembleAutocolor sEnsemble)
  groups <- autocolorGroups st
  gs <- forM (Map.toList groups) $ \(AutocolorGroupId n, g) -> do
    locked <- getDynamic (agLocked g)
    blend <- getDynamic (agBlendMisses g)
    tolerance <- getDynamic (agTolerance g)
    miss <- getDynamic (agMissColor g)
    table <- getDynamic (agTable g)
    entries <- forM (Map.toList table) $ \(pt@(x, y), cv) -> do
      c <- getDynamic cv
      pure (Aeson.object [ "x" .= x, "y" .= y, "point" .= showAutocolorPoint pt, "color" .= hexColor c ])
    pure (Aeson.object [ "id" .= n, "locked" .= locked, "blend" .= blend
                       , "tolerance" .= showAutocolorPoint (tolerance, 0), "miss" .= hexColor miss
                       , "entries" .= entries ])
  pure (Aeson.object [ "type" .= ("autocolor" :: Text.Text), "groups" .= gs ]))
  where
    hexColor c = let (r, g, b) = colorToRGB c in (printf "#%02x%02x%02x" r g b :: String)

-- | Post the groups now, and whenever they've changed, until the editor's
-- closed (checking a few times a second).
autocolorWatch :: Bool -> IO ()
autocolorWatch on = guarded $ do
  readIORef autocolorWatchRef >>= mapM_ killThread
  writeIORef autocolorWatchRef Nothing
  when on $ do
    tid <- forkIO $ do
      lastPosted <- newIORef Nothing
      forever $ do
        current <- autocolorJSON
        before <- readIORef lastPosted
        when (current /= before) $ do
          writeIORef lastPosted current
          mapM_ post current
        threadDelay 300000
    writeIORef autocolorWatchRef (Just tid)

-- | An edit from the editor (see @fs_autocolor_edit@).
autocolorEdit :: JSString -> IO ()
autocolorEdit edit = guarded $ readIORef sessionRef >>= mapM_ (\Session{..} -> do
  st <- getDynamic (ensembleAutocolor sEnsemble)
  groups <- autocolorGroups st
  let say msg = post (Aeson.object [ "type" .= ("autocolorError" :: Text.Text), "message" .= msg ])
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack (fromJSString edit))) of
    Left err -> say ("couldn't read the edit: " ++ err)
    Right (Aeson.Object o) -> do
      let str k = case KM.lookup k o of Just (Aeson.String t) -> Just (Text.unpack t); _ -> Nothing
          num k = case KM.lookup k o of Just (Aeson.Number n) -> Just (realToFrac n :: Double); _ -> Nothing
          bool k = case KM.lookup k o of Just (Aeson.Bool b) -> Just b; _ -> Nothing
          readPoint txt = case parseConstant ComplexType txt of
            Right (x :+ y) -> Right (x, y)
            Left _ -> Left ("Couldn't read " ++ show txt ++ " as a complex number.")
          color = str "color" >>= readHex
          point = (,) <$> num "x" <*> num "y"
      case num "group" >>= \n -> Map.lookup (AutocolorGroupId (round n)) groups of
        Nothing -> say "There's no such autocolor group."
        -- (an edit to save, as the desktop counts one: if it's refused,
        -- the next save writes what was there anyway)
        Just g -> markUnsaved >> case str "op" of
          Just "locked" -> mapM_ (setAutocolorLocked st g) (bool "on")
          Just "blend" -> mapM_ (setAutocolorBlendMisses st g) (bool "on")
          Just "tolerance" -> case parseConstant RealType (fromMaybe "" (str "text")) of
            Right r | r > 0 -> setAutocolorTolerance st g r
            _ -> say ("The tolerance has to be a positive number, not " ++ show (fromMaybe "" (str "text")) ++ ".")
          Just "miss" -> mapM_ (setAutocolorMissColor st g) color
          Just "color" -> sequence_ (setAutocolorEntryColor st g <$> point <*> color)
          Just "move" -> forM_ point $ \from -> case readPoint (fromMaybe "" (str "text")) of
            Left err -> say err
            Right to | to /= from -> moveAutocolorEntry st g from to
                     | otherwise -> pure ()
          Just "delete" -> forM_ point $ \(x, y) -> deleteAutocolor st g x y
          Just "add" -> case readPoint (fromMaybe "" (str "text")) of
            Left err -> say err
            Right pt -> void (addAutocolorEntry st g pt Nothing)
          Just "clear" -> clearAutocolorGroup st g
          _ -> say "That isn't an autocolor edit."
    Right _ -> say "That isn't an autocolor edit.")
  where
    readHex :: String -> Maybe Color
    readHex ('#' : h) | length h == 6, all isHexDigit h =
      let byte k = fromIntegral (digitToInt (h !! (2 * k)) * 16 + digitToInt (h !! (2 * k + 1)))
      in Just (rgbToColor (byte 0, byte 1, byte 2))
    readHex _ = Nothing

------------------------------------------------------------------------
-- Capturing pictures and movies

-- | A copy of a viewer at its size in CSS pixels, times @scale@ (made even
-- each way, if @makeEven@), showing the same view.
scaledCopy :: Viewer -> Double -> Bool -> IO Viewer
scaledCopy viewer scale makeEven = do
  d <- readIORef densityRef
  vi <- cloneViewer viewer
  Dimensions (w, h) <- getDynamic (vSize vi)
  px <- getDynamic (vPixelSize vi)
  let sized n = let x = max 1 (round (fromIntegral n / d * scale)) :: Int
                in if makeEven then max 2 (2 * (x `div` 2)) else x
      (nw, nh) = (sized w, sized h)
  setValue (vSize vi) (Dimensions (nw, nh))
  setValue (vPixelSize vi) (px * fromIntegral w / fromIntegral nw)
  pure vi

-- | A viewer's title, made fit to name a file with.
fileTitle :: Viewer -> IO String
fileTitle v = map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '-' else c) <$> getDynamic (vTitle v)

captureImage :: Int -> Double -> Bool -> IO ()
captureImage k multiplier smooth = withViewerState k $ \vs -> do
  vi <- scaledCopy (vsViewer vs) (max 0.01 multiplier) False
  name <- fileTitle (vsViewer vs)
  withRenderedViewerBuffer vi smooth (\_ (w, h) buf -> bufferToPng (w, h) buf) >>= \case
    Left err -> postError ("Couldn't draw the picture: " ++ err)
    Right (_, png) -> BS.useAsCStringLen (LBS.toStrict png) $ \(p, n) ->
      postDownload (toJSString (name ++ ".png")) (castPtr p) n

------------------------------------------------------------------------
-- The project's details

-- | Post the project's details (see @fs_details@).
details :: IO ()
details = guarded $ readIORef sessionRef >>= mapM_ (postDetails False)

postDetails :: Bool -> Session -> IO ()
postDetails fromViewer Session{..} = do
  md <- getDynamic (ensembleMetadata sEnsemble)
  post $ Aeson.object
    [ "type" .= ("details" :: Text.Text)
    , "title" .= fromMaybe "" (mdTitle md)
    , "description" .= fromMaybe "" (mdDescription md)
    , "preview" .= fmap (Text.decodeUtf8 . B64.encode) (mdPreview md)
    , "fileName" .= sFileName
    , "fromViewer" .= fromViewer
    ]

-- | Set the project's title and description, as the desktop's Project
-- info does. Its title (for a rebuild, too) is its new one, or else its
-- file's name.
setDetails :: JSString -> JSString -> IO ()
setDetails title description = guarded $ readIORef sessionRef >>= mapM_ (\s@Session{..} -> do
  let nonEmpty t = if all isSpace t then Nothing else Just t
      newTitle = nonEmpty (fromJSString title)
  md <- getDynamic (ensembleMetadata sEnsemble)
  let md' = md { mdTitle = newTitle, mdDescription = nonEmpty (fromJSString description) }
  when (md' /= md) $ do
    setValue' (ensembleMetadata sEnsemble) md'
    writeIORef sessionRef (Just s { sTitle = fromMaybe (takeBaseName sFileName) newTitle })
    markUnsaved)

-- | "Use this as preview": what the viewer shows (and its tools have
-- drawn), small, as the project's preview.
useAsPreview :: Int -> IO ()
useAsPreview k = withViewerState k $ \vs -> renderPreview False (vsViewer vs) >>= \case
  Left err -> postError ("Couldn't make a preview: " ++ err)
  Right png -> readIORef sessionRef >>= mapM_ (\s -> do
    modifyValue (ensembleMetadata (sEnsemble s)) (\md -> md { mdPreview = Just png })
    markUnsaved
    postDetails True s)

-- | A movie's script to start with, as the desktop's: moving the first
-- complex variable around a small circle, from where it is now.
movieTemplate :: Int -> IO ()
movieTemplate k = guarded $ readIORef sessionRef >>= mapM_ (\Session{..} -> do
  fields <- ensembleFields sEnsemble
  script <- case complexVariables sContext of
    [] -> pure (movieScriptTemplate ["pass"])
    var : _ -> do
      current <- case [ f | (ProjectConfig, f) <- fields, fieldName f == var ] of
        f : _ -> fieldText f
        [] -> pure var
      pure (movieScriptTemplate [var ++ " <- (" ++ current ++ ") + 0.05 e^(2 pi i t)"])
  post $ Aeson.object [ "type" .= ("movieTemplate" :: Text.Text), "viewer" .= k, "script" .= script ])

movieCheck :: Int -> JSString -> IO ()
movieCheck reqId txt = guarded $ readIORef sessionRef >>= mapM_ (\Session{..} -> do
  let src = fromJSString txt
  defs <- either (const noCommonDefs) id <$> ensembleCommonDefs sEnsemble
  post $ Aeson.object
    [ "type" .= ("movieCheck" :: Text.Text), "id" .= reqId
    , "error" .= either (Just . errorJSON src) (const Nothing) (checkMovieScript defs sContext src) ])

-- | Whether a movie's being recorded (the viewers don't redraw meanwhile),
-- whether it's to stop, and the last frame the page has encoded.
recordingRef :: IORef Bool
recordingRef = unsafePerformIO (newIORef False)
{-# NOINLINE recordingRef #-}

movieStopRef :: IORef Bool
movieStopRef = unsafePerformIO (newIORef False)
{-# NOINLINE movieStopRef #-}

movieAckRef :: MVar Int
movieAckRef = unsafePerformIO (newMVar 0)
{-# NOINLINE movieAckRef #-}

movieStart :: Int -> JSString -> Int -> Int -> Bool -> Bool -> Bool -> IO ()
movieStart k txt frames fps loop smooth drawings = withViewerState k $ \vs -> readIORef sessionRef >>= mapM_ (\session@Session{..} -> do
  defs <- ensembleCommonDefs sEnsemble
  animation <- either (pure . Left) (\ds -> scriptAnimation ds sContext (fromJSString txt)) defs
  case animation of
    Left err -> movieDone (Just err) False Nothing
    Right animate -> recordFrames session vs (movieSettings frames fps loop smooth drawings) animate Nothing)

-- | A movie along a path drawn in a viewer (see "Animate along path").
moviePathStart :: Int -> Int -> JSString -> Int -> Int -> Bool -> Bool -> Bool -> Int -> IO ()
moviePathStart k drawnIn pathJson frames fps loop smooth drawings corner =
  withViewerState k $ \vs -> withViewerState drawnIn $ \from -> readIORef sessionRef >>= mapM_ (\session@Session{..} -> do
    let source = vsViewer from
    -- (its points in the plane: on the sphere, those on the globe)
    points <- case Aeson.decodeStrict (Text.encodeUtf8 (Text.pack (fromJSString pathJson))) of
      Just (ps :: [(Double, Double)]) -> catMaybes <$> mapM (toModel source) ps
      Nothing -> pure []
    case pathAnimation sContext (vCoordinate source) loop points of
      Left err -> movieDone (Just err) False Nothing
      Right animate -> do
        let inset = [ MovieInset source (pathPoint loop points) (toEnum corner) | corner >= 0 && corner <= 3 ]
        recordFrames session vs (movieSettings frames fps loop smooth drawings) animate (listToMaybe inset))

movieSettings :: Int -> Int -> Bool -> Bool -> Bool -> MovieSettings
movieSettings frames fps loop smooth drawings =
  defaultMovieSettings { msFrames = max 1 frames, msFps = max 1 fps, msLoop = loop
                       , msSmooth = smooth, msWithDrawings = drawings }

movieDone :: Maybe String -> Bool -> Maybe String -> IO ()
movieDone e stopped issues = post $ Aeson.object
  [ "type" .= ("movieDone" :: Text.Text), "error" .= e, "stopped" .= stopped, "issues" .= issues ]

-- | Record a movie of a viewer: for each frame, the animation's step,
-- and a copy of the viewer (at its size, made even, as H.264 needs)
-- rendered (with the inset, if there is one) and posted, no more than
-- two ahead of the page's encoder; the project's values put back after.
recordFrames :: Session -> ViewerState -> MovieSettings -> Animation -> Maybe MovieInset -> IO ()
recordFrames Session{..} vs settings@MovieSettings{..} animate minset = void $ forkIO $ do
  let times = movieFrameTimes settings
  fields <- map snd . filter ((== ProjectConfig) . fst) <$> ensembleFields sEnsemble
  vi <- scaledCopy (vsViewer vs) 1 True
  writeIORef movieStopRef False
  void (swapMVar movieAckRef 0)
  writeIORef recordingRef True
  result <- try $ withFieldsRestored fields $ do
    Dimensions size <- getDynamic (vSize vi)
    inset <- forM minset $ \mi -> prepareInset msSmooth size mi >>= \case
      Left err -> throwIO (userError ("Couldn't draw the inset: " ++ err))
      Right ready -> pure ready
    foldM (\acc (i, t) -> case acc of
      Left stopped -> pure (Left stopped)
      Right issues -> readIORef movieStopRef >>= \case
        True -> pure (Left True)
        False -> do
          -- (no more than two frames ahead of the page's encoder)
          let waitForPage = do
                acked <- readMVar movieAckRef
                stop <- readIORef movieStopRef
                when (acked < i - 2 && not stop) (threadDelay 5000 >> waitForPage)
          waitForPage
          animate t
          renderViewerImage msSmooth msWithDrawings vi >>= \case
            Left err -> throwIO (userError err)
            Right (frameIssues, img0) -> do
              let img = maybe img0 (\ins -> withInset ins t img0) inset
              VS.unsafeWith (imageData img) $ \p ->
                postMovieFrame i (length times) (imageWidth img) (imageHeight img) (castPtr p)
              pure (Right (issues <> frameIssues)))
      (Right mempty) (zip [1 ..] times)
  writeIORef recordingRef False
  case result of
    Left (e :: SomeException) -> movieDone (Just (displayException e)) False Nothing
    Right (Left (_ :: Bool)) -> movieDone Nothing True Nothing
    Right (Right issues) -> movieDone Nothing False (describeViewerIssues issues)

movieAck :: Int -> IO ()
movieAck i = guarded (void (swapMVar movieAckRef i))

movieStop :: IO ()
movieStop = guarded (writeIORef movieStopRef True)

-- | Where a viewer is: the point in the middle of its view (on the
-- sphere, the one the globe faces), and a CSS pixel's size.
whereIs :: Int -> IO ()
whereIs k = withViewerState k $ \vs -> do
  d <- readIORef densityRef
  Model c (px, _) shape <- viewerModel (vsViewer vs)
  let (x, y) = case shape of
        FlatShape -> c
        GlobeShape r -> sphereToPlane (rotateBy r (0, 0, 1))
        -- (in the hyperbolic plane, the point in the middle of the window)
        HyperbolicShape model display iso -> fromMaybe (0, 0) (screenToModelH model display iso c)
  post $ Aeson.object [ "type" .= ("where" :: Text.Text), "viewer" .= k, "x" .= x, "y" .= y
                      , "pixelSize" .= (px * d) ]

-- | A viewer's view as its file saved it (in @web-properties:@, with the
-- desktop's keys): on the sphere (@view: sphere@), the globe's rotation
-- (@rotation@: a unit quaternion, w x y z). Its center and zoom the page
-- sets.
applySavedView :: Viewer -> Maybe Aeson.Value -> String -> IO ()
applySavedView v saved title = case saved of
  Just (Aeson.Object o)
    | Just (Aeson.Object vs) <- KM.lookup "viewers" o
    , Just (Aeson.Object p) <- KM.lookup (fromString title) vs -> do
        forM_ (KM.lookup "rotation" p >>= decodeValue) $ \case
          [w, x, y, z] -> setValue (vRotation v) (Rotation w x y z)
          _ -> pure ()
        forM_ (KM.lookup "isometry" p >>= decodeValue) $ \case
          [ar, ai, br, bi] -> setValue (vIsometry v) (normalizeIsometry (Isometry (ar :+ ai) (br :+ bi)))
          _ -> pure ()
        forM_ (KM.lookup "aspect" p) $ \case
          Aeson.Number a | a > 0 -> setValue (vAspect v) (realToFrac a)
          _ -> pure ()
        case KM.lookup "view" p of
          Just (Aeson.String "sphere") | isComplex, not hyperbolic -> setValue (vView v) SphereView
          Just (Aeson.String "plane") | not hyperbolic -> setValue (vView v) PlaneView
          Just (Aeson.String "disk") | hyperbolic -> setValue (vView v) DiskView
          Just (Aeson.String "halfplane") | hyperbolic -> setValue (vView v) HalfPlaneView
          _ -> pure ()
  _ -> pure ()
  where
    decodeValue :: Aeson.Value -> Maybe [Double]
    decodeValue x = case Aeson.fromJSON x of Aeson.Success r -> Just r; _ -> Nothing
    isComplex = case vCoordinate v of ComplexCoordinate _ -> True; _ -> False
    hyperbolic = isJust (hyperbolicDisplay (vOriginalView v))

-- | The page's view state, with what the engine knows of each viewer's
-- view: on the sphere or the plane (when it isn't the script's), and the
-- globe's rotation.
withViews :: Map.Map Int ViewerState -> Aeson.Value -> IO Aeson.Value
withViews viewers = \case
  Aeson.Object o | Just (Aeson.Object vs) <- KM.lookup "viewers" o -> do
    vs' <- foldM (\acc st -> do
      let v = vsViewer st
      title <- getDynamic (vTitle v)
      kind <- getDynamic (vView v)
      Rotation w x y z <- getDynamic (vRotation v)
      Isometry (ar :+ ai) (br :+ bi) <- getDynamic (vIsometry v)
      aspect <- getDynamic (vAspect v)
      let viewName = case kind of { SphereView -> "sphere"; PlaneView -> "plane"; DiskView -> "disk"; HalfPlaneView -> "halfplane" }
          extra = [ ("view", Aeson.String viewName) | kind /= vOriginalView v ]
               ++ [ ("aspect", Aeson.toJSON aspect) | aspect /= vOriginalAspect v ]
               ++ [ ("rotation", Aeson.toJSON [w, x, y, z]) | kind == SphereView ]
               ++ [ ("isometry", Aeson.toJSON [ar, ai, br, bi]) | isJust (hyperbolicDisplay kind) ]
      pure $ case KM.lookup (fromString title) acc of
        Just (Aeson.Object p) -> KM.insert (fromString title) (Aeson.Object (foldr (uncurry KM.insert) p extra)) acc
        _ -> acc) vs (Map.elems viewers)
    pure (Aeson.Object (KM.insert "viewers" (Aeson.Object vs') o))
  other -> pure other

------------------------------------------------------------------------
-- Colorizers

-- | A viewer's colorizer menu: the one in use, and those it could use
-- (as the desktop's menu lists them).
colorizerInfo :: Ensemble -> String -> IO Aeson.Value
colorizerInfo ens title = pure $ case Map.lookup title (ensembleViewerColorizers ens) of
  Nothing -> Aeson.Null
  Just ViewerColorizer{..} ->
    let available = ensembleAvailableColorizers ens
        inUse c = choiceName colorizerInUse == colorizerName c
        needsArgs c = not (null (missingArguments colorizerScope c))
        label c | inUse c = colorizerInUse
                | needsArgs c = colorizerName c ++ "..."
                | otherwise = colorizerName c
    in Aeson.object
      [ "inUse" .= colorizerInUse
      , "arguments" .= maybe False (not . null . colorizerParams) (find inUse available)
      , "items" .= [ Aeson.object [ "name" .= colorizerName c, "label" .= label c
                                  , "description" .= colorizerDescription c
                                  , "inUse" .= inUse c, "asks" .= needsArgs c ]
                   | c <- available ] ]

-- | Choose a colorizer for a viewer, as the desktop's menu does: at once,
-- if the viewer can fill in its parameters; else asking for its
-- arguments first (starting from its default's, if it's the default).
chooseColorizer :: Int -> JSString -> IO ()
chooseColorizer k name = withColorizers k $ \vs title _ vc available -> do
  let n = fromJSString name
  forM_ (find ((== n) . colorizerName) available) $ \c ->
    unless (choiceName (colorizerInUse vc) == n) $
      if null (missingArguments (colorizerScope vc) c)
        then useColorizer title n
        else askArguments vs vc c (if choiceName (colorizerDefault vc) == n then choiceArguments (colorizerDefault vc) else "")

-- | Ask for the arguments of the colorizer in use.
colorizerArguments :: Int -> IO ()
colorizerArguments k = withColorizers k $ \vs _ _ vc available ->
  forM_ (find ((== choiceName (colorizerInUse vc)) . colorizerName) available) $ \c ->
    askArguments vs vc c (choiceArguments (colorizerInUse vc))

askArguments :: ViewerState -> ViewerColorizer -> Colorizer -> String -> IO ()
askArguments vs vc c written = post $ Aeson.object
  [ "type" .= ("colorizerArguments" :: Text.Text), "viewer" .= vsId vs, "name" .= colorizerName c
  , "fields" .= [ Aeson.object [ "param" .= p, "note" .= note, "text" .= text ]
                | (p, note, text) <- argumentFields (colorizerScope vc) c written ] ]

-- | The answers to 'askArguments': use the colorizer with them.
colorizerAnswers :: Int -> JSString -> JSString -> IO ()
colorizerAnswers k name answers = withColorizers k $ \_ title _ _ available ->
  case Aeson.eitherDecodeStrict (Text.encodeUtf8 (Text.pack (fromJSString answers))) of
    Left err -> postError ("couldn't read the arguments: " ++ err)
    Right (as :: [String]) -> forM_ (find ((== fromJSString name) . colorizerName) available) $ \c ->
      useColorizer title (choiceWithArguments c as)

withColorizers :: Int -> (ViewerState -> String -> Ensemble -> ViewerColorizer -> [Colorizer] -> IO ()) -> IO ()
withColorizers k act = guarded $ readIORef sessionRef >>= \case
  Nothing -> pure ()
  Just Session{..} -> forM_ (Map.lookup k sViewers) $ \vs -> do
    title <- getDynamic (vTitle (vsViewer vs))
    forM_ (Map.lookup title (ensembleViewerColorizers sEnsemble)) $ \vc ->
      act vs title sEnsemble vc (ensembleAvailableColorizers sEnsemble)

-- | Rebuild the project with a viewer's colorizer chosen (choosing its
-- default is the same as not choosing one, which keeps the project file
-- tidy), as the desktop does. If it doesn't build, the project stays as
-- it was, and says why.
useColorizer :: String -> String -> IO ()
useColorizer title chosen = readIORef sessionRef >>= mapM_ (\Session{..} -> do
  choices <- getDynamic (ensembleColorizerChoices sEnsemble)
  let isDefault = (Just chosen ==) . fmap colorizerDefault $ Map.lookup title (ensembleViewerColorizers sEnsemble)
      choices' | isDefault = Map.delete title choices
               | otherwise = Map.insert title chosen choices
  src <- getDynamic (ensembleSourceScript sEnsemble)
  buildEnsembleWithColorizers choices' src >>= \case
    Left (_, err) -> postError ("Couldn't use that colorizer: " ++ err)
    Right new -> rebuildAs new)

-- | Move a disk viewer's view as the pointer drags from one point of the
-- screen plane to another (the disk's drawing, in its radii, y up): the
-- hyperbolic translation that keeps the point under the pointer.
diskDrag :: Int -> Double -> Double -> Double -> Double -> IO ()
diskDrag k fx fy tx ty = withViewerState k $ \vs -> do
  iso <- getDynamic (vIsometry (vsViewer vs))
  display <- fromMaybe DiskDisplay . hyperbolicDisplay <$> getDynamic (vView (vsViewer vs))
  setValue (vIsometry (vsViewer vs)) (dragOnDisplay display iso (fx, fy) (tx, ty))
  -- (the last picture shows none of it where it is now)
  writeIORef (vsCovered vs) Nothing
  requestRender (vsSignal vs)

rotate :: Int -> Double -> Double -> Double -> Double -> IO ()
rotate k fx fy tx ty = withViewerState k $ \vs -> do
  r <- getDynamic (vRotation (vsViewer vs))
  setValue (vRotation (vsViewer vs)) (trackballRotation r (fx, fy) (tx, ty))
  -- (the globe's turned: the last picture shows none of it where it is now)
  writeIORef (vsCovered vs) Nothing
  requestRender (vsSignal vs)

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
    dims <- cssPixelDims vsViewer
    case atDispatch dims ev of
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
  DiskView -> getDynamic (vIsometry viewer) <&> \iso -> screenToModelH (hyperbolicModel (vOriginalView viewer)) DiskDisplay iso p
  HalfPlaneView -> getDynamic (vIsometry viewer) <&> \iso -> screenToModelH (hyperbolicModel (vOriginalView viewer)) HalfPlaneDisplay iso p

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
  -- (on the sphere or in the disk, drawings move with the view)
  curved <- (/= PlaneView) <$> getDynamic (vView vsViewer)
  when (changed || curved) $ do
    layers <- vDrawCmds vsViewer
    proj <- viewProjection (vOriginalView vsViewer) (vView vsViewer) (vRotation vsViewer) (vIsometry vsViewer)
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
      Right new -> rebuildAs new

-- | Show a project rebuilt from the open one (edited, or with another
-- colorizer), with everything about it that isn't in its script carried
-- over (values, tables, autocolor, ...), and each viewer's tool, view,
-- smoothing and history (by title).
rebuildAs :: Ensemble -> IO ()
rebuildAs new = readIORef sessionRef >>= mapM_ (\Session{..} -> do
  carried <- fmap Map.fromList . forM (Map.elems sViewers) $ \vs -> do
    let v = vsViewer vs
    title <- getDynamic (vTitle v)
    c <- Carried <$> (fmap atName <$> readIORef (vsTool vs)) <*> viewerModel v <*> getDynamic (vSize v)
                 <*> readIORef (vsSmooth vs) <*> readIORef (vsHistory vs)
    pure (title, c)
  carryOverProjectState sEnsemble new
  startProject sOpening sTitle sFileName True carried new)

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
