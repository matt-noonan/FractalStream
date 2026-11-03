{- |
Module      : Actor.Movie
Description : Recording a movie of a viewer as the project's values change

A movie is made by running an /animation/ -- something that puts the
project's configuration values where they should be at a time @t@
between 0 and 1 -- and rendering a viewer after each step, one frame at
a time, into a movie file ("Data.Movie"). Two kinds of animation are
provided, matching the two ways of asking for a movie:

* 'scriptAnimation': a small piece of script, run once a frame with a
  real variable (normally @t@) set to the frame's time, which may assign
  to any of the project's configuration variables, as a tool's script
  does: @C <- 0.3 e^(2 pi i t)@.
* 'pathAnimation': a path drawn as a list of points (in a parameter
  plane, say), which sets one complex variable to the point a fraction
  @t@ of the way along it, by arclength, so the parameter moves along
  it at a steady speed.

Frames are rendered like 'Actor.Viewer.withRenderedViewerBuffer' renders
a snapshot (supersampled at twice the size and averaged down, when
smoothing), but in horizontal strips across every capability, since a
movie renders many frames. Setting the values is what makes the
project's own windows redraw, too; they go back to their old values
afterwards ('withFieldsRestored').
-}
module Actor.Movie
  ( MovieSettings(..)
  , defaultMovieSettings
  , movieFrameTimes
  , Animation
  , scriptAnimation
  , checkMovieScript
  , movieScriptTemplate
  , pathAnimation
  , complexVariables
  , pathPointAt
  , renderViewerImage
  , recordMovie
  , withFieldsRestored
  ) where

import FractalStream.Prelude

import Actor.Event (EventArgument_, EventArgument(..), Coordinate(..), constArg, interpretedToolExec,
                   assertMissingUnitArgs, parseUnitScript)
import Actor.Ensemble (formatScriptError)
import Actor.Layout (Dimensions(..), Field(..))
import Actor.Viewer
import Actor.Viewer.Overlay (compositeDrawLayers)
import Data.DynamicValue (getDynamic)
import Data.Movie
import Language.Code (Code)
import Language.Code.Syntax (Stmt(..), StmtF(..))
import Language.Value.Syntax (Syn(..), ExprF(..))
import Language.Code.Parser (CommonDefs, parseParsedCode, withCommon)
import Language.Parser.SourceRange (SourceRange(..), HasErrorLocation(..))
import Language.Value.Parser (parseParsedValue)
import Language.Value.Typecheck (ParsedValue)
import Language.Draw (DrawSink(..))
import Language.Environment
import Language.Type

import Codec.Picture (Image(..), PixelRGB8(..), PixelRGBA8(..), pixelMap)
import Control.Concurrent (getNumCapabilities)
import Control.Exception (Exception, throwIO, try, finally)
import Control.Concurrent.MVar
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Marshal.Alloc (allocaBytes)
import Task.Concurrent (forPool_)

-- | How a movie is to be made.
data MovieSettings = MovieSettings
  { msFrames       :: Int
    -- ^ how many frames to record
  , msFps          :: Int
    -- ^ how many frames are shown each second
  , msLoop         :: Bool
    -- ^ whether the movie is to be played in a loop: then its last frame
    -- is the one just before @t = 1@, since the movie goes back to its
    -- first frame (@t = 0@) next, and an animation that comes back to
    -- where it started would otherwise show that frame twice
  , msSmooth       :: Bool
    -- ^ render each frame at twice the size and average it down
  , msWithDrawings :: Bool
    -- ^ include what the viewer's tools have drawn (as it was when the
    -- movie started: tools don't run while it's recorded)
  , msQuality      :: Int
    -- ^ JPEG quality, from 1 to 100
  , msFormat       :: MovieFormat
  } deriving Show

defaultMovieSettings :: MovieSettings
defaultMovieSettings = MovieSettings
  { msFrames = 150, msFps = 30, msLoop = False, msSmooth = True
  , msWithDrawings = False, msQuality = 90, msFormat = defaultMovieFormat }

-- | The time, from 0 to 1, of each frame of a movie.
movieFrameTimes :: MovieSettings -> [Double]
movieFrameTimes MovieSettings{..}
  | n <= 1    = [0]
  | msLoop    = [ fromIntegral i / fromIntegral n | i <- [0 .. n - 1] ]
  | otherwise = [ fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
  where n = msFrames

-- | Put the project's values where they should be at the given time,
-- from 0 to 1.
type Animation = Double -> IO ()

---------------------------------------------------------------------------
-- Animations
---------------------------------------------------------------------------

-- | An animation that runs a piece of script each frame. The script
-- defines the animation as a code block taking the time, from 0 to 1:
--
-- > define animate(t):
-- >     C <- 0.3 e^(2 pi i t)
--
-- and may define other things (functions, other code blocks) before it.
-- The parameter can have any name, even one the project already uses:
-- the time itself is bound under a name no script can write
-- ('movieTimeName'), and the block is run with @run animate(...)@ of it,
-- appended to the script, so that 'Language.Code.Procedures' gives the
-- parameter a fresh name of its own, as for any other @run@.
--
-- The script sees the project's configuration variables (the given
-- context) and may assign to them, and it's typechecked here, once, so
-- a mistake is found before the movie starts. It's treated just as a
-- tool's @on refresh@ handler is ('parseUnitScript'): it sees the
-- project's @\@common@ definitions (the given 'CommonDefs', from
-- 'Actor.Ensemble.ensembleCommonDefs') and has the bookkeeping that
-- @iterate@ and @until ... escapes@ need, with the limits from the
-- project's @max_iters@, @escape_radius@, and @vanishing_radius@. It's
-- run by the interpreter, since it runs once a frame.
scriptAnimation :: CommonDefs -> SomeContext EventArgument_ -> String
                -> IO (Either String Animation)
scriptAnimation defs ctx script =
  case withMovieScript defs ctx script prepare of
    Left (NoSourceRange, msg) -> pure (Left msg)
    Left err -> pure (Left (formatScriptError script err))
    Right action -> Right <$> action
  where
    prepare env code withPx = do
      exec <- interpretedToolExec noDrawing env code
      pure (exec . withPx)

-- | Check a movie's script, as 'scriptAnimation' would, without
-- preparing to run it: pure, so it can drive an editor's live error
-- display (see "UI.Widgets.scriptWidget" in the wx app).
checkMovieScript :: CommonDefs -> SomeContext EventArgument_ -> String
                 -> Either (SourceRange, String) ()
checkMovieScript defs ctx script =
  withMovieScript defs ctx script (\_ _ _ -> ())

-- | A script to start a movie's from: the animation block, with this
-- body (one statement per line).
movieScriptTemplate :: [String] -> String
movieScriptTemplate body = unlines ("define animate(t):" : map ("    " ++) body)

-- | The name the time is bound under, while a movie's script runs: one
-- that no script can write, so that it can't clash with anything.
type MovieTimeName = "[movie] time"

movieTimeName :: String
movieTimeName = symbolVal (Proxy @MovieTimeName)

-- | Parse and typecheck a movie's script (see 'scriptAnimation'), and
-- hand its code to the continuation, with its environment and how to
-- make the context it runs in at a given time.
withMovieScript :: CommonDefs -> SomeContext EventArgument_ -> String
                -> (forall env. EnvironmentProxy env -> Code env -> (Double -> Context EventArgument_ env) -> r)
                -> Either (SourceRange, String) r
withMovieScript defs (SomeContext ctx) script k = do
  Stmt sr parsed <- first (errorLocation &&& unlines . pp) (parseParsedCode defs script)
  let stmts = case parsed of
        SBlock ss -> ss
        _ -> [Stmt sr parsed]
  case [ (r, params) | Stmt r (SProc "animate" params _) <- stmts ] of
    [] -> Left (NoSourceRange, "The script should define the animation as a block, \
                               \`define animate(t):`, followed by what to do at time t \
                               \(which goes from 0 to 1).")
    [(_, [_])] -> pure ()
    (r, _) : _ -> Left (r, "`animate` should have one parameter, the time (from 0 to 1): \
                          \`define animate(t):`.")
  let runIt = Stmt NoSourceRange (SRun "animate" (Just (Syn NoSourceRange (EVar movieTimeName))))
      body = withCommon defs (Stmt sr (SBlock (stmts ++ [runIt])))
  case lookupEnv' (Proxy @MovieTimeName) (contextToEnv ctx) of
    Found'{} -> Left (NoSourceRange, "INTERNAL ERROR: the project has a variable named " ++ movieTimeName)
    Absent' proof -> recallIsAbsent proof $ do
      let withTime t = Bind (Proxy @MovieTimeName) RealType (constArg t) ctx
          env = contextToEnv (withTime 0)
      assertMissingUnitArgs env $ do
        code <- parseUnitScript defs movieLimits (ComplexCoordinate "") Nothing body env
        -- (the pixel size a unit handler is given: a movie's script
        -- has no pixel-size variable to read it with)
        let withPx t = constArg 0 # withTime t
        pure (k (contextToEnv (withPx 0)) code withPx)

-- | The iteration limits a movie's script runs with: the project's own
-- settings, from the "Iteration settings" every project has.
movieLimits :: (ParsedValue, ParsedValue, ParsedValue)
movieLimits = (setting "max_iters", setting "escape_radius", setting "vanishing_radius")
  where setting name = fromRight (error ("INTERNAL ERROR: movieLimits: " ++ name))
                                 (parseParsedValue name)

-- | A 'DrawSink' that ignores everything: a movie's script has nowhere
-- to draw.
noDrawing :: DrawSink
noDrawing = DrawSink
  { dsClear = pure (), dsStroke = const (pure ()), dsFill = const (pure ())
  , dsPoint = const (pure ()), dsLine = \_ _ -> pure ()
  , dsCircle = \_ _ _ -> pure (), dsRect = \_ _ _ -> pure ()
  , dsWrite = \_ _ -> pure () }

-- | The complex variables in a context that can be set, which a path
-- can move along.
complexVariables :: SomeContext EventArgument_ -> [String]
complexVariables (SomeContext ctx) = catMaybes $ fromContext
  (\name ty arg -> case ty of
      ComplexType | isJust (argSetValue arg) -> Just (symbolVal name)
      _ -> Nothing) ctx

-- | An animation that moves a complex variable along a path, at a
-- steady speed: at time @t@, it's the fraction @t@ of the way along the
-- path, by length. A path of a single point stays put. If it's to go
-- back to the start (for a movie made to loop, 'msLoop'), the path is
-- closed by a straight line from its end back to its start.
pathAnimation :: SomeContext EventArgument_ -> String -> Bool -> [(Double, Double)] -> Either String Animation
pathAnimation ctx var backToStart path0 = do
  when (null path0) (Left "The path has no points.")
  -- (going back to the start along one more straight line)
  let path = if backToStart then path0 ++ take 1 path0 else path0
  case lookupSomeContextByName var ctx of
    Nothing -> Left ("The project has no variable named `" ++ var ++ "`.")
    Just (FoundBinding ty arg) -> case (ty, argSetValue arg) of
      (ComplexType, Just setter) -> Right $ \t ->
        let (x, y) = pathPointAt path t in setter ComplexType (x :+ y)
      (ComplexType, Nothing) -> Left ("`" ++ var ++ "` can't be changed.")
      _ -> Left ("`" ++ var ++ "` isn't a complex variable, so a path can't set it.")

-- | The point the fraction @t@ (from 0 to 1) of the way along a path
-- (a list of points joined by straight lines), measured by length.
pathPointAt :: [(Double, Double)] -> Double -> (Double, Double)
pathPointAt [] _ = (0, 0)
pathPointAt path@(p0 : _) t0 = go path (t * total)
  where
    t = max 0 (min 1 t0)
    segments = zip path (drop 1 path)
    dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    total = sum (map (uncurry dist) segments)
    go (a : rest@(b : _)) remaining
      | d >= remaining && d > 0 = lerp a b (remaining / d)
      | otherwise = go rest (remaining - d)
      where d = dist a b
    go [p] _ = p
    go [] _ = p0
    lerp (x1, y1) (x2, y2) s = (x1 + s * (x2 - x1), y1 + s * (y2 - y1))

-- | Run an action, and then put each field back to what it was before
-- (as typed, so that @1/3@ stays @1/3@), however the action finishes.
withFieldsRestored :: [Field] -> IO a -> IO a
withFieldsRestored fields action = do
  saved <- mapM (\f -> (f,) <$> fieldText f) fields
  action `finally` forM_ saved (\(f, txt) -> do
    now <- fieldText f
    when (now /= txt) (void (setFieldText f txt)))

---------------------------------------------------------------------------
-- Rendering
---------------------------------------------------------------------------

-- | Render a viewer's current view (with its current values) as an
-- image of the viewer's size, in parallel strips, supersampled if
-- smoothing. Tools' drawings are included if asked for.
renderViewerImage :: Bool -> Bool -> Viewer -> IO (Either String (ViewerIssues, Image PixelRGB8))
renderViewerImage smooth withDrawings Viewer{..} = case vCodeWithArgs of
  CodeWithArgs getArgs _ code -> getArgs >>= \case
    Left err -> pure (Left err)
    Right vaArgs -> do
      Dimensions (w, h) <- getDynamic vSize
      pixelSize <- getDynamic vPixelSize
      center@(cx, cy) <- getDynamic vCenter
      ViewerFunction fn <- getDynamic code
      let s = if smooth then 2 else 1
          bigW = w * s
          px = pixelSize / fromIntegral s
          (x0, y0) = (cx - fromIntegral bigW * px / 2, cy + fromIntegral (h * s) * px / 2)
          -- strips of output rows; enough of them to keep every
          -- capability busy even if some strips are slower than others
          stripRows = max 1 (min 16 (h `div` 64))
          strips = [ (r, min stripRows (h - r)) | r <- [0, stripRows .. h - 1] ]
      caps <- getNumCapabilities
      out <- mallocForeignPtrBytes (3 * w * h)
      issuesVar <- newMVar mempty
      withForeignPtr out $ \outPtr -> do
        forPool_ caps strips $ \(row, rows) ->
          allocaBytes (3 * bigW * rows * s) $ \big -> do
            issues <- fn ViewerArgs
              { vaPoint = (x0, y0 - fromIntegral (row * s) * px)
              , vaStep = (px, px)
              , vaWidth = fromIntegral bigW
              , vaHeight = fromIntegral (rows * s)
              , vaSubsamples = 1
              , vaBuffer = big
              , vaArgs }
            modifyMVar_ issuesVar (pure . (<> issues))
            let dest = outPtr `plusPtr` (3 * w * row)
            if smooth then downsample bigW w rows big dest
                      else copyBytes' (3 * w * rows) big dest
        issues <- readMVar issuesVar
        img <- if withDrawings
          then do
            layers <- vDrawCmds
            pixelMap dropAlpha <$>
              compositeDrawLayers center pixelSize layers (fromIntegral w, fromIntegral h) outPtr
          else pure (Image w h (VS.unsafeFromForeignPtr0 out (3 * w * h)))
        -- (the image's vector shares the buffer, which lives as long as it does)
        pure (Right (issues, img))
  where
    dropAlpha (PixelRGBA8 r g b _) = PixelRGB8 r g b

-- | Average each 2x2 block of a buffer @2w@ pixels wide and @2 rows@
-- high into one pixel of a buffer @w@ wide.
downsample :: Int -> Int -> Int -> Ptr Word8 -> Ptr Word8 -> IO ()
downsample bigW w rows big dest =
  forM_ [0 .. rows - 1] $ \j -> forM_ [0 .. w - 1] $ \i -> forM_ [0, 1, 2] $ \k -> do
    let ix = 3 * (2 * i) + 3 * bigW * (2 * j) + k
        at o = fromIntegral <$> peekElemOff big o :: IO Word16
    c1 <- at ix
    c2 <- at (ix + 3)
    c3 <- at (ix + 3 * bigW)
    c4 <- at (ix + 3 * bigW + 3)
    pokeElemOff dest (3 * i + 3 * w * j + k) (fromIntegral ((c1 + c2 + c3 + c4 + 2) `div` 4))

copyBytes' :: Int -> Ptr Word8 -> Ptr Word8 -> IO ()
copyBytes' n src dst = forM_ [0 .. n - 1] $ \i -> peekElemOff src i >>= pokeElemOff dst i

---------------------------------------------------------------------------
-- Recording
---------------------------------------------------------------------------

newtype MovieFailed = MovieFailed String deriving Show
instance Exception MovieFailed

-- | Record a movie of a viewer into a file: for each frame, step the
-- animation to the frame's time, render the viewer, and add the image.
-- After each frame, the number of frames done so far is reported. Any
-- problem the renders ran into is returned with the result (like a
-- snapshot's), and a failure leaves no file behind.
--
-- The animation's changes aren't undone here; see 'withFieldsRestored'.
recordMovie :: MovieSettings -> Viewer -> Animation -> (Int -> IO ()) -> FilePath
            -> IO (Either String ViewerIssues)
recordMovie settings@MovieSettings{..} viewer0 animate progress path = do
  -- Every frame must be the same size, and show the same view, even if
  -- the viewer's window is resized or moved around while the movie is
  -- recorded: so it's recorded from a copy of the viewer, as it is now.
  viewer <- cloneViewer viewer0
  Dimensions size <- getDynamic (vSize viewer)
  try (withMovieFile msFormat path size msFps $ \mf ->
         foldM (\issues (i, t) -> do
                   animate t
                   renderViewerImage msSmooth msWithDrawings viewer >>= \case
                     Left err -> throwIO (MovieFailed err)
                     Right (frameIssues, img) -> do
                       addMovieFrame mf (encodeJpegFrame msQuality img)
                       progress i
                       pure (issues <> frameIssues))
               mempty (zip [1 ..] (movieFrameTimes settings)))
    <&> first (\(MovieFailed err) -> err)
