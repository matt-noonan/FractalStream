module Actor.Viewer
  ( ViewerContext(..)
  , buildViewerContext
  , parseViewerScript
  , InternalX
  , InternalY
  , InternalDX
  , InternalDY
  , ViewerEnv
  , InternalViewerEnv
  , MissingViewerArgs
  , SomeViewerWithContext(..)
  , hoistViewerCode
  , hoistedViewerFunction
  , ViewerCompiler(..)
  , ViewerArgs(..)
  , ViewerFunction(..)
  , ViewerIssues(..)
  , describeViewerIssues
  , Viewer(..)
  , refreshOverlay
  , refreshOverlayWith
  , CodeWithArgs(..)
  , cloneViewer
  , onParameterChanges
  , runViewerFunction
  , assertMissingViewerArgs
  , snapshotToFile
  , withRenderedViewerBuffer
  , RenderedBuffer(..)
  , renderViewerBuffer
  , renderViewerBufferAt
  , Progressive(..)
  , ImagePiece(..)
  , renderProgressively
  , pointerText
  , viewProjection
  , switchView
  , centerViewOn
  , allGrey
  , defaultIterLimit
  , defaultMaxRadius
  , defaultMinRadius
  -- * Backend
  , ToolRunnerFactory(..)
  , ToolRunner(..)
  , defaultToolRunner
  , Backend(..)
  , defaultToolRunnerFactory
  ) where

import FractalStream.Prelude

import Actor.Viewer.Types
import Actor.Autocolor (AutocolorLookup)
import Data.DynamicValue
import Actor.Layout (Dimensions(..))
import Actor.Tool
import Actor.Event ( ToolRunner(..), defaultToolRunner, EventArgument_, ParsedToolScript
                   , Coordinate(..), ViewKind(..), bindOrDeclare, RealIn(..), bindFreshReals
                   , pixelWidthVar, pixelHeightVar, PixelDims, ToolDispatcher
                   , ToolExec, EventArgument(..), HandlerRun(..), newCancelFlag, CancelFlag )
import Language.Code.Hoist (Split, splitHoistable, hoistedIterations, hoistedStuck)
import Language.Code.Typecheck (expandParsedCode)
import Language.Code.InterpretIO (IORefTypeOfBinding)
import Language.Value.Typing (strictTypeEnv)
import Language.Value.Builtins ( internalIterations, internalStuck, internalIterationLimit
                               , internalEscapeRadius, internalVanishingRadius )
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Language.Environment
import Language.Typecheck (TC(..), ppError)
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Code.Parser
import Control.Concurrent.MVar
import Task.Concurrent (forPool_, renderConcurrency)
import Task.Block (progressivePlan)
import Language.Parser.SourceRange
import Language.Value.Evaluator
import Foreign
import Data.PNG
import Actor.Viewer.Sphere (sphereViewName, onTheSphere, Rotation, Projection(..), ScreenView(..), rotationFacing, rotateBy, sphereToPlane)
import qualified Data.Map as Map
import Text.Printf (printf)

data ViewerContext env = ViewerContext
  { vcContext :: Context DynamicValue env
  , vcCoord   :: Coordinate
  , vcIterLimit :: Maybe (Value
      '( '(InternalEscapeRadius, RealT) ':
         '(InternalVanishingRadius, RealT) ':
         ViewerEnv env, 'IntegerT))
  , vcEscapes  :: Maybe (Value
      '( '(InternalVanishingRadius, RealT) ':
         ViewerEnv env, 'RealT))
  , vcVanishes   :: Maybe (Value
      '( ViewerEnv env, 'RealT))
  }

-- | Build a 'ViewerContext' from already-resolved (non-reactive) pieces:
-- the config context, the coordinate variable's name, and the raw
-- iteration-limit\/escape-radius\/vanishing-radius expressions (if given).
-- Extracted from 'Actor.Viewer.Complex.ComplexViewer's @code:@ codec so
-- that a caller which has already resolved these values some other way
-- (e.g. the unified-script builder, which has no 'Dynamic' dependencies
-- to wait on) can build the same 'ViewerContext' without duplicating the
-- internal-argument bookkeeping below.
buildViewerContext :: forall env
                     . EnvironmentProxy env
                    -> Context DynamicValue env
                    -> Coordinate
                    -> Maybe ParsedValue
                    -> Maybe ParsedValue
                    -> Maybe ParsedValue
                    -> Either String (ViewerContext env)
buildViewerContext env vcContext vcCoord mIterLimit mEscape mVanish =
  withEnvironment env $ do
    let assertAbsentViewerArgs :: forall e t
                                 . EnvironmentProxy e
                                -> (MissingViewerArgs e => Either String t)
                                -> Either String t
        assertAbsentViewerArgs e action =
          fromMaybe (Left "Internal error") (assertMissingViewerArgs e action)

    vcIterLimit <- case mIterLimit of
      Nothing -> Right Nothing
      Just pv -> assertAbsentViewerArgs env $
        case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
          Absent pf -> recallIsAbsent pf $
            case lookupEnv (Proxy @InternalEscapeRadius) RealType env of
              Absent pf' -> recallIsAbsent pf' $ case pv `atType` IntegerType of
                TC (Left err) -> Left ("Error with internal iteration limit: " ++ ppError err)
                TC (Right x)  -> Right (Just x)
              _ -> Left "Internal error"
          _ -> Left "Internal error"

    vcEscapes <- case mEscape of
      Nothing -> Right Nothing
      Just pv -> assertAbsentViewerArgs env $
        case lookupEnv (Proxy @InternalVanishingRadius) RealType env of
          Absent pf -> recallIsAbsent pf $ case pv `atType` RealType of
            TC (Left err) -> Left ("Error with internal escape radius: " ++ ppError err)
            TC (Right x)  -> Right (Just x)
          _ -> Left "Internal error"

    vcVanishes <- case mVanish of
      Nothing -> Right Nothing
      Just pv -> assertAbsentViewerArgs env $
        case pv `atType` RealType of
          TC (Left err) -> Left ("Error with internal vanishing radius: " ++ ppError err)
          TC (Right x)  -> Right (Just x)

    Right ViewerContext{..}

data SomeViewerWithContext where
  SomeViewerWithContext :: forall env
     . MissingViewerArgs env
    => Context DynamicValue env
    -> Code (ViewerEnv env)
    -> Maybe (Code env)
       -- ^ The part of the viewer's code that's the same for every pixel,
       -- to be run once per render, before the rest (see
       -- 'hoistViewerCode' and 'hoistedViewerFunction'): its results are
       -- variables of the context, which the rest reads.
    -> SomeViewerWithContext

-- | A block of pixels for a viewer to render (see 'ViewerFunction').
data ViewerArgs = ViewerArgs
  { vaPoint      :: (Double, Double)
    -- ^ the point of the image's first pixel (its top left one), not
    --   the block's: see 'vaOffset'
  , vaOffset     :: (Int32, Int32)
    -- ^ where the block's first pixel is in the image (its column and
    --   row). A pixel's point is the image's first one, plus its column
    --   (or minus its row) times the step: from the image's origin and
    --   the pixel's index in it, not stepped to from the block's start,
    --   so a pixel's point (to the last bit) doesn't depend on how the
    --   image is cut into blocks.
  , vaStep       :: (Double, Double)
    -- ^ how far apart the block's pixels are, in the model
  , vaPixel      :: (Double, Double)
    -- ^ a pixel's width and height as the viewer's code sees them (its
    --   @pixel_size@, or @pixel_width@ and @pixel_height@): usually the
    --   step, but a pixel of the screen, as the user sees one, where the
    --   block is sampled more finely than that (a smoothed render, or a
    --   frontend drawing at a high-DPI screen's density)
  , vaWidth      :: Int32
  , vaHeight     :: Int32
  , vaSubsamples :: Int32
  , vaBuffer     :: Ptr Word8
  }

data CodeWithArgs where
  CodeWithArgs :: forall env
                . MissingViewerArgs env
               => IO (Either String (Context HaskellValue env))
               -> Maybe (Code (ViewerEnv env))
               -> Dynamic (ViewerFunction env)
               -> CodeWithArgs

data Viewer = Viewer
  { vTitle     :: Variable String
  , vSize      :: Variable Dimensions
  , vPosition  :: Variable Dimensions
  , vCanResize :: Bool
  , vCenter    :: Variable (Double, Double)
  , vPixelSize :: Variable Double
    -- ^ The width of a pixel, in model coordinates.
  , vAspect    :: Variable Double
    -- ^ The height of a pixel over its width. Always 1 for a complex
    -- viewer (see 'vCoordinate'), whose pixels are square.
  , vOriginalCenter    :: (Double, Double)
  , vOriginalPixelSize :: Double
  , vOriginalAspect    :: Double
    -- ^ The view the script's own @center=@\/@pixelSize=@\/@aspect=@
    -- attributes describe (or the defaults, without them). 'vCenter',
    -- 'vPixelSize', and 'vAspect' start out here too, unless a frontend
    -- restores a saved view over them; either way, a frontend's
    -- "original view" action comes back to this one.
  , vCoordinate :: Coordinate
    -- ^ How the viewer's points are named: a complex viewer's pixels are
    -- square, while a real viewer's aspect can be changed.
  , vView :: Variable ViewKind
    -- ^ Whether a complex viewer shows the plane or the sphere. On the
    -- sphere, 'vCenter' is the globe's center on the screen (0), and
    -- 'vPixelSize' is in units of the sphere's radius.
  , vRotation :: Variable Rotation
    -- ^ The globe's orientation, on the sphere (see "Actor.Viewer.Sphere").
  , vOriginalView :: ViewKind
  , vOriginalRotation :: Rotation
  , vOriginalPlaneView :: ((Double, Double), Double)
    -- ^ The plane view the script describes (center and pixel size),
    -- whichever view it starts with.
  , vOriginalSpherePixelSize :: Double
  , vSaveView  :: IO ()
  , vListen    :: IO () -> IO (IO ())
  , vDrawCmds  :: IO [[DrawCommand]]
  , vDrawCmdsChanged :: IO Bool
  , vCodeWithArgs :: CodeWithArgs
  , vTools     :: Dynamic [Tool]
  , vSelectTool :: Maybe Tool
    -- ^ The built-in "Select ..." tool (also in 'vTools'), when the
    --   viewer's coordinate is a configuration variable (or a pair of
    --   them) that clicking can set. A frontend selects it when it first
    --   opens the viewer, instead of navigating.
  , vRefreshOverlay :: CancelFlag -> PixelDims -> IO ()
    -- ^ Run the viewer's overlay's @on refresh:@ block (if it has an
    --   overlay), for a view with the given pixel size: its drawings go
    --   to a layer below every tool's. A frontend runs it whenever the
    --   view changes, and when 'vListenOverlay' says to, and before it
    --   draws tool drawings anywhere they can't be kept up to date that
    --   way (a capture, a movie frame). Safe to call from any thread, and
    --   never throws: a failing overlay only reports its error on stderr.
    --   Setting the flag stops the run, with no effect (see 'HandlerRun');
    --   'refreshOverlay' runs it with a flag of its own.
  , vStopOverlay :: IO ()
    -- ^ Turn the overlay off: its drawings are erased, and refreshing it
    --   does nothing from then on, for this build of the viewer (a
    --   frontend does this when the user stops a slow overlay, which
    --   would otherwise only run again at the next refresh).
  , vListenOverlay :: IO () -> IO (IO ())
    -- ^ Run an action whenever the overlay may need refreshing because
    --   a configuration value changed (never, without an overlay);
    --   returns the action that stops listening. The action shouldn't
    --   run the overlay itself, since it may be called while a value is
    --   being set: it should ask for a refresh later.
  , vBuildToolHandler
      :: Int
      -> SomeContext EventArgument_
      -> ParsedToolScript
      -> IO (Either String ToolDispatcher)
    -- ^ Build a tool's event dispatcher for the given draw layer.  Supplied by
    --   the backend (interpreter or JIT-compiled); see 'ToolRunner'.
  }

-- | Render a 'Viewer's base per-pixel script into an in-memory RGB8 buffer
-- (already downsampled, if requested) and hand it, with the model-space
-- center and pixel size it corresponds to and its pixel dimensions, to the
-- given continuation. This is the shared rendering step behind
-- 'snapshotToFile' (and 'renderViewerBuffer'); it's a separate entry point
-- so that a caller wanting to draw something /on top of/ the base render
-- -- e.g. a tool's recorded 'DrawCommand's -- can do so by drawing into the
-- continuation's buffer before encoding it, without re-deriving any of this
-- per-pixel setup.
--
-- Note that the center\/pixel-size passed to the continuation is always the
-- viewer's own nominal 'vCenter'\/'vPixelSize' (relative to the *final*
-- buffer, whatever its dimensions): downsampling supersamples internally at
-- twice the resolution and half the pixel size, then averages back down,
-- so the buffer handed to the continuation always covers the same
-- model-space extent as the viewer's own 'vSize' at 'vPixelSize' -- this
-- spares a caller doing its own model-space\/pixel-space placement (e.g.
-- compositing tool-drawn annotations) from having to duplicate that
-- halve-then-double arithmetic to find out what coordinate system the
-- buffer it was handed is actually in.
withRenderedViewerBuffer
  :: Viewer
  -> Bool
  -> (ScreenView -> (Word32, Word32) -> Ptr Word8 -> IO a)
  -> IO (Either String (ViewerIssues, a))
withRenderedViewerBuffer viewer downsample withBuffer =
  renderViewerBuffer downsample viewer >>= \case
    Left err -> pure (Left err)
    Right RenderedBuffer{..} -> withForeignPtr rbPixels $ \buf ->
      Right . (rbIssues,) <$>
        withBuffer (ScreenView rbCenter rbPixelSize rbProjection) (fromIntegral rbWidth, fromIntegral rbHeight) buf

-- | A viewer's base render: @3 * rbWidth * rbHeight@ bytes of RGB8, with
-- the model-space center and pixel size it was drawn at.
data RenderedBuffer = RenderedBuffer
  { rbPixels    :: ForeignPtr Word8
  , rbWidth     :: Int
  , rbHeight    :: Int
  , rbCenter    :: (Double, Double)
  , rbPixelSize :: (Double, Double)
    -- ^ the width and height of a pixel
  , rbProjection :: Projection
    -- ^ how model points get onto the image (on the plane, or the globe)
  , rbIssues    :: ViewerIssues
  }

-- | Render a viewer's base per-pixel script at its current size and view,
-- supersampled at twice the size and averaged down when @downsample@ is
-- set. The image is split into horizontal strips, rendered concurrently
-- across every capability (so a program built without @-threaded@, or
-- run with one capability, renders them one after another). Every
-- pixel comes out the same whatever the split, except that a viewer
-- adding points to an unlocked autocolor group colors them in the order
-- they're first seen, which then varies from run to run (see
-- "Actor.Autocolor").
renderViewerBuffer :: Bool -> Viewer -> IO (Either String RenderedBuffer)
renderViewerBuffer = renderViewerBufferAt 1

-- | The point under the pointer, as a viewer's "Show pointer position"
-- shows it: a complex number, or a real viewer's pair of reals, to 14
-- places (lined up, whatever the sign); "…" off the model (as off the
-- globe).
pointerText :: Coordinate -> Maybe (Double, Double) -> String
pointerText coord = \case
  Nothing -> "…"
  Just xy@(x, _) -> let txt = shown xy in if x < 0 then txt else ' ' : txt
  where
    shown :: (Double, Double) -> String
    shown (x, y) = case coord of
      RealCoordinates{} -> printf "(%.14f, %.14f)" x y
      ComplexCoordinate{}
        | y < 0     -> printf "%.14f - %.14f𝑖" x (negate y)
        | otherwise -> printf "%.14f + %.14f𝑖" x y

-- | 'renderViewerBuffer', for a viewer whose pixels are a fraction of the
-- screen's (logical) pixels: @scale@ of them make one, which is what the
-- viewer's code sees as its @pixel_size@ (a frontend drawing at a
-- high-DPI screen's density). A smoothed render's code sees the pixel's
-- size too, not the finer step it's sampled at.
renderViewerBufferAt :: Double -> Bool -> Viewer -> IO (Either String RenderedBuffer)
renderViewerBufferAt scale downsample Viewer{..} = case vCodeWithArgs of
  CodeWithArgs getArgs _ code -> getArgs >>= \case
    Left err -> pure (Left err)
    Right vaArgs -> do
      Dimensions (w, h) <- getDynamic vSize
      pixelSize <- getDynamic vPixelSize
      aspect <- getDynamic vAspect
      projection <- viewProjection vView vRotation
      center <- getDynamic vCenter
      ViewerFunction bind <- getDynamic code
      fn <- bind vaArgs
      let s = if downsample then 2 else 1
          bigW = w * s
          ((x0, y0), (px, py)) = imageGeometry s w h center pixelSize aspect
          -- strips of output rows; enough of them to keep every
          -- capability busy even if some strips are slower than others
          stripRows = max 1 (min 16 (h `div` 64))
          strips = [ (r, min stripRows (h - r)) | r <- [0, stripRows .. h - 1] ]
      caps <- renderConcurrency
      out <- mallocForeignPtrBytes (3 * w * h)
      issuesVar <- newMVar mempty
      withForeignPtr out $ \outPtr ->
        forPool_ caps strips $ \(row, rows) ->
          allocaBytes (3 * bigW * rows * s) $ \big -> do
            issues <- fn ViewerArgs
              { vaPoint = (x0, y0)
              , vaOffset = (0, fromIntegral (row * s))
              , vaStep = (px, py)
              , vaPixel = (pixelSize * scale, pixelSize * scale * aspect)
              , vaWidth = fromIntegral bigW
              , vaHeight = fromIntegral (rows * s)
              , vaSubsamples = 1
              , vaBuffer = big }
            modifyMVar_ issuesVar (pure . (<> issues))
            let dest = outPtr `plusPtr` (3 * w * row)
            if downsample then downsampleRows bigW w rows w big dest
                          else copyBytes dest big (3 * w * rows)
      issues <- readMVar issuesVar
      pure (Right (RenderedBuffer out w h center (pixelSize, pixelSize * aspect) projection issues))

-- | Where a viewer's image is, @s@ times as fine as the viewer's pixels
-- each way (2, to smooth it; @w@ by @h@ of the viewer's pixels): its
-- first pixel's point, and the step from one pixel to the next.
imageGeometry :: Int -> Int -> Int -> (Double, Double) -> Double -> Double
              -> ((Double, Double), (Double, Double))
imageGeometry s w h (cx, cy) pixelSize aspect =
  let bigW = w * s
      px = pixelSize / fromIntegral s
      py = px * aspect
  in ((cx - fromIntegral bigW * px / 2, cy + fromIntegral (h * s) * py / 2), (px, py))

-- | A piece of a viewer's image: its first pixel's column and row, and
-- its width and height, in pixels.
data ImagePiece = ImagePiece { ipX :: !Int, ipY :: !Int, ipWidth :: !Int, ipHeight :: !Int }
  deriving (Eq, Ord, Show)

-- | How to render a viewer's image progressively ('renderProgressively').
data Progressive = Progressive
  { prPieceSize :: Int
    -- ^ the pieces' size (each square, but at the right and bottom edges)
  , prConcurrency :: Int
    -- ^ how many pieces to render at once
  , prScale :: Double
    -- ^ how many of the viewer's pixels make one of the screen's (as for
    --   'renderViewerBufferAt')
  , prSmooth :: Bool
    -- ^ smoothed, as 'renderViewerBuffer' smooths: a last pass
  , prShown :: Int -> Int -> Bool
    -- ^ which pieces (by column and row, in pieces) already show
    --   something (the last image, moved into this view): they skip the
    --   coarse passes
  , prStale :: IO Bool
    -- ^ whether the render's no longer wanted: asked before each piece's
    --   pass, and once it says so, the rest of the render is skipped
  , prDrawn :: RenderedBuffer -> ImagePiece -> IO ()
    -- ^ a pass of a piece is in the image (whose issues so far aren't
    --   counted yet)
  }

-- | Render a viewer's image progressively, in pieces and passes, as the
-- desktop app's tiles are ('Task.Block.progressivelyOver', whose order
-- this follows: coarse passes over what isn't shown yet, then every piece
-- at full resolution, then, smoothed, at twice that). Each pixel's point
-- is the image's origin plus its index ('vaOffset'), so once it's done
-- the image is just what 'renderViewerBufferAt' renders, to the byte.
-- Nothing, if it's abandoned ('prStale').
renderProgressively :: Progressive -> Viewer -> IO (Either String (Maybe RenderedBuffer))
renderProgressively Progressive{..} Viewer{..} = case vCodeWithArgs of
  CodeWithArgs getArgs _ code -> getArgs >>= \case
    Left err -> pure (Left err)
    Right vaArgs -> do
      Dimensions (w, h) <- getDynamic vSize
      pixelSize <- getDynamic vPixelSize
      aspect <- getDynamic vAspect
      projection <- viewProjection vView vRotation
      center <- getDynamic vCenter
      ViewerFunction bind <- getDynamic code
      fn <- bind vaArgs
      out <- mallocForeignPtrBytes (max 1 (3 * w * h))
      issuesVar <- newMVar mempty
      abandoned <- newIORef False
      let image = RenderedBuffer out w h center (pixelSize, pixelSize * aspect) projection mempty
          pixel = (pixelSize * prScale, pixelSize * prScale * aspect)
          (origin1, (px, py)) = imageGeometry 1 w h center pixelSize aspect
          (origin2, step2) = imageGeometry 2 w h center pixelSize aspect
          call origin (ox, oy) step (bw, bh) buf = do
            issues <- fn ViewerArgs { vaPoint = origin, vaOffset = (fromIntegral ox, fromIntegral oy)
                                    , vaStep = step, vaPixel = pixel
                                    , vaWidth = fromIntegral bw, vaHeight = fromIntegral bh
                                    , vaSubsamples = 1, vaBuffer = buf }
            modifyMVar_ issuesVar (pure . (<> issues))
          piece rate (x0, pw) (y0, ph) outPtr
            -- smoothed: the piece at twice the size, averaged down
            | rate > 0 = allocaBytes (3 * 4 * pw * ph) $ \big -> do
                call origin2 (2 * x0, 2 * y0) step2 (2 * pw, 2 * ph) big
                downsampleRows (2 * pw) pw ph w big (outPtr `plusPtr` (3 * (y0 * w + x0)))
            -- one sample a skip x skip square (a pixel, at full resolution)
            | otherwise = do
                let skip = 2 ^ negate rate :: Int
                    (cw, ch) = (pw `div` skip, ph `div` skip)
                allocaBytes (3 * cw * ch) $ \small -> allocaBytes (3 * pw) $ \line -> do
                  call origin1 (x0 `div` skip, y0 `div` skip)
                       (px * fromIntegral skip, py * fromIntegral skip) (cw, ch) small
                  forM_ [0 .. ch - 1] $ \j -> do
                    let row = small `plusPtr` (3 * cw * j)
                    if skip == 1 then copyBytes line row (3 * pw) else
                      forM_ [0 .. cw - 1] $ \i -> forM_ [0 .. skip - 1] $ \d ->
                        copyBytes (line `plusPtr` (3 * (skip * i + d))) (row `plusPtr` (3 * i)) 3
                    forM_ [0 .. skip - 1] $ \d ->
                      copyBytes (outPtr `plusPtr` (3 * ((y0 + skip * j + d) * w + x0))) line (3 * pw)
      plan <- progressivePlan prPieceSize (if prSmooth then 1 else 0) w h prShown
      withForeignPtr out $ \outPtr ->
        forPool_ prConcurrency plan $ \(_, rate, (x, pw), (y, ph)) -> do
          -- (once it's stale, the rest is skipped: the pieces being
          -- rendered still finish)
          stop <- readIORef abandoned >>= \case
            True -> pure True
            False -> prStale >>= \s -> s <$ when s (writeIORef abandoned True)
          unless stop $ do
            let (x0, y0) = (prPieceSize * x, prPieceSize * y)
            piece rate (x0, pw) (y0, ph) outPtr
            prDrawn image (ImagePiece x0 y0 pw ph)
      readIORef abandoned >>= \case
        True -> pure (Right Nothing)
        False -> readMVar issuesVar <&> \issues -> Right (Just image { rbIssues = issues })

-- | Average each 2x2 block of a @bigW@-pixel-wide RGB8 buffer down to
-- one pixel of a @w@-pixel-wide piece of one whose rows are @stride@
-- pixels apart, for @rows@ output rows.
downsampleRows :: Int -> Int -> Int -> Int -> Ptr Word8 -> Ptr Word8 -> IO ()
downsampleRows bigW w rows stride big dest =
  forM_ [0 .. rows - 1] $ \j -> forM_ [0 .. w - 1] $ \i -> forM_ [0, 1, 2] $ \k -> do
    let ix = 3 * (2 * i) + 3 * bigW * (2 * j) + k
        at o = fromIntegral <$> peekElemOff big o :: IO Word16
    c1 <- at ix
    c2 <- at (ix + 3)
    c3 <- at (ix + 3 * bigW)
    c4 <- at (ix + 3 * bigW + 3)
    pokeElemOff dest (3 * i + 3 * stride * j + k) (fromIntegral ((c1 + c2 + c3 + c4 + 2) `div` 4))

-- | Show a complex viewer's points on the plane or on the sphere,
-- keeping the point in the middle of the view there: on the sphere,
-- the globe turns to face it; on the plane, the view is centered on the
-- point the globe faced (or 0, if that was ∞). Each starts at the zoom
-- the script gives it.
switchView :: Viewer -> ViewKind -> IO ()
switchView v kind = do
  old <- getDynamic (vView v)
  when (old /= kind) $ case kind of
    SphereView -> do
      c <- getDynamic (vCenter v)
      setValue' (vRotation v) (rotationFacing c)
      setValue' (vCenter v) (0, 0)
      setValue' (vPixelSize v) (vOriginalSpherePixelSize v)
      setValue' (vView v) SphereView
    PlaneView -> do
      r <- getDynamic (vRotation v)
      let c@(cx, cy) = sphereToPlane (rotateBy r (0, 0, 1))
          finite a = not (isNaN a || isInfinite a) && abs a < 1e300
      setValue' (vCenter v) (if finite cx && finite cy then c else (0, 0))
      setValue' (vPixelSize v) (snd (vOriginalPlaneView v))
      setValue' (vView v) PlaneView

-- | Center a viewer's view on a point: on the sphere, turn the globe to
-- face it.
centerViewOn :: Viewer -> (Double, Double) -> IO ()
centerViewOn v c = getDynamic (vView v) >>= \case
  PlaneView -> setValue' (vCenter v) c
  SphereView -> setValue' (vRotation v) (rotationFacing c)

-- | How a viewer's model points get onto its screen now.
viewProjection :: Variable ViewKind -> Variable Rotation -> IO Projection
viewProjection viewV rotationV = getDynamic viewV >>= \case
  PlaneView -> pure FlatProjection
  SphereView -> GlobeProjection <$> getDynamic rotationV

-- | Render a viewer to a PNG file, returning any issues the render ran into.
snapshotToFile :: Viewer -> Bool -> FilePath -> IO (Either String ViewerIssues)
snapshotToFile viewer downsample path =
  fmap fst <$>
    withRenderedViewerBuffer viewer downsample (\_ dim buf -> encodeBufferToPngFile dim buf path)

onParameterChanges :: Viewer -> IO () -> IO (IO ())
onParameterChanges = vListen

-- | A viewer's compiled script. Given the configuration's values, it
-- gives a function rendering a block of pixels with them, reporting any
-- problems the script ran into along the way. A render binds the values
-- once and then renders its many blocks, often concurrently: the backend
-- prepares the values for its code (say, copying a list into the layout
-- compiled code reads) when they're bound, not for every block. What's
-- prepared lives as long as the block function does.
newtype ViewerFunction env =
  ViewerFunction (Context HaskellValue env -> IO (ViewerArgs -> IO ViewerIssues))

-- | Render one block with a viewer function, binding the values just for
-- it. (A render of many blocks should bind them once instead.)
runViewerFunction :: ViewerFunction env -> Context HaskellValue env -> ViewerArgs -> IO ViewerIssues
runViewerFunction (ViewerFunction bind) args block = bind args >>= ($ block)

-- | Problems a viewer's script ran into while rendering. A pixel that hit
-- one of them is drawn magenta.
data ViewerIssues = ViewerIssues
  { viOutOfMemory :: Bool  -- ^ some pixel's lists or text didn't fit in
                           --   its scratch memory
  , viBadIndex    :: Bool  -- ^ some pixel indexed a list out of range
  } deriving (Eq, Show)

instance Semigroup ViewerIssues where
  ViewerIssues a b <> ViewerIssues c d = ViewerIssues (a || c) (b || d)

instance Monoid ViewerIssues where
  mempty = ViewerIssues False False

-- | A message for the user explaining a render's issues, if there were any.
describeViewerIssues :: ViewerIssues -> Maybe String
describeViewerIssues ViewerIssues{..}
  | null problems = Nothing
  | otherwise = Just ("Magenta pixels: the script " ++ intercalate ", and " problems ++ ".")
  where
    problems =
      [ "ran out of scratch memory for its lists or text" | viOutOfMemory ]
      ++ [ "indexed a list or table out of range" | viBadIndex ]

assertMissingViewerArgs :: forall env t
                         . EnvironmentProxy env
                        -> (MissingViewerArgs env => t)
                        -> Maybe t
assertMissingViewerArgs env ok = do
  let assertAbsent :: forall name -> KnownSymbol name => Maybe (AssertAbsent name env)
      assertAbsent name = case lookupEnv' (Proxy @name) env of
        Absent' pf -> recallIsAbsent pf (Just AssertAbsent)
        _          -> Nothing

  AssertAbsent <- assertAbsent InternalX
  AssertAbsent <- assertAbsent InternalY
  AssertAbsent <- assertAbsent InternalDX
  AssertAbsent <- assertAbsent InternalDY
  AssertAbsent <- assertAbsent "color"
  pure ok

data AssertAbsent name env where
  AssertAbsent :: forall name env. NotPresent name env => AssertAbsent name env


-- | Compiles a viewer's code, given the project's autocolor lookup (for
-- @autocolor@ and @get_autocolor@; see "Actor.Autocolor").
newtype ViewerCompiler = ViewerCompiler
  { withCompiledViewer :: forall env t
                        . (MissingViewerArgs env, KnownEnvironment env)
                       => AutocolorLookup
                       -> Code (ViewerEnv env)
                       -> (ViewerFunction env -> IO t)
                       -> IO t }

-- | Resolve a viewer's already-parsed body code ('ParsedCode', the untyped
-- statement syntax 'Language.Code.Parser.parseParsedCode' produces) against
-- a specific 'ViewerContext', typechecking it there. Takes a
-- 'ParsedCode' rather than a raw source string so that a caller which
-- already has one in hand (e.g. the unified-script builder, which parsed
-- the whole project file in a single Earley pass) never needs to
-- re-serialize and re-tokenize it -- the normal, string-sourced YAML path
-- (see 'Actor.Viewer.Complex.ComplexViewer') just parses its @code:@
-- string into a 'ParsedCode' once and passes that in here, same as
-- before.
parseViewerScript :: forall env
                   . Maybe String
                  -> ViewerContext env
                  -> ParsedCode
                  -> Either (SourceRange, String) (Code (ViewerEnv env))
parseViewerScript mpx ViewerContext{..} c = do
  let env = contextToEnv vcContext
  withEnvironment env $ do

    let declareE :: forall n -> forall t e. KnownSymbol n => TypeProxy t -> EnvironmentProxy e
                 -> Either (SourceRange, String) (EnvironmentProxy ( '(n,t) ': e ))
        declareE n t e = case lookupEnv' (Proxy @n) e of
          Found'{} -> throwError (NoSourceRange, "Internal error: duplicate definition of `" ++
                                   symbolVal (Proxy @n) ++ "`.")
          Absent' pf -> pure (recallIsAbsent pf $ declare t e)

    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalViewerEnv env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalX               RealType
        <=< declareE InternalY               RealType
        <=< declareE InternalDX              RealType
        <=< declareE InternalDY              RealType
        <=< declareE "color"                 ColorType
      ) env

    withEnvironment env' $ do

      let getVar :: forall n -> forall t e.  KnownSymbol n
                 => TypeProxy t -> EnvironmentProxy e -> Either (SourceRange, String) (Value '(e, t))
          getVar n t e = withEnvironment e $ case lookupEnv (Proxy @n) t e of
            Found pf -> pure (Var (Proxy @n) t pf)
            _ -> throwError (NoSourceRange,
                             "INTERNAL ERROR, there was a problem locating `" ++ symbolVal (Proxy @n) ++ "`")

      let i = Const (Scalar ComplexType (0 :+ 1))
          internal :: forall n -> KnownSymbol n => RealIn
          internal n = RealIn (getVar n RealType)

      -- Bind the pixel size (and a real viewer's pixel width and
      -- height) around the body; each must be a new name.
      let pixelVars = (fromMaybe "[unused] pixel size" mpx, internal InternalDX) : case vcCoord of
            ComplexCoordinate _ -> []
            RealCoordinates _ _ -> [ (pixelWidthVar, internal InternalDX)
                                   , (pixelHeightVar, internal InternalDY) ]
          -- (a complex viewer given its view can show the sphere; see
          -- "Actor.Viewer.Sphere")
          c' = case vcCoord of
            ComplexCoordinate z | sphereViewName `Map.member` envToMap env -> onTheSphere z c
            _ -> c
          body :: forall e. KnownEnvironment e => EnvironmentProxy e -> Either (SourceRange, String) (Code e)
          body e = bindFreshReals pixelVars e $ \e' ->
            left (errorLocation &&& unlines . pp) (runParsedCode e' c')

      -- Bind the viewer's coordinate(s) to the point: a variable that's
      -- already defined (a configuration value) is set, and must have
      -- the right type; otherwise, it's a new variable.
      code :: Code (InternalViewerEnv env) <- case vcCoord of
        ComplexCoordinate z -> case someSymbolVal z of
          SomeSymbol zp -> do
            x <- getVar InternalX RealType env'
            y <- getVar InternalY RealType env'
            bindOrDeclare zp ComplexType (R2C x + i * R2C y) env' body
        RealCoordinates xn yn -> case (someSymbolVal xn, someSymbolVal yn) of
          (SomeSymbol xp, SomeSymbol yp) -> do
            x <- getVar InternalX RealType env'
            bindOrDeclare xp RealType x env' $ \e1 -> do
              y <- getVar InternalY RealType e1
              bindOrDeclare yp RealType y e1 body

      -- (a viewer can only read tables; see 'Language.Code.TableWrite')
      when (writesTables code) $ throwError (NoSourceRange,
        "Only a tool can reset a table or write to one: a viewer can only read tables.")

      -- Now bind all of the bookkeeping variables (the iteration count
      -- and whether a loop got stuck starting from where the hoisted part
      -- of the code left them, if it has loops: see 'hoistViewerCode')
      let (_, code') = (env', code)
                     & letInEnv (hoistedOr hoistedIterations IntegerType 0)
                     & letInEnv (hoistedOr hoistedStuck BooleanType False)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 100)) vcIterLimit)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 10.0)) vcEscapes)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 0.0001)) vcVanishes)

      pure code'

-- | The value of a variable, if the environment has one by that name and
-- type, or else a constant.
hoistedOr :: forall e t. KnownEnvironment e => String -> TypeProxy t -> HaskellType t -> Value '(e, t)
hoistedOr name ty dflt = case someSymbolVal name of
  SomeSymbol n -> case lookupEnv n ty (envProxy (Proxy @e)) of
    Found pf -> Var n ty pf
    _ -> Const (Scalar ty dflt)

-- | Split a viewer's code (see "Language.Code.Hoist") into the part that's
-- the same for every pixel and the rest, given its pixel size variable,
-- its coordinate, and the environment of its configuration (and view).
-- 'Nothing' when there's nothing worth running once, or the code doesn't
-- check (which building it as it is will report).
hoistViewerCode :: Maybe String -> Coordinate -> EnvironmentProxy env -> ParsedCode -> Maybe Split
hoistViewerCode mpx coord env code =
  either (const Nothing) (splitHoistable varying) (expandParsedCode tenv code)
  where
    coordinates = case coord of
      ComplexCoordinate z -> [(z, ComplexT)]
      RealCoordinates x y -> [(x, RealT), (y, RealT)]
    pixelSizes = fromMaybe "[unused] pixel size" mpx : case coord of
      ComplexCoordinate _ -> []
      RealCoordinates _ _ -> [pixelWidthVar, pixelHeightVar]
    pixelInternals = [ symbolVal (Proxy @InternalX), symbolVal (Proxy @InternalY)
                     , symbolVal (Proxy @InternalDX), symbolVal (Proxy @InternalDY) ]
    varying = Set.fromList ("color" : map fst coordinates ++ pixelSizes ++ pixelInternals)
    -- (the types of the names the viewer's code sees, as 'parseViewerScript'
    -- binds them)
    tenv = strictTypeEnv $ Map.unions
      [ Map.map (\(SomeType t) -> typeToFS t) (envToMap env)
      , Map.fromList coordinates
      , Map.fromList [ (n, RealT) | n <- pixelSizes ++ pixelInternals ]
      , Map.fromList [ ("color", ColorT), (internalIterations, IntegerT), (internalStuck, BooleanT)
                     , (internalIterationLimit, IntegerT), (internalEscapeRadius, RealT)
                     , (internalVanishingRadius, RealT) ] ]

-- | A viewer function that first runs the part of a viewer's code that's
-- the same for every pixel (compiled, or interpreted, by the given
-- 'ToolExec'), once each time it's given the configuration's values,
-- and then renders with the given function, with those values as that
-- part left them. If that part fails (an index out of range, say), every
-- pixel is magenta, as it would have been had each pixel run it.
hoistedViewerFunction :: ToolExec -> EnvironmentProxy env -> Code env -> ViewerFunction env
                      -> IO (ViewerFunction env)
hoistedViewerFunction exec env prefix (ViewerFunction bind) = do
  invoke <- exec env prefix
  pure $ ViewerFunction $ \args -> do
    refs <- mapContextM @HaskellValue @IORefTypeOfBinding (\_ _ -> newIORef) args
    let eargs = mapContext @IORefTypeOfBinding @EventArgument_
                  (\_ _ r -> EventArgument (Just <$> readIORef r) (Just (\_ v -> writeIORef r v))) refs
    flag <- newCancelFlag
    try (startHandlerRun (invoke eargs) flag >>= id) >>= \case
      Right () -> mapContextM @IORefTypeOfBinding @HaskellValue (\_ _ -> readIORef) refs >>= bind
      Left (err :: SomeException) -> pure $ \ViewerArgs{..} -> do
        let (w, h) = (fromIntegral vaWidth, fromIntegral vaHeight) :: (Int, Int)
        forM_ [0 .. w * h - 1] $ \k -> do
          pokeElemOff vaBuffer (3 * k + 0) 0xff
          pokeElemOff vaBuffer (3 * k + 1) 0x00
          pokeElemOff vaBuffer (3 * k + 2) 0xff
        let memory = "memory" `isInfixOf` show err
        pure ViewerIssues { viOutOfMemory = memory, viBadIndex = not memory }

cloneViewer :: Viewer -> IO Viewer
cloneViewer v = do
  newTitle     <- newVariable =<< getDynamic (vTitle v)
  newSize      <- newVariable =<< getDynamic (vSize v)
  newPosition  <- newVariable =<< getDynamic (vPosition v)
  newCenter    <- newVariable =<< getDynamic (vCenter v)
  newPixelSize <- newVariable =<< getDynamic (vPixelSize v)
  newAspect    <- newVariable =<< getDynamic (vAspect v)
  newView      <- newVariable =<< getDynamic (vView v)
  newRotation  <- newVariable =<< getDynamic (vRotation v)
  pure $ Viewer
    { vTitle = newTitle
    , vSize  = newSize
    , vPosition = newPosition
    , vCanResize = vCanResize v
    , vCenter = newCenter
    , vPixelSize = newPixelSize
    , vAspect = newAspect
    , vOriginalCenter = vOriginalCenter v
    , vOriginalPixelSize = vOriginalPixelSize v
    , vOriginalAspect = vOriginalAspect v
    , vCoordinate = vCoordinate v
    , vView = newView
    , vRotation = newRotation
    , vOriginalView = vOriginalView v
    , vOriginalRotation = vOriginalRotation v
    , vOriginalPlaneView = vOriginalPlaneView v
    , vOriginalSpherePixelSize = vOriginalSpherePixelSize v
    , vSaveView = vSaveView v

    , vListen = vListen v
    , vTools = vTools v
    , vSelectTool = vSelectTool v
    , vRefreshOverlay = vRefreshOverlay v
    , vStopOverlay = vStopOverlay v
    , vListenOverlay = vListenOverlay v
    , vCodeWithArgs = vCodeWithArgs v
    , vDrawCmds = vDrawCmds v
    , vDrawCmdsChanged = vDrawCmdsChanged v
    , vBuildToolHandler = vBuildToolHandler v
    }

-- | Refresh a viewer's overlay for its current view (see 'vRefreshOverlay').
refreshOverlay :: Viewer -> IO ()
refreshOverlay v = newCancelFlag >>= \flag -> refreshOverlayWith flag v

-- | 'refreshOverlay', stopped (with no effect) when the flag is set.
refreshOverlayWith :: CancelFlag -> Viewer -> IO ()
refreshOverlayWith flag v = do
  px <- getDynamic (vPixelSize v)
  aspect <- getDynamic (vAspect v)
  vRefreshOverlay v flag (px, px * aspect)

-- | A fallback viewer function that paints every point grey
allGrey :: ViewerFunction '[]
allGrey = ViewerFunction $ \_ -> pure $ \ViewerArgs{..} -> do
  let (w, h) = (fromIntegral vaWidth, fromIntegral vaHeight)
  forM_ [0..w - 1] $ \j ->
    forM_ [0..h - 1] $ \i -> do
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 0) 0x7f
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 1) 0x7f
      pokeElemOff vaBuffer (3 * j * w + 3 * i + 2) 0x7f
  pure mempty

defaultIterLimit, defaultMaxRadius, defaultMinRadius :: ParsedValue
defaultIterLimit = fromRight (error "INTERNAL ERROR: defaultIterLimit") $ parseParsedValue "100"
defaultMaxRadius = fromRight (error "INTERNAL ERROR: defaultMaxRadius") $ parseParsedValue "10"
defaultMinRadius = fromRight (error "INTERNAL ERROR: defaultMinRadius") $ parseParsedValue "0.0001"

------------------------------------------------------------------------
-- Backend
------------------------------------------------------------------------

-- | A factory that sets up the draw-command accumulation system for a viewer's
-- tools.  Returns three things:
--   * an action to read all accumulated draw commands (grouped by layer),
--   * an action to check whether any commands have been added since last read,
--   * a function from layer number to a 'DrawSink' for that layer.
newtype ToolRunnerFactory = ToolRunnerFactory
  { makeToolRunnerForLayer :: IO ( IO [[DrawCommand]]
                                 , IO Bool
                                 , Int -> DrawSink
                                 )
  }

-- | Bundles a viewer (shader) compiler with the tool execution machinery.
data Backend = Backend
  { bViewerCompiler    :: ViewerCompiler
  , bToolRunnerFactory :: ToolRunnerFactory
  , bToolRunner        :: ToolRunner
    -- ^ How tool event handlers are executed: 'defaultToolRunner' (interpreter)
    --   or a backend-specific JIT-compiled runner.
  , bRunOnce           :: AutocolorLookup -> ToolExec
    -- ^ How code that runs once and gives back its variables is run: the
    --   part of a viewer's code that's the same for every pixel (see
    --   'hoistedViewerFunction'). Like a tool's handler, but drawing
    --   nothing.
  }

-- | The default 'ToolRunnerFactory': tool scripts are executed by the
-- pure Haskell interpreter, and draw commands are accumulated in an
-- in-memory layer map for the UI to paint afterward.
defaultToolRunnerFactory :: ToolRunnerFactory
defaultToolRunnerFactory = ToolRunnerFactory $ do
  layersMVar <- newMVar (Map.empty :: Map Int [DrawCommand])
  dcChanged  <- newMVar False

  let consDrawCmd :: DrawCommand
                  -> Maybe [DrawCommand]
                  -> Maybe [DrawCommand]
      consDrawCmd = \case
        Clear{} -> const (Just [])
        c       -> Just . \case
          Nothing -> [c]
          Just cs -> c : cs

      emit :: Int -> DrawCommand -> IO ()
      emit n cmd' = do
        modifyMVar_ dcChanged  (pure . const True)
        modifyMVar_ layersMVar (pure . Map.alter (consDrawCmd cmd') n)

      mkSink :: Int -> DrawSink
      mkSink n = DrawSink
        { dsClear  = emit n (Clear EmptyEnvProxy)
        , dsStroke = \c -> emit n (SetStroke EmptyEnvProxy c)
        , dsFill   = \c -> emit n (SetFill EmptyEnvProxy c)
        , dsPoint  = \p -> emit n (DrawPoint EmptyEnvProxy p)
        , dsLine   = \f t -> emit n (DrawLine EmptyEnvProxy f t)
        , dsCircle = \fill r p -> emit n (DrawCircle EmptyEnvProxy fill r p)
        , dsRect   = \fill f t -> emit n (DrawRect EmptyEnvProxy fill f t)
        , dsWrite  = \txt pt -> emit n (Write EmptyEnvProxy txt pt)
        }

      getDrawCommands = do
        tryTakeMVar dcChanged >>= \case
          Nothing -> pure ()
          Just _  -> putMVar dcChanged False
        tryReadMVar layersMVar >>= \case
          Nothing -> pure [[]]
          Just m  -> pure (map reverse (Map.elems m))

      drawCommandsChanged = tryReadMVar dcChanged >>= \case
        Nothing -> pure True
        Just tf -> pure tf

  pure (getDrawCommands, drawCommandsChanged, mkSink)
