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
  , PrepScript(..)
  , PrepArrayPtr
  , PrepArrays(..)
  , ViewerCompiler(..)
  , ViewerArgs(..)
  , ViewerFunction(..)
  , ViewerIssues(..)
  , describeViewerIssues
  , Viewer(..)
  , refreshOverlay
  , CodeWithArgs(..)
  , cloneViewer
  , onParameterChanges
  , invokeViewerFunction
  , assertMissingViewerArgs
  , snapshotToFile
  , withRenderedViewerBuffer
  , RenderedBuffer(..)
  , renderViewerBuffer
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
import Actor.Event ( ToolRunner(..), defaultToolRunner, EventArgument_, ParsedToolScript, Event
                   , Coordinate(..), ViewKind(..), bindOrDeclare, RealIn(..), bindFreshReals
                   , pixelWidthVar, pixelHeightVar, PixelDims )
import Language.Environment
import Language.Typecheck (TC(..), ppError)
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Code.Parser
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar
import Task.Concurrent (forPool_)
import Language.Parser.SourceRange
import Language.Value.Evaluator
import Foreign
import Data.PNG
import Actor.Viewer.Sphere (sphereViewName, onTheSphere, Rotation, Projection(..), ScreenView(..), rotationFacing, rotateBy, sphereToPlane)
import qualified Data.Map as Map

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
    -> Maybe (PrepScript env)
    -> Code (ViewerEnv env)
    -> SomeViewerWithContext

-- | A preparation script that runs per-pixel in Haskell before the compiled viewer
-- kernel. Its outputs are written into flat arrays that the kernel reads inside its
-- pixel loop. The 'prepOutputEnv' identifies which variables are array-backed outputs;
-- the code runs in 'InternalViewerEnv env' and can 'Set' those output variables.
-- Parameterised by 'env' so callers can match on it without an existential escape.
data PrepScript (env :: Environment) where
  PrepScript :: forall prepOutputEnv env
              . KnownEnvironment prepOutputEnv
             => EnvironmentProxy prepOutputEnv
             -> Code (ViewerEnv env)
             -> PrepScript env

-- | Maps each prep output variable to a flat @Ptr Word8@ buffer.
-- The stride per element is determined by the variable's 'FSType'
-- (see 'prepArrayStride').
data PrepArrayPtr :: Symbol -> FSType -> Exp Type
type instance Eval (PrepArrayPtr _name _t) = Ptr Word8

-- | A collection of flat byte arrays, one per prep output variable.
data PrepArrays where
  PrepArrays :: forall prepOutputEnv
              . KnownEnvironment prepOutputEnv
             => EnvironmentProxy prepOutputEnv
             -> Context PrepArrayPtr prepOutputEnv
             -> PrepArrays

data ViewerArgs env = ViewerArgs
  { vaPoint      :: (Double, Double)
  , vaStep       :: (Double, Double)
  , vaWidth      :: Int32
  , vaHeight     :: Int32
  , vaSubsamples :: Int32
  , vaBuffer     :: Ptr Word8
  , vaArgs       :: Context HaskellValue env
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
  , vRefreshOverlay :: PixelDims -> IO ()
    -- ^ Run the viewer's overlay's @on refresh:@ block (if it has an
    --   overlay), for a view with the given pixel size: its drawings go
    --   to a layer below every tool's. A frontend runs it whenever the
    --   view changes, and when 'vListenOverlay' says to, and before it
    --   draws tool drawings anywhere they can't be kept up to date that
    --   way (a capture, a movie frame). Safe to call from any thread, and
    --   never throws: a failing overlay only reports its error on stderr.
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
      -> IO (Either String (PixelDims -> Event -> Maybe (IO ())))
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
renderViewerBuffer downsample Viewer{..} = case vCodeWithArgs of
  CodeWithArgs getArgs _ code -> getArgs >>= \case
    Left err -> pure (Left err)
    Right vaArgs -> do
      Dimensions (w, h) <- getDynamic vSize
      pixelSize <- getDynamic vPixelSize
      aspect <- getDynamic vAspect
      projection <- viewProjection vView vRotation
      center@(cx, cy) <- getDynamic vCenter
      ViewerFunction fn <- getDynamic code
      let s = if downsample then 2 else 1
          bigW = w * s
          px = pixelSize / fromIntegral s
          py = px * aspect
          (x0, y0) = (cx - fromIntegral bigW * px / 2, cy + fromIntegral (h * s) * py / 2)
          -- strips of output rows; enough of them to keep every
          -- capability busy even if some strips are slower than others
          stripRows = max 1 (min 16 (h `div` 64))
          strips = [ (r, min stripRows (h - r)) | r <- [0, stripRows .. h - 1] ]
      caps <- getNumCapabilities
      out <- mallocForeignPtrBytes (3 * w * h)
      issuesVar <- newMVar mempty
      withForeignPtr out $ \outPtr ->
        forPool_ caps strips $ \(row, rows) ->
          allocaBytes (3 * bigW * rows * s) $ \big -> do
            issues <- fn ViewerArgs
              { vaPoint = (x0, y0 - fromIntegral (row * s) * py)
              , vaStep = (px, py)
              , vaWidth = fromIntegral bigW
              , vaHeight = fromIntegral (rows * s)
              , vaSubsamples = 1
              , vaBuffer = big
              , vaArgs }
            modifyMVar_ issuesVar (pure . (<> issues))
            let dest = outPtr `plusPtr` (3 * w * row)
            if downsample then downsampleRows bigW w rows big dest
                          else copyBytes dest big (3 * w * rows)
      issues <- readMVar issuesVar
      pure (Right (RenderedBuffer out w h center (pixelSize, pixelSize * aspect) projection issues))

-- | Average each 2x2 block of a @bigW@-pixel-wide RGB8 buffer down to
-- one pixel of a @w@-pixel-wide one, for @rows@ output rows.
downsampleRows :: Int -> Int -> Int -> Ptr Word8 -> Ptr Word8 -> IO ()
downsampleRows bigW w rows big dest =
  forM_ [0 .. rows - 1] $ \j -> forM_ [0 .. w - 1] $ \i -> forM_ [0, 1, 2] $ \k -> do
    let ix = 3 * (2 * i) + 3 * bigW * (2 * j) + k
        at o = fromIntegral <$> peekElemOff big o :: IO Word16
    c1 <- at ix
    c2 <- at (ix + 3)
    c3 <- at (ix + 3 * bigW)
    c4 <- at (ix + 3 * bigW + 3)
    pokeElemOff dest (3 * i + 3 * w * j + k) (fromIntegral ((c1 + c2 + c3 + c4 + 2) `div` 4))

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

-- | Render a block of pixels, reporting any problems the viewer's script
-- ran into along the way.
newtype ViewerFunction env =
  ViewerFunction (ViewerArgs env -> IO ViewerIssues)

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

invokeViewerFunction :: MissingViewerArgs env
                     => ViewerFunction env
                     -> Context HaskellValue env
                     -> Word32
                     -> Word32
                     -> Word32
                     -> Complex Double
                     -> Complex Double
                     -> Ptr Word8
                     -> IO ViewerIssues
invokeViewerFunction (ViewerFunction fn) vaArgs =
  \w h subsamples (dx :+ dy) (x :+ y) vaBuffer -> do
    let vaPoint = (x, y)
        vaStep  = (dx, dy)
        vaWidth = fromIntegral w
        vaHeight = fromIntegral h
        vaSubsamples = fromIntegral subsamples
    fn ViewerArgs{..}


-- | Compiles a viewer's code, given the project's autocolor lookup (for
-- @autocolor@ and @get_autocolor@; see "Actor.Autocolor").
newtype ViewerCompiler = ViewerCompiler
  { withCompiledViewer :: forall env t
                        . (MissingViewerArgs env, KnownEnvironment env)
                       => AutocolorLookup
                       -> Maybe (PrepScript env)
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
        "Only a tool can allocate a table or write to one: a viewer can only read tables.")

      -- Now bind all of the bookkeeping variables
      let (_, code') = (env', code)
                     & letInEnv (Const (Scalar typeProxy 0))
                     & letInEnv (Const (Scalar typeProxy False))
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 100)) vcIterLimit)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 10.0)) vcEscapes)
                     & letInEnv (fromMaybe (Const (Scalar typeProxy 0.0001)) vcVanishes)

      pure code'

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
    , vRefreshOverlay = vRefreshOverlay v
    , vListenOverlay = vListenOverlay v
    , vCodeWithArgs = vCodeWithArgs v
    , vDrawCmds = vDrawCmds v
    , vDrawCmdsChanged = vDrawCmdsChanged v
    , vBuildToolHandler = vBuildToolHandler v
    }

-- | Refresh a viewer's overlay for its current view (see 'vRefreshOverlay').
refreshOverlay :: Viewer -> IO ()
refreshOverlay v = do
  px <- getDynamic (vPixelSize v)
  aspect <- getDynamic (vAspect v)
  vRefreshOverlay v (px, px * aspect)

-- | A fallback viewer function that paints every point grey
allGrey :: ViewerFunction '[]
allGrey = ViewerFunction $ \ViewerArgs{..} -> do
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
