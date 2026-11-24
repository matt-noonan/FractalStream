{-# language RecursiveDo #-}
module Actor.Event
  ( Event(..)
  , type EventDependencies

  , EventArgument_
  , EventArgument(..)

  , Coordinate(..)
  , PixelDims
  , coordinateNames
  , ViewKind(..)
  , hyperbolicDisplay
  , hyperbolicModel
  , showCoordinate
  , bindOrDeclare
  , RealIn(..)
  , Point
  , planePoint
  , bindFreshReals
  , realPixelVars
  , pixelWidthVar
  , pixelHeightVar
  , buildHandlerWith
  , ToolExec
  , ToolDispatcher
  , HandlerRun(..)
  , runHandlerNow
  , CancelFlag
  , newCancelFlag
  , cancelRun
  , runCancelled
  , withCancelFlagPtr
  , HandlerCancelled(..)
  , ToolRunner(..)
  , defaultToolRunner
  , makeEventHandler

  , constArg
  , mutableVar
  , interpretedToolExec
  , mutableArg
  , toUserString
  , snapshotEventArgs

  , CombinedEventHandler(..)
  , ParsedToolBlock
  , toolBlockTimers
  , toolBlockEvent
  , ParsedToolScript(..)
  , parseToolScript
  , toolBlocksGrammar
  , mergeToolBlocks
  , emptyToolScript

  , assertMissingClickArgs
  , assertMissingDragArgs
  , assertMissingUnitArgs
  , parseUnitScript
  , runParsedBody

  ) where

import FractalStream.Prelude

import Language.Type
import Actor.Autocolor (AutocolorLookup)
import Language.Environment
import Language.Typecheck
import Language.Code
import Language.Parser.SourceRange
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Parser hiding (many)
import Language.Parser.Tokenizer (Token(..), tokenizeWithIndentation)
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform (backendRewrites)
import Language.Code.InterpretIO hiding (update)
import Language.Draw
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import Control.Exception (Exception)
--import Control.Monad.Trans.Maybe

import Data.DynamicValue
import Data.Codec
import Actor.Viewer.Disk (HyperbolicDisplay(..), HyperbolicModel(..))
import Actor.Viewer.Sphere (sphereViewName)
import Language.Value.Typing (surfaceTypeMarker)
--import Data.Typeable

-- | Where an event is: a point in homogeneous coordinates @[a : b]@. In
-- a sphere viewer, any point of the Riemann sphere (∞ is @[1 : 0]@); in
-- any other, @b@ is 1 and @a@ is the point (@x + iy@ for a real viewer's
-- @(x, y)@: see 'planePoint').
type Point = (Complex Double, Complex Double)

-- | A point of a viewer's plane (not a sphere's), as an event's 'Point'.
planePoint :: (Double, Double) -> Point
planePoint (x, y) = (x :+ y, 1)

data Event
  = Click Point
  | DoubleClick Point
  | Drag Point Point -- where the pointer is, then where the drag started
  | DragDone Point Point -- where the drag ended, then where it started
  | Timer String -- timer with given name
  | ButtonPressed String -- button press with given name
  | Refresh
  | Activated
  | Deactivated
  deriving Show

data CombinedEventHandler env = CombinedEventHandler
  { onClick       :: Maybe (Code (ClickHandlerEnv env))
  , onDoubleClick :: Maybe (Code (ClickHandlerEnv env))
  , onDrag     :: Maybe (Code (DragHandlerEnv env))
  , onDragDone :: Maybe (Code (DragHandlerEnv env))
  , onTimer  :: Map String (Int, Code (UnitHandlerEnv env))
  , onButton :: Map String (Code (UnitHandlerEnv env))
  , onRefresh     :: Maybe (Code (UnitHandlerEnv env))
  , onActivated   :: Maybe (Code (UnitHandlerEnv env))
  , onDeactivated :: Maybe (Code (UnitHandlerEnv env))
  }

data EventArgument_ :: Symbol -> FSType -> Exp Type
type instance Eval (EventArgument_ _ t) = EventArgument t

data EventArgument t = EventArgument
  { argGetValue :: IO (Maybe (HaskellType t))
  , argSetValue :: Maybe (TypeProxy t -> HaskellType t -> IO ())
  }

constArg :: HaskellType t -> EventArgument t
constArg x = EventArgument (pure $ Just x) Nothing

mutableVar :: Eq (HaskellType t) => Variable (HaskellType t) -> EventArgument t
mutableVar v = EventArgument
  (Just <$> getDynamic (dyn v))
  (Just $ \_ x -> setValue' v x)

mutableArg :: Eq (HaskellType t) => Mapped String (Either err (HaskellType t)) -> EventArgument t
mutableArg var = EventArgument getter setter
  where
    getter = either (const Nothing) Just <$> getDynamic (dyn var)
    setter = Just $ \t x -> getDynamic (dyn var) >>= \case
      Right oldX | Scalar t oldX == Scalar t x -> pure ()
      _ -> setValue' (source var) (toUserString t x)

data MaybeHaskellValue :: Symbol -> FSType -> Exp Type
type instance Eval (MaybeHaskellValue _ t) = Maybe (HaskellType t)

-- | Run a tool handler in the interpreter: work it out (drawing into a
-- buffer), then give back what applies its effects, its changes to the
-- arguments that it set and its drawing (see 'HandlerRun'). It's stopped
-- by an asynchronous exception rather than its 'CancelFlag'.
run :: forall env. DrawSink -> Context EventArgument_ env -> Code env -> HandlerRun
run sink ctx script = HandlerRun $ \_ -> do

  -- Snapshot the arguments
  (mapContextM @MaybeHaskellValue @HaskellValue (\_ _ -> id) <$> mapContextM (\_ _ -> argGetValue) ctx) >>= \case
    Nothing -> pure (pure ())
    Just (initialArgs :: Context HaskellValue env) -> do
      args :: Context IORefTypeOfBinding env <- mapContextM (\_ _ -> newIORef) initialArgs
      drawn <- newIORef []
      execStateT (interpretToIO (drawHandlerForSink (bufferedSink drawn sink)) script) args
      finalArgs <- mapContextM @_ @HaskellValue (\_ _ -> readIORef) args

      -- Then update any arguments that have been changed by the script,
      -- and draw
      let ctx' = zipContext (zipContext initialArgs finalArgs) ctx
      pure $ do
        fromContextM_ (\_ ty ((old, new), arg) -> case argSetValue arg of
                          Nothing -> pure ()
                          Just setter -> when (Scalar ty old /= Scalar ty new) (setter ty new)
                      ) ctx'
        readIORef drawn >>= sequence_ . reverse

-- | Whether a tool handler's run should stop: a byte that compiled code
-- polls (in its loops), set once the run should stop.
newtype CancelFlag = CancelFlag (ForeignPtr Word8)

newCancelFlag :: IO CancelFlag
newCancelFlag = do
  fp <- mallocForeignPtrBytes 1
  withForeignPtr fp (\p -> poke p 0)
  pure (CancelFlag fp)

-- | Ask the run to stop.
cancelRun :: CancelFlag -> IO ()
cancelRun (CancelFlag fp) = withForeignPtr fp (\p -> poke p 1)

runCancelled :: CancelFlag -> IO Bool
runCancelled (CancelFlag fp) = withForeignPtr fp (fmap (/= 0) . peek)

withCancelFlagPtr :: CancelFlag -> (Ptr Word8 -> IO a) -> IO a
withCancelFlagPtr (CancelFlag fp) = withForeignPtr fp

-- | Thrown by a tool handler's run that stopped because it was asked to
-- (its 'CancelFlag').
data HandlerCancelled = HandlerCancelled
  deriving Show

instance Exception HandlerCancelled

-- | One run of a tool's event handler, in two steps. First it's worked
-- out, on any thread, stopping early once its 'CancelFlag' is set: that
-- gives back the run's effects, its changes to the tool's variables and
-- its drawing, which are applied second, on the UI thread. A run that
-- fails, or is stopped, throws instead, and has no effect at all.
newtype HandlerRun = HandlerRun { startHandlerRun :: CancelFlag -> IO (IO ()) }

-- | Run a handler and apply its effects at once, on this thread.
runHandlerNow :: HandlerRun -> IO ()
runHandlerNow r = newCancelFlag >>= startHandlerRun r >>= id

-- | A tool's event dispatcher: the run for an event, if the tool handles
-- it.
type ToolDispatcher = PixelDims -> Event -> Maybe HandlerRun

-- | Read the current value of every argument in a tool's event context,
-- yielding a concrete 'HaskellValue' context (or 'Nothing' if any argument is
-- currently unavailable, e.g. an unparsed configuration field).
snapshotEventArgs :: Context EventArgument_ env -> IO (Maybe (Context HaskellValue env))
snapshotEventArgs ctx =
  mapContextM @MaybeHaskellValue @HaskellValue (\_ _ -> id) <$> mapContextM (\_ _ -> argGetValue) ctx

-- | How a tool event handler's 'Code' is /prepared/ for execution: given a
-- witness for its environment and the code, produce an invoker that runs the
-- handler against a (per-event) argument context.  Preparation happens once,
-- when the dispatcher is built; the invoker runs on every event.  The
-- interpreter prepares trivially (returns a closure over 'run'); the LLVM
-- backend JIT-compiles the handler during preparation so each event just
-- marshals arguments and calls the compiled code (rather than recompiling).
type ToolExec =
  forall e. EnvironmentProxy e
         -> Code e
         -> IO (Context EventArgument_ e -> HandlerRun)

makeEventHandler :: forall env
                  . (MissingClickArgs env, MissingDragArgs env, MissingUnitArgs env)
                 => ToolExec
                 -> Context EventArgument_ env
                 -> CombinedEventHandler env
                 -> IO ToolDispatcher
makeEventHandler exec ctx CombinedEventHandler{..} = do
  -- Prepare (e.g. JIT-compile) each present handler once.  The dummy contexts
  -- only serve to recover each branch's 'EnvironmentProxy'; 'contextToEnv'
  -- ignores the values.
  let dummyR     = constArg 0 :: EventArgument 'RealT
      dummyC     = constArg 0 :: EventArgument 'ComplexT
      clickEnvP  = contextToEnv (dummyC # dummyC # dummyR # dummyR # ctx)
      dragEnvP   = contextToEnv (dummyC # dummyC # dummyC # dummyC # dummyR # dummyR # ctx)
      unitEnvP   = contextToEnv (dummyR # dummyR # ctx)
  clickRun       <- traverse (exec clickEnvP) onClick
  dblClickRun    <- traverse (exec clickEnvP) onDoubleClick
  dragRun        <- traverse (exec dragEnvP)  onDrag
  dragDoneRun    <- traverse (exec dragEnvP)  onDragDone
  timerRun       <- traverse (\(i, code) -> (i,) <$> exec unitEnvP code) onTimer
  buttonRun      <- traverse (exec unitEnvP)  onButton
  refreshRun     <- traverse (exec unitEnvP)  onRefresh
  activatedRun   <- traverse (exec unitEnvP)  onActivated
  deactivatedRun <- traverse (exec unitEnvP)  onDeactivated
  pure $ \(px, py) -> \case
    Click (a, b) -> clickRun <&> \invoke ->
      invoke (constArg a # constArg b # constArg px # constArg py # ctx)
    DoubleClick (a, b) -> dblClickRun <&> \invoke ->
      invoke (constArg a # constArg b # constArg px # constArg py # ctx)
    Drag (a, b) (a', b') -> dragRun <&> \invoke ->
      invoke (constArg a # constArg b # constArg a' # constArg b' # constArg px # constArg py # ctx)
    DragDone (a, b) (a', b') -> dragDoneRun <&> \invoke ->
      invoke (constArg a # constArg b # constArg a' # constArg b' # constArg px # constArg py # ctx)
    Timer name -> Map.lookup name timerRun <&> \(_, invoke) ->
      invoke (constArg px # constArg py # ctx)
    ButtonPressed name -> Map.lookup name buttonRun <&> \invoke ->
      invoke (constArg px # constArg py # ctx)
    Refresh -> refreshRun <&> \invoke -> invoke (constArg px # constArg py # ctx)
    Activated -> activatedRun <&> \invoke -> invoke (constArg px # constArg py # ctx)
    Deactivated -> deactivatedRun <&> \invoke -> invoke (constArg px # constArg py # ctx)

combineEventHandlers :: forall env
                      . Either (SourceRange, String) (CombinedEventHandler env)
                     -> Either (SourceRange, String) (CombinedEventHandler env)
                     -> Either (SourceRange, String) (CombinedEventHandler env)
combineEventHandlers err@(Left _) _ = err
combineEventHandlers _ err@(Left _) = err
combineEventHandlers (Right lhs) (Right rhs) = do
    -- No specific source location to blame for either kind of error
    -- below: a duplicate block isn't a property of either block on its
    -- own, but of there being two of them, in (possibly) unrelated
    -- places in the source.
    let atMostOne :: String -> (CombinedEventHandler env -> Maybe b) -> Either (SourceRange, String) (Maybe b)
        atMostOne what how = case (how lhs, how rhs) of
          (Just _, Just _) -> Left (NoSourceRange, "Duplicate definitions of the " ++ what ++ " event handler")
          (Nothing, x)     -> Right x
          (x, Nothing)     -> Right x
        uniqueKey :: String -> (CombinedEventHandler env -> Map String v) -> Either (SourceRange, String) (Map String v)
        uniqueKey what how =
          case Set.toList $ Set.intersection (Map.keysSet $ how lhs) (Map.keysSet $ how rhs) of
            [] -> Right (how lhs `Map.union` how rhs)
            (k:_) -> Left (NoSourceRange, "Duplicate definitions of the " ++ what ++ " event handler named `" ++ k ++ "`")
    CombinedEventHandler
      <$> atMostOne "click"          onClick
      <*> atMostOne "double-click"   onDoubleClick
      <*> atMostOne "drag"           onDrag
      <*> atMostOne "drag-completed" onDragDone
      <*> uniqueKey "timer"          onTimer
      <*> uniqueKey "button"         onButton
      <*> atMostOne "refresh"        onRefresh
      <*> atMostOne "activated"      onActivated
      <*> atMostOne "deactivated"    onDeactivated

noHandlers :: CombinedEventHandler env
noHandlers = CombinedEventHandler Nothing Nothing Nothing Nothing Map.empty Map.empty Nothing Nothing Nothing

-- | Parse a tool's event handlers and build its event dispatcher, with handler
-- execution supplied by the given 'ToolExec' (interpreter or JIT-compiled).
buildHandlerWith :: ToolExec
                 -> SomeContext EventArgument_
                 -> ParsedToolScript
                 -> IO (Either String ToolDispatcher)
buildHandlerWith exec (SomeContext ctx) script = do
  let env = contextToEnv ctx
  case runParsedToolScript script env of
    -- This function's own callers only ever show a tool-activation
    -- error as a plain message (there's no editor open at this point
    -- to highlight a span in), so the source location 'checkTool'
    -- wants is dropped right here, at this one boundary, rather than
    -- rippling further up into 'ToolRunner'/'vBuildToolHandler'.
    Left (_, err) -> pure (Left err)
    Right combined0 ->
      assertMissingClickArgs env $
      assertMissingDragArgs env $
      assertMissingUnitArgs env $
      let combined = case (onDrag combined0, onDragDone combined0) of
            (Just _, Nothing) -> combined0 { onDragDone = Just NoOp }
            _ -> combined0
      in Right <$> makeEventHandler exec ctx combined

-- | How a backend turns a tool's parsed event handlers into the event
-- dispatcher the UI calls.  Given the per-layer draw handler (from the tool
-- draw-command accumulator), the layer number, the tool's variable context, and
-- the parsed script, it produces (or fails to produce) a
-- @PixelDims -> Event -> Maybe (IO ())@ dispatcher.  The pure/interpreter backend
-- uses 'defaultToolRunner'; the LLVM backend supplies one that JIT-compiles the
-- handlers.
newtype ToolRunner = ToolRunner
  { runTool :: AutocolorLookup
              -- ^ the project's autocolor tables (see "Actor.Autocolor")
            -> (Int -> DrawSink)
            -> Int
            -> SomeContext EventArgument_
            -> ParsedToolScript
            -> IO (Either String ToolDispatcher) }

-- | The interpreter tool runner: evaluate draw values through the pure
-- interpreter and emit them into the given layer's 'DrawSink'.
defaultToolRunner :: ToolRunner
defaultToolRunner = ToolRunner $ \_ sink layer ctx script ->
  buildHandlerWith (interpretedToolExec (sink layer)) ctx script

-- | Run tool handlers in the interpreter, drawing into the given sink.
interpretedToolExec :: DrawSink -> ToolExec
interpretedToolExec sink _ code =
  let code' = transformValues backendRewrites code
  in pure (\c -> run sink c code')

-- | Adapt a 'DrawSink' into an interpreter 'DrawHandler': evaluate each draw
-- command's value arguments and forward the concrete values to the sink.
drawHandlerForSink :: DrawSink -> DrawHandler ScalarIORefM
drawHandlerForSink sink = DrawHandler $ \case
  DrawPoint _ pv          -> eval pv >>= liftIO . dsPoint sink
  DrawCircle _ fill u rv pv -> do r <- eval rv; p <- eval pv; liftIO (dsCircle sink fill u r p)
  DrawLine _ fv tv        -> do f <- eval fv; t <- eval tv; liftIO (dsLine sink f t)
  DrawRect _ fill fv tv   -> do f <- eval fv; t <- eval tv; liftIO (dsRect sink fill f t)
  SetStroke _ cv          -> eval cv >>= liftIO . dsStroke sink
  SetFill _ cv            -> eval cv >>= liftIO . dsFill sink
  Clear _                 -> liftIO (dsClear sink)
  Write _ tv pv           -> do txt <- eval tv; pt <- eval pv; liftIO (dsWrite sink txt pt)

type EventDependencies =
  (Dynamic (Either String CommonDefs),
   Dynamic (Either String Coordinate),
   Dynamic (Either String (ParsedValue, ParsedValue, ParsedValue)),
   Dynamic (Either String (Maybe String))
  )

declareE :: forall n -> forall t e. KnownSymbol n => TypeProxy t -> EnvironmentProxy e
         -> Either (SourceRange, String) (EnvironmentProxy ( '(n,t) ': e ))
declareE n t e = case lookupEnv' (Proxy @n) e of
  Found'{} -> throwError (NoSourceRange, "Internal error: duplicate definition of `" ++
                           symbolVal (Proxy @n) ++ "`.")
  Absent' pf -> pure (recallIsAbsent pf $ declare t e)

getVar :: forall n -> forall t e.  KnownSymbol n
       => TypeProxy t -> EnvironmentProxy e -> Either (SourceRange, String) (Value '(e, t))
getVar n t e = withEnvironment e $ case lookupEnv (Proxy @n) t e of
  Found pf -> pure (Var (Proxy @n) t pf)
  _ -> throwError (NoSourceRange,
                    "INTERNAL ERROR, there was a problem locating `" ++ symbolVal (Proxy @n) ++ "`")

------------------------------------------------------------
-- Click-type event handler
------------------------------------------------------------

type InternalPx  = "[internal] px"
type InternalPy  = "[internal] py"

-- | The width and height of one pixel, in model coordinates, as a tool's
-- event dispatcher is given them. A complex viewer's pixels are square.
type PixelDims = (Double, Double)

-- | An event's point, @[a : b]@ (see 'Point'), as its handler is given it.
type InternalPointA = "[internal] point a"
type InternalPointB = "[internal] point b"

type ClickHandlerEnv env =
  ( '(InternalPointA, 'ComplexT) ': '(InternalPointB, 'ComplexT) ':
    '(InternalPx, 'RealT) ': '(InternalPy, 'RealT) ': env )

type MissingClickArgs env =
  ( NotPresent InternalPointA env, NotPresent InternalPointB env
  , NotPresent InternalPx env, NotPresent InternalPy env )

assertMissingClickArgs :: forall env a
                       . EnvironmentProxy env
                      -> (MissingClickArgs env => a)
                      -> a
assertMissingClickArgs env k =
  assertAbsent (Proxy @InternalPointA) ComplexType env $
  assertAbsent (Proxy @InternalPointB) ComplexType env $
  assertAbsent (Proxy @InternalPx)   RealType env $
  assertAbsent (Proxy @InternalPy)   RealType env k

type Bookkeeping env =
    ( '(InternalIterations, 'IntegerT) ': '(InternalStuck, 'BooleanT) ':
      '(InternalIterationLimit, 'IntegerT) ':
      '(InternalEscapeRadius, 'RealT) ': '(InternalVanishingRadius, 'RealT) ':
      env)

withBookkeeping :: forall env
                 . EnvironmentProxy env
                -> CommonDefs
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> (KnownEnvironment (Bookkeeping env) =>
                    EnvironmentProxy (Bookkeeping env) ->
                    CommonDefs ->
                    Either (SourceRange, String) (Code (Bookkeeping env)))
                -> Either (SourceRange, String) (Code env)
withBookkeeping env defs (pMaxIters, pMaxRadius, pMinRadius) action = withEnvironment env $ do
  env' :: EnvironmentProxy (Bookkeeping env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
      ) env
  withEnvironment env' $ do
    code <- action env' defs
    -- Now bind all of the bookkeeping variables
    let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), code0) =
          (env', code) & letInEnv (Const (Scalar typeProxy 0))
                       & letInEnv (Const (Scalar typeProxy False))
    code1 <- snd . (`letInEnv` (env0, code0)) <$> pvAtType pMaxIters  IntegerType env1
    code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
    code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3
    pure code'

-- | Typecheck an already-'Earley'-parsed script body in a concrete
-- environment. This is the tail end of
-- 'parseCode', minus the tokenize-and-parse step, for use when the body's
-- 'ParsedCode' was already recovered as part of a larger single parse (e.g.
-- a whole multi-block tool script) rather than from its own separate
-- source string.
runParsedBody :: EnvironmentProxy env -> ParsedCode
              -> Either (Either ParseError TCError) (Code env)
runParsedBody env body = first Right (runParsedCode env body)

-- | Typecheck a handler's body, reporting errors the way tools do.
handlerBody :: ParsedCode -> EnvironmentProxy e -> Either (SourceRange, String) (Code e)
handlerBody body e = left (errorLocation &&& unlines . pp) (runParsedBody e body)

-- | An internal real variable, as a 'RealIn'.
internalReal :: forall n -> KnownSymbol n => RealIn
internalReal n = RealIn (getVar n RealType)

-- | A complex value, read in whatever environment it ends up used in.
newtype ComplexIn = ComplexIn
  (forall e. EnvironmentProxy e -> Either (SourceRange, String) (Value '(e, 'ComplexT)))

-- | An internal complex variable, as a 'ComplexIn'.
internalComplex :: forall n -> KnownSymbol n => ComplexIn
internalComplex n = ComplexIn (getVar n ComplexType)

-- | The pixel-size variables a tool's handlers see, which depend on the
-- kind of viewer the tool is in (not on how a handler names its event's
-- position): the pixel size (@pixelVar=@, if any), and a real viewer's
-- pixel width and height.
toolPixelVars :: Coordinate -> Maybe String -> [(String, RealIn)]
toolPixelVars viewerCoord mpx =
  [ (p, internalReal InternalPx) | p <- maybeToList mpx ] ++ case viewerCoord of
    ComplexCoordinate _ -> []
    RealCoordinates _ _ -> [ (pixelWidthVar,  internalReal InternalPx)
                           , (pixelHeightVar, internalReal InternalPy) ]

-- | Bind an event position @[a : b]@ (see 'Point'), read from the given
-- internal variables, to the coordinate a handler names it by, around
-- some code: in a sphere viewer, the point of Ĉ itself; in any other
-- (where @b@ is 1), @z = a@, or @(x, y)@ for a pair of real variables. A
-- coordinate that's already defined (a configuration value) is set
-- instead.
bindEventCoordinate :: forall env
                     . Coordinate
                    -> ComplexIn
                    -> ComplexIn
                    -> EnvironmentProxy env
                    -> (forall e. KnownEnvironment e => EnvironmentProxy e -> Either (SourceRange, String) (Code e))
                    -> Either (SourceRange, String) (Code env)
bindEventCoordinate coord (ComplexIn getA) (ComplexIn getB) env k = withEnvironment env $ case coord of
  -- (in a sphere viewer's tool, a point of Ĉ: see "Language.Code.Projective")
  ComplexCoordinate c | sphereViewName `Map.member` envToMap env -> case someSymbolVal c of
    SomeSymbol cp -> do
      a <- getA env
      b <- getB env
      let pairT = PairType ComplexType ComplexType
          point = PairV pairT a b
      bindOrDeclare cp pairT point env $ \e -> withEnvironment e $
        case someSymbolVal (surfaceTypeMarker c ExtComplexT) of
          SomeSymbol mp -> case lookupEnv mp BooleanType e of
            Absent pf -> recallIsAbsent pf $ do
              let e' = BindingProxy mp BooleanType e
              code <- withEnvironment e' (k e')
              pure (snd (letInEnv (Const (Scalar BooleanType True)) (e', code)))
            _ -> k e
  ComplexCoordinate c -> case someSymbolVal c of
    SomeSymbol cp -> do
      a <- getA env
      bindOrDeclare cp ComplexType a env k
  RealCoordinates p q -> case (someSymbolVal p, someSymbolVal q) of
    (SomeSymbol xp, SomeSymbol yp) -> do
      a <- getA env
      bindOrDeclare xp RealType (ReC a) env $ \env' -> do
        a' <- getA env'
        bindOrDeclare yp RealType (ImC a') env' k

-- | Bind an event's point @[a : b]@ (see 'Point') to the internal name a
-- handler reads it by (@click@, @drag start@, @drag end@: see
-- 'Language.Typecheck.clickPointVar'), as the kind of viewer has its
-- points: a pair of real numbers in a real viewer; a point of Ĉ in a
-- sphere viewer; else a complex number.
bindEventPoint :: forall env
                . String
               -> Coordinate
               -- ^ the viewer's coordinate (only its kind matters)
               -> ComplexIn
               -> ComplexIn
               -> EnvironmentProxy env
               -> (forall e. KnownEnvironment e => EnvironmentProxy e -> Either (SourceRange, String) (Code e))
               -> Either (SourceRange, String) (Code env)
bindEventPoint name viewerCoord getA@(ComplexIn readA) getB env k = case viewerCoord of
  ComplexCoordinate _ -> bindEventCoordinate (ComplexCoordinate name) getA getB env k
  RealCoordinates _ _ -> withEnvironment env $ case someSymbolVal name of
    SomeSymbol np -> do
      a <- readA env
      let pairT = PairType RealType RealType
      bindOrDeclare np pairT (PairV pairT (ReC a) (ImC a)) env k

parseClickScript :: CommonDefs
                 -> Coordinate
                 -- ^ the viewer's coordinate (only its kind matters)
                 -> String
                 -- ^ the name the click's point is bound to
                 -> (ParsedValue, ParsedValue, ParsedValue)
                 -> Maybe String
                 -> ParsedCode
                 -> (forall env. EnvironmentProxy env
                     -> Either (SourceRange, String) (Code (ClickHandlerEnv env)))
parseClickScript defs0 viewerCoord clickName limits mpixel body
  (env0 :: EnvironmentProxy env) = do

  env1 <- (declareE InternalPointA ComplexType
       <=< declareE InternalPointB ComplexType
       <=< declareE InternalPx RealType
       <=< declareE InternalPy RealType) env0

  withBookkeeping env1 defs0 limits $ \env _defs ->
    bindEventPoint clickName viewerCoord (internalComplex InternalPointA) (internalComplex InternalPointB) env $ \env' ->
      bindFreshReals (toolPixelVars viewerCoord mpixel) env' (handlerBody body)

------------------------------------------------------------
-- Drag-type event handler
------------------------------------------------------------

-- | Where a drag started, @[a : b]@ (see 'Point').
type InternalOldA  = "[internal] old point a"
type InternalOldB  = "[internal] old point b"

type MissingDragArgs env =
  ( NotPresent InternalPointA env, NotPresent InternalPointB env
  , NotPresent InternalOldA env, NotPresent InternalOldB env
  , NotPresent InternalPx env, NotPresent InternalPy env )

assertMissingDragArgs :: forall env a
                       . EnvironmentProxy env
                      -> (MissingDragArgs env => a)
                      -> a
assertMissingDragArgs env k =
  assertAbsent (Proxy @InternalPointA) ComplexType env $
  assertAbsent (Proxy @InternalPointB) ComplexType env $
  assertAbsent (Proxy @InternalOldA) ComplexType env $
  assertAbsent (Proxy @InternalOldB) ComplexType env $
  assertAbsent (Proxy @InternalPx)   RealType env $
  assertAbsent (Proxy @InternalPy)   RealType env k

type DragHandlerEnv env =
  ( '(InternalPointA, 'ComplexT) ': '(InternalPointB, 'ComplexT) ':
    '(InternalOldA, 'ComplexT) ': '(InternalOldB, 'ComplexT) ':
    '(InternalPx, 'RealT) ': '(InternalPy, 'RealT) ': env )

type InternalDragHandlerEnv env =
  ( '(InternalIterations, 'IntegerT) ':
    '(InternalStuck, 'BooleanT) ':
    '(InternalIterationLimit, 'IntegerT) ':
    '(InternalEscapeRadius, 'RealT) ':
    '(InternalVanishingRadius, 'RealT) ':
    DragHandlerEnv env)

bindOrDeclare :: forall name ty env
               . KnownSymbol name
              => Proxy name
              -> TypeProxy ty
              -> Value '(env, ty)
              -> EnvironmentProxy env
              -> (forall e. KnownEnvironment e => EnvironmentProxy e -> Either (SourceRange, String) (Code e))
              -> Either (SourceRange, String) (Code env)
bindOrDeclare name ty v env action = case lookupEnv name ty env of
  WrongType ty' -> withKnownType ty $
    throwError (NoSourceRange,
                 "The variable `" ++ symbolVal name ++ "` should be " ++ an ty ++ ", not " ++ an ty' ++ ".")

  Found pf -> withEnvironment env $ do
    code <- action env
    pure $ Block [ Set pf name v, code ]

  Absent pf -> withKnownType ty $ do
    let env' = recallIsAbsent pf $ BindingProxy name ty env
    code <- withEnvironment env' $ action env'
    pure (snd $ letInEnv @name v (env', code))

-- | A real value, read in whatever environment it ends up used in.
newtype RealIn = RealIn
  (forall e. EnvironmentProxy e -> Either (SourceRange, String) (Value '(e, 'RealT)))

-- | Bind each name, which mustn't be defined already, to its value
-- around some code (the pixel size, and a real viewer's pixel width and
-- height).
bindFreshReals :: forall env
                . [(String, RealIn)]
               -> EnvironmentProxy env
               -> (forall e. KnownEnvironment e => EnvironmentProxy e -> Either (SourceRange, String) (Code e))
               -> Either (SourceRange, String) (Code env)
bindFreshReals [] env k = withEnvironment env (k env)
bindFreshReals ((name, RealIn getV) : rest) env k = case someSymbolVal name of
  SomeSymbol (np :: Proxy n) -> case lookupEnv np RealType env of
    Absent pf -> recallIsAbsent pf $ withEnvironment env $ do
      v <- getV env
      let env' = BindingProxy np RealType env
      code <- bindFreshReals rest env' k
      pure (snd (letInEnv @n v (env', code)))
    _ -> Left (NoSourceRange, "The pixel size variable `" ++ name ++ "` was redefined.")

parseDragScript :: CommonDefs
                -> Coordinate
                -- ^ the viewer's coordinate (only its kind matters)
                -> String
                -- ^ the name the drag's current position is bound to
                -> String
                -- ^ the name the drag's starting position is bound to
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Maybe String
                -> ParsedCode
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (DragHandlerEnv env)))
parseDragScript _defs viewerCoord curName oldName (pMaxIters, pMaxRadius, pMinRadius) mpixel body
  (env :: EnvironmentProxy env) = do
  withEnvironment env $ do
    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalDragHandlerEnv env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalPointA          ComplexType
        <=< declareE InternalPointB          ComplexType
        <=< declareE InternalOldA            ComplexType
        <=< declareE InternalOldB            ComplexType
        <=< declareE InternalPx              RealType
        <=< declareE InternalPy              RealType
      ) env

    withEnvironment env' $ do

      code :: Code (InternalDragHandlerEnv env) <-
        bindEventPoint curName viewerCoord (internalComplex InternalPointA) (internalComplex InternalPointB) env' $ \env1 ->
          bindEventPoint oldName viewerCoord (internalComplex InternalOldA) (internalComplex InternalOldB) env1 $ \env2 ->
            bindFreshReals (toolPixelVars viewerCoord mpixel) env2 (handlerBody body)

      -- Now bind all of the bookkeeping variables
      let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), code0) =
            (env', code) & letInEnv (Const (Scalar typeProxy 0))
                         & letInEnv (Const (Scalar typeProxy False))
      code1 <- snd . (`letInEnv` (env0, code0)) <$> pvAtType pMaxIters  IntegerType env1
      code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
      code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3
      pure code'

------------------------------------------------------------
-- Nullary event handlers
------------------------------------------------------------

type UnitHandlerEnv env = '(InternalPx, 'RealT) ': '(InternalPy, 'RealT) ': env

type InternalUnitHandlerEnv env =
  ( '(InternalIterations, 'IntegerT) ':
    '(InternalStuck, 'BooleanT) ':
    '(InternalIterationLimit, 'IntegerT) ':
    '(InternalEscapeRadius, 'RealT) ':
    '(InternalVanishingRadius, 'RealT) ':
    UnitHandlerEnv env)

type MissingUnitArgs env = ( NotPresent InternalPx env, NotPresent InternalPy env )

assertAbsent :: KnownSymbol name
             => Proxy name
             -> TypeProxy t
             -> EnvironmentProxy env
             -> (NotPresent name env => k)
             -> k
assertAbsent name ty env k =
  case assertAbsentInEnv name ty env "" of
    Nothing -> error ("Internal error in `assertAbsent` on " ++ symbolVal name)
    Just ProofNameIsAbsent -> k

assertMissingUnitArgs :: forall env a
                       . EnvironmentProxy env
                      -> (MissingUnitArgs env => a)
                      -> a
assertMissingUnitArgs env k =
  assertAbsent (Proxy @InternalPx) RealType env $
  assertAbsent (Proxy @InternalPy) RealType env k

parseUnitScript :: CommonDefs
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Coordinate
                -- ^ the viewer's coordinate
                -> Maybe String
                -> ParsedCode
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (UnitHandlerEnv env)))
parseUnitScript _defs (pMaxIters, pMaxRadius, pMinRadius) viewerCoord mpx body
  (env :: EnvironmentProxy env) = do

  withEnvironment env $ do
    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalUnitHandlerEnv env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalPx              RealType
        <=< declareE InternalPy              RealType
      ) env

    withEnvironment env' $ do
      code <- bindFreshReals (toolPixelVars viewerCoord mpx) env' (handlerBody body)

      let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), code0) =
            (env', code) & letInEnv (Const (Scalar typeProxy 0))
                         & letInEnv (Const (Scalar typeProxy False))

      code1 <- snd . (`letInEnv` (env0, code0)) <$> pvAtType pMaxIters  IntegerType env1
      code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
      code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3

      pure code'


------------------------------------------------------------
-- Tool-script grammar
--
-- Parses a tool's whole `script:` field -- a sequence of `on <event>:`
-- blocks -- in a single tokenization + Earley parse over the entire input,
-- reusing `codeBlockGrammar` for each block's body the same way
-- `codeGrammar` itself reuses `valueGrammar`. This avoids re-tokenizing
-- substrings (which would need manual source-range offset correction) and
-- keeps indentation handling in the one place that already owns it.
------------------------------------------------------------

-- | One block's contribution to a tool's combined event handler. Deferred
-- not just over the final environment (the tool's variable context), which
-- the block's 'ParsedCode' is typechecked against, but also over the
-- default coordinate\/iteration-limits\/pixel-size-variable that a header
-- omitting @at ...@\/@from ... to ...@ falls back on -- so that
-- 'toolBlocksGrammar' can be built /once/, independent of which viewer a
-- @\@tool@ stanza in a unified project script ends up targeting (that's
-- only known once every @\@viewer@ stanza's header has been parsed, which
-- may happen after the tool's own stanza in the source). The single-tool
-- entry point below ('parseToolScript') simply applies these immediately,
-- since it already has them in hand before parsing starts.
data ParsedToolBlock = ParsedToolBlock
  String
  -- ^ The event this block handles, as its header names it (@on refresh@).
  [(String, Int)]
  -- ^ The timers this block defines, with their intervals in milliseconds.
  (Coordinate -> (ParsedValue, ParsedValue, ParsedValue) -> Maybe String
   -> forall env. EnvironmentProxy env -> Either (SourceRange, String) (CombinedEventHandler env))

-- | The timers a tool's blocks define, by name, with their intervals in
-- milliseconds. A frontend runs them while the tool is selected.
toolBlockTimers :: [ParsedToolBlock] -> [(String, Int)]
toolBlockTimers blocks = concat [ ts | ParsedToolBlock _ ts _ <- blocks ]

-- | The event a tool block handles, as its header names it (@on refresh@).
toolBlockEvent :: ParsedToolBlock -> String
toolBlockEvent (ParsedToolBlock ev _ _) = ev

-- | The result of parsing an entire tool script: all of its blocks already
-- merged and checked for duplicate event definitions.
newtype ParsedToolScript = ParsedToolScript
  { runParsedToolScript :: forall env. EnvironmentProxy env -> Either (SourceRange, String) (CombinedEventHandler env) }

mergeToolBlocks :: [ParsedToolBlock]
                -> Coordinate
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Maybe String
                -> ParsedToolScript
mergeToolBlocks blocks0 defaultCoord limits mpixel = ParsedToolScript $ \env ->
  foldl' combineEventHandlers (Right noHandlers)
    [ f defaultCoord limits mpixel env | ParsedToolBlock _ _ f <- blocks ]
  where
    -- (`on click:` handles drags only if there's no `on drag:`)
    hasDrag = any ((== "on drag") . toolBlockEvent) blocks0
    blocks = [ b | b <- blocks0, not (hasDrag && toolBlockEvent b == clickWhileDragging) ]

-- | The event of the block an @on click:@ makes to handle drags too, used
-- only when the tool has no @on drag:@ block.
clickWhileDragging :: String
clickWhileDragging = "on click (while dragging)"

-- | A tool script with no event handlers at all (e.g. a tool whose
-- @script:@ field is absent or empty).
emptyToolScript :: ParsedToolScript
emptyToolScript = ParsedToolScript $ const (Right noHandlers)

-- | Parse a tool's whole @script:@ text into its combined event handler.
parseToolScript :: CommonDefs
                 -> Coordinate                              -- ^ default coordinate (the viewer's z-coord)
                 -> (ParsedValue, ParsedValue, ParsedValue)  -- ^ iteration\/escape\/vanishing limits
                 -> Maybe String                             -- ^ pixel-size variable name, if any
                 -> String                                   -- ^ the whole script source
                 -> Either (SourceRange, String) ParsedToolScript
parseToolScript defs defaultCoord limits mpixel input =
  bimap (errorLocation &&& unlines . pp) (\blocks -> mergeToolBlocks blocks defaultCoord limits mpixel)
    (parse (toolScriptGrammar defs) (tokenizeWithIndentation input))

-- | The grammar for a whole tool script: a sequence of @on <event>:@ blocks,
-- surrounded by the @Indent@\/@Dedent@ pair that delimits the tool's own
-- @script:@ field. Thin wrapper around 'toolBlocksGrammar', the same way
-- 'Language.Code.Parser.codeBlockGrammar' wraps 'codeStatementsGrammar'.
toolScriptGrammar :: forall r
                   . CommonDefs
                  -> Grammar r (Prod r [ParsedToolBlock])
toolScriptGrammar defs = do
  blocks <- toolBlocksGrammar defs
  rule (token Indent *> blocks <* token Dedent)

-- | The grammar for a bare sequence of @on <event>:@ blocks with no
-- surrounding @Indent@\/@Dedent@ tokens. Exported so that a grammar whose
-- tool-script bodies aren't indentation-delimited (e.g. a future unified
-- project-script grammar, where an @\@tool@ stanza's @on ...:@ blocks sit
-- flush with the stanza's own header rather than nested beneath it) can
-- reuse the full set of event-block headers without re-deriving them.
toolBlocksGrammar :: forall r
                   . CommonDefs
                  -> Grammar r (Prod r [ParsedToolBlock])
toolBlocksGrammar defs = mdo

  -- Each handler's body, with the project's @common statements prepended.
  codeBlock <- rule . fmap (withCommon defs) =<< codeBlockGrammar defs

  colon <- rule (token Colon <?> ":")
  nl    <- rule (token Newline <?> "end of line")

  quoted <- rule (tokenMatch (\case { Quoted s -> Just s; _ -> Nothing }))
  intLit <- rule (tokenMatch (\case { NumberI n -> Just (fromIntegral n); _ -> Nothing }))

  onRefresh' <- rule $
    (\body -> ParsedToolBlock "on refresh" [] $ \defaultCoord limits mpixel env ->
        second (\u -> noHandlers { onRefresh = Just u })
          (parseUnitScript defs limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "refresh" *> colon *> nl *> codeBlock)

  onActivated' <- rule $
    (\body -> ParsedToolBlock "on activated" [] $ \defaultCoord limits mpixel env ->
        second (\u -> noHandlers { onActivated = Just u })
          (parseUnitScript defs limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "activated" *> colon *> nl *> codeBlock)

  onDeactivated' <- rule $
    (\body -> ParsedToolBlock "on deactivated" [] $ \defaultCoord limits mpixel env ->
        second (\u -> noHandlers { onDeactivated = Just u })
          (parseUnitScript defs limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "deactivated" *> colon *> nl *> codeBlock)

  onButton' <- rule $
    (\label body -> ParsedToolBlock "on button" [] $ \defaultCoord limits mpixel env ->
        second (\u -> noHandlers { onButton = Map.singleton label u })
          (parseUnitScript defs limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "button" *> quoted <* colon <* nl)
    <*> codeBlock

  onTimer' <- rule $
    (\name interval body -> ParsedToolBlock "on timer" [(name, interval)] $ \defaultCoord limits mpixel env ->
        second (\u -> noHandlers { onTimer = Map.singleton name (interval, u) })
          (parseUnitScript defs limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "timer" *> quoted)
    <*> (tok "every" *> intLit <* tok "ms" <* colon <* nl)
    <*> codeBlock

  -- A click's point is `click`. With no `on drag:`, the tool's
  -- `on click:` also handles drags, with `click` where the pointer is
  -- now (see 'clickWhileDragging' and 'mergeToolBlocks').
  onClick' <- rule $
    (\body ->
      [ ParsedToolBlock "on click" [] $ \viewerCoord limits mpixel env ->
          second (\c -> noHandlers { onClick = Just c })
            (parseClickScript defs viewerCoord clickPointVar limits mpixel body env)
      , ParsedToolBlock clickWhileDragging [] $ \viewerCoord limits mpixel env ->
          second (\d -> noHandlers { onDrag = Just d })
            (parseDragScript defs viewerCoord clickPointVar "[unused] drag start" limits mpixel body env) ])
    <$> (tok "on" *> tok "click" *> colon *> nl *> codeBlock)

  onDoubleClick' <- rule $
    (\body -> [ ParsedToolBlock "on double-click" [] $ \viewerCoord limits mpixel env ->
        second (\c -> noHandlers { onDoubleClick = Just c })
          (parseClickScript defs viewerCoord clickPointVar limits mpixel body env) ])
    <$> (tok "on" *> tok "double" *> token Minus *> tok "click" *> colon *> nl *> codeBlock)

  -- A drag's points are `drag start` and `drag end` (where the pointer
  -- is now, during the drag).
  onDrag' <- rule $
    (\body -> [ ParsedToolBlock "on drag" [] $ \viewerCoord limits mpixel env ->
        second (\d -> noHandlers { onDrag = Just d })
          (parseDragScript defs viewerCoord dragEndVar dragStartVar limits mpixel body env) ])
    <$> (tok "on" *> tok "drag" *> colon *> nl *> codeBlock)

  onDragFinished' <- rule $
    (\body -> [ ParsedToolBlock "on drag-finished" [] $ \viewerCoord limits mpixel env ->
        second (\d -> noHandlers { onDragDone = Just d })
          (parseDragScript defs viewerCoord dragEndVar dragStartVar limits mpixel body env) ])
    <$> (tok "on" *> tok "drag" *> token Minus *> tok "finished" *> colon *> nl *> codeBlock)

  toolBlock <- ruleChoice
    [ onClick', onDoubleClick', onDragFinished', onDrag'
    , (:[]) <$> onRefresh', (:[]) <$> onActivated', (:[]) <$> onDeactivated', (:[]) <$> onButton', (:[]) <$> onTimer'
    ]

  rule (concat <$> many toolBlock)

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | How a point of a viewer (or a tool event's position) is named: one
-- complex variable, or a pair of real ones (@x@, then @y@).
data Coordinate = ComplexCoordinate String | RealCoordinates String String
  deriving (Eq, Show)

-- | How a viewer shows its points: on the plane, or (for a complex
-- coordinate) on the Riemann sphere, as a globe, or the hyperbolic plane,
-- in the Poincaré disk or the upper half-plane (see "Actor.Viewer.Disk").
data ViewKind = PlaneView | SphereView | DiskView | HalfPlaneView
  deriving (Eq, Show)

-- | How a view draws the hyperbolic plane, if it does.
hyperbolicDisplay :: ViewKind -> Maybe HyperbolicDisplay
hyperbolicDisplay = \case
  DiskView -> Just DiskDisplay
  HalfPlaneView -> Just HalfPlaneDisplay
  _ -> Nothing

-- | Where a script's points are, by the view its script names: the
-- half-plane for @view=halfplane@, else the disk.
hyperbolicModel :: ViewKind -> HyperbolicModel
hyperbolicModel = \case
  HalfPlaneView -> HalfPlaneModel
  _ -> DiskModel

-- | The names a 'Coordinate' binds.
coordinateNames :: Coordinate -> [String]
coordinateNames = \case
  ComplexCoordinate z -> [z]
  RealCoordinates x y -> [x, y]

-- | A coordinate as a script writes it: @z@, or @(x, y)@.
showCoordinate :: Coordinate -> String
showCoordinate = \case
  ComplexCoordinate z -> z
  RealCoordinates x y -> "(" ++ x ++ ", " ++ y ++ ")"

-- | The pixel dimensions a real viewer's code (and its tools') can read,
-- besides the pixel size (@pixel_size@, or @pixelVar=@): the width
-- (the same as the pixel size) and height of one pixel. Their pixels
-- needn't be square, unlike a complex viewer's.
realPixelVars :: Coordinate -> [String]
realPixelVars = \case
  ComplexCoordinate _ -> []
  RealCoordinates _ _ -> [pixelWidthVar, pixelHeightVar]

pixelWidthVar, pixelHeightVar :: String
pixelWidthVar  = "pixel_width"
pixelHeightVar = "pixel_height"

newtype MCoordinate = MC (Maybe Coordinate)

instance Codec MCoordinate where
  codec = match
    [ Fragment (MC . Just . ComplexCoordinate)
               (\case { MC (Just (ComplexCoordinate z)) -> Just z; _ -> Nothing }) $
      (key "coord")
    , Fragment (MC . Just . uncurry RealCoordinates)
               (\case { MC (Just (RealCoordinates x y)) -> Just (x, y); _ -> Nothing }) $ do
        x <-fst-< key "x-coord"
        y <-snd-< key "y-coord"
        build (,) x y
    , Fragment (\_ -> MC Nothing) (\case { MC Nothing -> Just (); _ -> Nothing }) (build ())
    ]

newtype SCoordinate = SC (Maybe Coordinate)

instance Codec SCoordinate where
  codec = match
    [ Fragment (SC . Just . ComplexCoordinate)
               (\case { SC (Just (ComplexCoordinate z)) -> Just z; _ -> Nothing }) $
      (key "start")
    , Fragment (SC . Just . uncurry RealCoordinates)
               (\case { SC (Just (RealCoordinates x y)) -> Just (x, y); _ -> Nothing }) $ do
        x <-fst-< key "x-start"
        y <-snd-< key "y-start"
        build (,) x y
    , Fragment (\_ -> SC Nothing) (\case { SC Nothing -> Just (); _ -> Nothing }) (build ())
    ]

pvAtType :: forall env ty. ParsedValue -> TypeProxy ty -> EnvironmentProxy env
         -> Either (SourceRange, String) (Value '(env, ty))
pvAtType pv ty env = withEnvironment env $ withKnownType ty $ case atType pv ty of
  TC (Left err) -> Left (NoSourceRange, ppError err)
  TC (Right v)  -> pure v
