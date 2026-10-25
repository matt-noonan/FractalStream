{-# language RecursiveDo #-}
module Actor.Event
  ( Event(..)
  , type EventDependencies

  , EventArgument_
  , EventArgument(..)

  , Coordinate(..)
  , buildHandlerWith
  , ToolExec
  , ToolRunner(..)
  , defaultToolRunner
  , makeEventHandler

  , constArg
  , mutableVar
  , mutableArg
  , toUserString
  , snapshotEventArgs

  , CombinedEventHandler
  , ParsedToolBlock
  , ParsedToolScript(..)
  , parseToolScript
  , toolBlocksGrammar
  , mergeToolBlocks
  , emptyToolScript

  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Typecheck
import Language.Code
import Language.Parser.SourceRange
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Parser hiding (many)
import Language.Parser.Tokenizer (Token(..), tokenizeWithIndentation)
import Language.Value.Evaluator (HaskellValue)
import Language.Code.InterpretIO hiding (update)
import Language.Draw
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef
--import Control.Monad.Trans.Maybe

import Data.DynamicValue
import Data.Codec
import Actor.Viewer.Types
--import Data.Typeable

type Point = (Double, Double)

data Event
  = Click Point
  | DoubleClick Point
  | Drag Point Point -- drag from / to
  | DragDone Point Point -- dragged from / to
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

run :: forall env. DrawHandler ScalarIORefM -> Context EventArgument_ env -> Code env -> IO ()
run draw ctx script = do

  -- Snapshot the arguments
  (mapContextM @MaybeHaskellValue @HaskellValue (\_ _ -> id) <$> mapContextM (\_ _ -> argGetValue) ctx) >>= \case
    Nothing -> pure ()
    Just (initialArgs :: Context HaskellValue env) -> do
      args :: Context IORefTypeOfBinding env <- mapContextM (\_ _ -> newIORef) initialArgs
      execStateT (interpretToIO draw script) args
      finalArgs <- mapContextM @_ @HaskellValue (\_ _ -> readIORef) args

      -- Update any arguments that have been changed by the script
      let ctx' = zipContext (zipContext initialArgs finalArgs) ctx
      fromContextM_ (\_ ty ((old, new), arg) -> case argSetValue arg of
                        Nothing -> pure ()
                        Just setter -> when (Scalar ty old /= Scalar ty new) (setter ty new)
                    ) ctx'

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
         -> IO (Context EventArgument_ e -> IO ())

makeEventHandler :: forall env
                  . (MissingClickArgs env, MissingDragArgs env, MissingUnitArgs env)
                 => ToolExec
                 -> Context EventArgument_ env
                 -> CombinedEventHandler env
                 -> IO (Double -> Event -> Maybe (IO ()))
makeEventHandler exec ctx CombinedEventHandler{..} = do
  -- Prepare (e.g. JIT-compile) each present handler once.  The dummy contexts
  -- only serve to recover each branch's 'EnvironmentProxy'; 'contextToEnv'
  -- ignores the values.
  let dummyR     = constArg 0 :: EventArgument 'RealT
      clickEnvP  = contextToEnv (dummyR # dummyR # dummyR # ctx)
      dragEnvP   = contextToEnv (dummyR # dummyR # dummyR # dummyR # dummyR # ctx)
      unitEnvP   = contextToEnv (dummyR # ctx)
  clickRun       <- traverse (exec clickEnvP) onClick
  dblClickRun    <- traverse (exec clickEnvP) onDoubleClick
  dragRun        <- traverse (exec dragEnvP)  onDrag
  dragDoneRun    <- traverse (exec dragEnvP)  onDragDone
  timerRun       <- traverse (\(i, code) -> (i,) <$> exec unitEnvP code) onTimer
  buttonRun      <- traverse (exec unitEnvP)  onButton
  refreshRun     <- traverse (exec unitEnvP)  onRefresh
  activatedRun   <- traverse (exec unitEnvP)  onActivated
  deactivatedRun <- traverse (exec unitEnvP)  onDeactivated
  pure $ \px -> \case
    Click (x, y) -> clickRun <&> \invoke ->
      invoke (constArg x # constArg y # constArg px # ctx)
    DoubleClick (x, y) -> dblClickRun <&> \invoke ->
      invoke (constArg x # constArg y # constArg px # ctx)
    Drag (x, y) (x', y') -> dragRun <&> \invoke ->
      invoke (constArg x # constArg y # constArg x' # constArg y' # constArg px # ctx)
    DragDone (x, y) (x', y') -> dragDoneRun <&> \invoke ->
      invoke (constArg x # constArg y # constArg x' # constArg y' # constArg px # ctx)
    Timer name -> Map.lookup name timerRun <&> \(_, invoke) ->
      invoke (constArg px # ctx)
    ButtonPressed name -> Map.lookup name buttonRun <&> \invoke ->
      invoke (constArg px # ctx)
    Refresh -> refreshRun <&> \invoke -> invoke (constArg px # ctx)
    Activated -> activatedRun <&> \invoke -> invoke (constArg px # ctx)
    Deactivated -> deactivatedRun <&> \invoke -> invoke (constArg px # ctx)

combineEventHandlers :: forall env
                      . Either String (CombinedEventHandler env)
                     -> Either String (CombinedEventHandler env)
                     -> Either String (CombinedEventHandler env)
combineEventHandlers err@(Left _) _ = err
combineEventHandlers _ err@(Left _) = err
combineEventHandlers (Right lhs) (Right rhs) = do
    let atMostOne :: String -> (CombinedEventHandler env -> Maybe b) -> Either String (Maybe b)
        atMostOne what how = case (how lhs, how rhs) of
          (Just _, Just _) -> Left ("Duplicate definitions of the " ++ what ++ " event handler")
          (Nothing, x)     -> Right x
          (x, Nothing)     -> Right x
        uniqueKey :: String -> (CombinedEventHandler env -> Map String v) -> Either String (Map String v)
        uniqueKey what how =
          case Set.toList $ Set.intersection (Map.keysSet $ how lhs) (Map.keysSet $ how rhs) of
            [] -> Right (how lhs `Map.union` how rhs)
            (k:_) -> Left ("Duplicate definitions of the " ++ what ++ " event handler named `" ++ k ++ "`")
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
                 -> IO (Either String (Double -> Event -> Maybe (IO ())))
buildHandlerWith exec (SomeContext ctx) script = do
  let env = contextToEnv ctx
  case runParsedToolScript script env of
    Left err -> pure (Left err)
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
-- @Double -> Event -> Maybe (IO ())@ dispatcher.  The pure/interpreter backend
-- uses 'defaultToolRunner'; the LLVM backend supplies one that JIT-compiles the
-- handlers.
newtype ToolRunner = ToolRunner
  { runTool :: (Int -> DrawSink)
            -> Int
            -> SomeContext EventArgument_
            -> ParsedToolScript
            -> IO (Either String (Double -> Event -> Maybe (IO ()))) }

-- | The interpreter tool runner: evaluate draw values through the pure
-- interpreter and emit them into the given layer's 'DrawSink'.
defaultToolRunner :: ToolRunner
defaultToolRunner = ToolRunner $ \sink layer ctx script ->
  buildHandlerWith (\_ code -> pure (\c -> run (drawHandlerForSink (sink layer)) c code)) ctx script

-- | Adapt a 'DrawSink' into an interpreter 'DrawHandler': evaluate each draw
-- command's value arguments and forward the concrete values to the sink.
drawHandlerForSink :: DrawSink -> DrawHandler ScalarIORefM
drawHandlerForSink sink = DrawHandler $ \case
  DrawPoint _ pv          -> eval pv >>= liftIO . dsPoint sink
  DrawCircle _ fill rv pv -> do r <- eval rv; p <- eval pv; liftIO (dsCircle sink fill r p)
  DrawLine _ fv tv        -> do f <- eval fv; t <- eval tv; liftIO (dsLine sink f t)
  DrawRect _ fill fv tv   -> do f <- eval fv; t <- eval tv; liftIO (dsRect sink fill f t)
  SetStroke _ cv          -> eval cv >>= liftIO . dsStroke sink
  SetFill _ cv            -> eval cv >>= liftIO . dsFill sink
  Clear _                 -> liftIO (dsClear sink)
  Write _ tv pv           -> do txt <- eval tv; pt <- eval pv; liftIO (dsWrite sink txt pt)

type EventDependencies =
  (Dynamic (Either String Splices),
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

type ClickHandlerEnv env =
  ( '(InternalX, 'RealT) ': '(InternalY, 'RealT) ':
    '(InternalPx, 'RealT) ': env )

type MissingClickArgs env =
  ( NotPresent InternalX  env, NotPresent InternalY  env
  , NotPresent InternalPx env )

assertMissingClickArgs :: forall env a
                       . EnvironmentProxy env
                      -> (MissingClickArgs env => a)
                      -> a
assertMissingClickArgs env k =
  assertAbsent (Proxy @InternalX)    RealType env $
  assertAbsent (Proxy @InternalY)    RealType env $
  assertAbsent (Proxy @InternalPx)   RealType env k

type Bookkeeping env =
    ( '(InternalIterations, 'IntegerT) ': '(InternalStuck, 'BooleanT) ':
      '(InternalIterationLimit, 'IntegerT) ':
      '(InternalEscapeRadius, 'RealT) ': '(InternalVanishingRadius, 'RealT) ':
      env)

withBookkeeping :: forall env
                 . EnvironmentProxy env
                -> Splices
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> (KnownEnvironment (Bookkeeping env) =>
                    EnvironmentProxy (Bookkeeping env) ->
                    Splices ->
                    Either (SourceRange, String) (Code (Bookkeeping env)))
                -> Either (SourceRange, String) (Code env)
withBookkeeping env splices (pMaxIters, pMaxRadius, pMinRadius) action = withEnvironment env $ do
  env' :: EnvironmentProxy (Bookkeeping env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
      ) env
  withEnvironment env' $ do
    code <- action env' splices
    -- Now bind all of the bookkeeping variables
    let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), code0) =
          (env', code) & letInEnv (Const (Scalar typeProxy 0))
                       & letInEnv (Const (Scalar typeProxy False))
    code1 <- snd . (`letInEnv` (env0, code0)) <$> pvAtType pMaxIters  IntegerType env1
    code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
    code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3
    pure code'

-- | Apply an already-'Earley'-parsed script body to a concrete environment,
-- running its (deferred) typechecking phase. This is the tail end of
-- 'parseCode', minus the tokenize-and-parse step, for use when the body's
-- 'ParsedCode' was already recovered as part of a larger single parse (e.g.
-- a whole multi-block tool script) rather than from its own separate
-- source string.
runParsedBody :: EnvironmentProxy env -> ParsedCode
              -> Either (Either ParseError TCError) (Code env)
runParsedBody env (ParsedCode f) = case f env of TC x -> first Right x

parseClickScript :: Splices
                 -> Coordinate
                -> (ParsedValue, ParsedValue, ParsedValue)
                 -> Maybe String
                 -> ParsedCode
                 -> (forall env. EnvironmentProxy env
                     -> Either (SourceRange, String) (Code (ClickHandlerEnv env)))
parseClickScript splices0 clickCoord limits mpixel body
  (env0 :: EnvironmentProxy env) = do

  env1 <- (declareE InternalX  RealType
       <=< declareE InternalY  RealType
       <=< declareE InternalPx RealType) env0

  case clickCoord of
    RealCoordinates p q -> case (someSymbolVal p, someSymbolVal q) of
      (SomeSymbol xcoord, SomeSymbol ycoord) -> do
        withBookkeeping env1 splices0 limits $ \env _splices -> do
          x <- getVar InternalX RealType env
          bindOrDeclare xcoord RealType x env $ \env' -> do
            y <- getVar InternalY RealType env'
            bindOrDeclare ycoord RealType y env' $ \env'' -> do
              SomeSymbol (px :: Proxy px) <- pure (someSymbolVal $ fromMaybe "[unused] pixel size" mpixel)
              case lookupEnv px RealType env'' of
                Absent pf -> recallIsAbsent pf $ do
                  let env3 = BindingProxy px RealType env''
                  ip <- getVar InternalPx RealType env''
                  code <- left (errorLocation &&& unlines . pp) (runParsedBody env3 body)
                  pure (Let (bindName px RealType pf) px ip code)
                _ -> Left (NoSourceRange, "The pixel size variable `" ++ symbolVal px ++ "` was redefined.")

    ComplexCoordinate c -> case someSymbolVal c of
      SomeSymbol coord -> do

        withBookkeeping env1 splices0 limits $ \env _splices -> do
          let i = Const (Scalar ComplexType (0 :+ 1))
          x <- getVar InternalX RealType env
          y <- getVar InternalY RealType env
          bindOrDeclare coord ComplexType (R2C x + i * R2C y) env $ \env' -> do
              SomeSymbol (px :: Proxy px) <- pure (someSymbolVal $ fromMaybe "[unused] pixel size" mpixel)
              case lookupEnv px RealType env' of
                Absent pf -> recallIsAbsent pf $ do
                  let env2 = BindingProxy px RealType env'
                  ip <- getVar InternalPx RealType env'
                  code <- left (errorLocation &&& unlines . pp) (runParsedBody env2 body)
                  pure (Let (bindName px RealType pf) px ip code)
                _ -> Left (NoSourceRange, "The pixel size variable `" ++ symbolVal px ++ "` was redefined.")

------------------------------------------------------------
-- Drag-type event handler
------------------------------------------------------------

type InternalOldX  = "[internal] oldx"
type InternalOldY  = "[internal] oldy"

type MissingDragArgs env =
  ( NotPresent InternalX  env, NotPresent InternalY  env
  , NotPresent InternalOldX env, NotPresent InternalOldY  env
  , NotPresent InternalPx env )

assertMissingDragArgs :: forall env a
                       . EnvironmentProxy env
                      -> (MissingDragArgs env => a)
                      -> a
assertMissingDragArgs env k =
  assertAbsent (Proxy @InternalX)    RealType env $
  assertAbsent (Proxy @InternalY)    RealType env $
  assertAbsent (Proxy @InternalOldX) RealType env $
  assertAbsent (Proxy @InternalOldY) RealType env $
  assertAbsent (Proxy @InternalPx)   RealType env k

type DragHandlerEnv env =
  ( '(InternalX, 'RealT) ': '(InternalY, 'RealT) ':
    '(InternalOldX, 'RealT) ': '(InternalOldY, 'RealT) ':
    '(InternalPx, 'RealT) ': env )

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
                 "Expected variable `" ++ "` should be " ++ an ty ++ ", not " ++ an ty')

  Found pf -> withEnvironment env $ do
    code <- action env
    pure $ Block [ Set pf name v, code ]

  Absent pf -> withKnownType ty $ do
    let env' = recallIsAbsent pf $ BindingProxy name ty env
    code <- withEnvironment env' $ action env'
    pure (snd $ letInEnv @name v (env', code))

parseDragScript :: Splices
                -> Coordinate
                -> Coordinate
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Maybe String
                -> ParsedCode
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (DragHandlerEnv env)))
parseDragScript _splices curCoord oldCoord (pMaxIters, pMaxRadius, pMinRadius) mpixel body
  (env :: EnvironmentProxy env) = do
  withEnvironment env $ do
    -- Bind all of the internal bookkeeping variables
    env' :: EnvironmentProxy (InternalDragHandlerEnv env) <-
      (     declareE InternalIterations      IntegerType
        <=< declareE InternalStuck           BooleanType
        <=< declareE InternalIterationLimit  IntegerType
        <=< declareE InternalEscapeRadius    RealType
        <=< declareE InternalVanishingRadius RealType
        <=< declareE InternalX               RealType
        <=< declareE InternalY               RealType
        <=< declareE InternalOldX            RealType
        <=< declareE InternalOldY            RealType
        <=< declareE InternalPx              RealType
      ) env

    withEnvironment env' $ do

      SomeSymbol (coord1 :: Proxy coordT1) <- pure $ case curCoord of
        ComplexCoordinate c -> someSymbolVal c
        RealCoordinates{}   -> error "INTERNAL ERROR: Non-complex event coordinates are not yet implemented."
      SomeSymbol (coord2 :: Proxy coordT2) <- pure $ case oldCoord of
        ComplexCoordinate c -> someSymbolVal c
        RealCoordinates{}   -> error "INTERNAL ERROR: Non-complex event coordinates are not yet implemented."

      let i :: forall e. KnownEnvironment e => Value '(e, ComplexT)
          i = Const (Scalar ComplexType (0 :+ 1))
      x  <- getVar InternalX    RealType env'
      y  <- getVar InternalY    RealType env'

      code :: Code (InternalDragHandlerEnv env) <-
        bindOrDeclare coord1 ComplexType (R2C x  + i * R2C y ) env' $ \env1 -> do
          x' <- getVar InternalOldX RealType env1
          y' <- getVar InternalOldY RealType env1
          bindOrDeclare coord2 ComplexType (R2C x' + i * R2C y') env1 $ \env2 ->
            case someSymbolVal <$> mpixel of
              Nothing -> left (errorLocation &&& unlines . pp) (runParsedBody env2 body)
              Just (SomeSymbol (px :: Proxy px)) -> do
                case lookupEnv px RealType env2 of
                  Absent pf -> recallIsAbsent pf $ do
                    let env3 = BindingProxy px RealType env2
                    p <- getVar InternalPx RealType env2
                    c <- left (errorLocation &&& unlines . pp) (runParsedBody env3 body)
                    pure (snd $ letInEnv @px p (env3, c))
                  _ -> Left (NoSourceRange, "The pixel size variable `" ++ symbolVal px ++ "` was redefined.")

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

type UnitHandlerEnv env = '(InternalPx, 'RealT) ': env

type InternalUnitHandlerEnv env =
  ( '(InternalIterations, 'IntegerT) ':
    '(InternalStuck, 'BooleanT) ':
    '(InternalIterationLimit, 'IntegerT) ':
    '(InternalEscapeRadius, 'RealT) ':
    '(InternalVanishingRadius, 'RealT) ':
    UnitHandlerEnv env)

type MissingUnitArgs env = ( NotPresent InternalPx env )

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
assertMissingUnitArgs = assertAbsent (Proxy @InternalPx) RealType

parseUnitScript :: Splices
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Coordinate
                -> Maybe String
                -> ParsedCode
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (UnitHandlerEnv env)))
parseUnitScript _splices (pMaxIters, pMaxRadius, pMinRadius) _vc mpx body
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
      ) env

    withEnvironment env' $ case someSymbolVal <$> mpx of
      Nothing -> do

        code <- left (errorLocation &&& unlines . pp) (runParsedBody env' body)

        let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), code0) =
              (env', code) & letInEnv (Const (Scalar typeProxy 0))
                           & letInEnv (Const (Scalar typeProxy False))

        code1 <- snd . (`letInEnv` (env0, code0)) <$> pvAtType pMaxIters  IntegerType env1
        code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
        code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3

        pure code'

      Just (SomeSymbol px) -> do
        case lookupEnv px RealType env' of
          Absent pf -> do
            let env'' = recallIsAbsent pf $ BindingProxy px RealType env'

            code0 <- left (errorLocation &&& unlines . pp) (runParsedBody env'' body)
            let code = Let bindingEvidence px (Var (Proxy @InternalPx) RealType bindingEvidence) code0
            let (env0@(BindingProxy _ _ env1@(BindingProxy _ _ env2@(BindingProxy _ _ env3))), codeX) =
                  (env', code) & letInEnv (Const (Scalar typeProxy 0))
                               & letInEnv (Const (Scalar typeProxy False))

            code1 <- snd . (`letInEnv` (env0, codeX)) <$> pvAtType pMaxIters  IntegerType env1
            code2 <- snd . (`letInEnv` (env1, code1)) <$> pvAtType pMaxRadius RealType    env2
            code' <- snd . (`letInEnv` (env2, code2)) <$> pvAtType pMinRadius RealType    env3

            pure code'
          _ -> Left (NoSourceRange, "Pixel variable `" ++ symbolVal px ++ "` was redefined.")


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
-- not just over the final environment (the tool's variable context), the
-- same way 'ParsedCode' defers over its environment, but also over the
-- default coordinate\/iteration-limits\/pixel-size-variable that a header
-- omitting @at ...@\/@from ... to ...@ falls back on -- so that
-- 'toolBlocksGrammar' can be built /once/, independent of which viewer a
-- @\@tool@ stanza in a unified project script ends up targeting (that's
-- only known once every @\@viewer@ stanza's header has been parsed, which
-- may happen after the tool's own stanza in the source). The single-tool
-- entry point below ('parseToolScript') simply applies these immediately,
-- since it already has them in hand before parsing starts.
newtype ParsedToolBlock = ParsedToolBlock
  (Coordinate -> (ParsedValue, ParsedValue, ParsedValue) -> Maybe String
   -> forall env. EnvironmentProxy env -> Either String (CombinedEventHandler env))

-- | The result of parsing an entire tool script: all of its blocks already
-- merged and checked for duplicate event definitions.
newtype ParsedToolScript = ParsedToolScript
  { runParsedToolScript :: forall env. EnvironmentProxy env -> Either String (CombinedEventHandler env) }

mergeToolBlocks :: [ParsedToolBlock]
                -> Coordinate
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Maybe String
                -> ParsedToolScript
mergeToolBlocks blocks defaultCoord limits mpixel = ParsedToolScript $ \env ->
  foldl' combineEventHandlers (Right noHandlers)
    [ f defaultCoord limits mpixel env | ParsedToolBlock f <- blocks ]

-- | A tool script with no event handlers at all (e.g. a tool whose
-- @script:@ field is absent or empty).
emptyToolScript :: ParsedToolScript
emptyToolScript = ParsedToolScript $ const (Right noHandlers)

-- | Parse a tool's whole @script:@ text into its combined event handler.
parseToolScript :: Splices
                 -> Coordinate                              -- ^ default coordinate (the viewer's z-coord)
                 -> (ParsedValue, ParsedValue, ParsedValue)  -- ^ iteration\/escape\/vanishing limits
                 -> Maybe String                             -- ^ pixel-size variable name, if any
                 -> String                                   -- ^ the whole script source
                 -> Either (SourceRange, String) ParsedToolScript
parseToolScript splices defaultCoord limits mpixel input =
  bimap (errorLocation &&& unlines . pp) (\blocks -> mergeToolBlocks blocks defaultCoord limits mpixel)
    (parse (toolScriptGrammar splices) (tokenizeWithIndentation input))

-- | The grammar for a whole tool script: a sequence of @on <event>:@ blocks,
-- surrounded by the @Indent@\/@Dedent@ pair that delimits the tool's own
-- @script:@ field. Thin wrapper around 'toolBlocksGrammar', the same way
-- 'Language.Code.Parser.codeBlockGrammar' wraps 'codeStatementsGrammar'.
toolScriptGrammar :: forall r
                   . Splices
                  -> Grammar r (Prod r [ParsedToolBlock])
toolScriptGrammar splices = do
  blocks <- toolBlocksGrammar splices
  rule (token Indent *> blocks <* token Dedent)

-- | The grammar for a bare sequence of @on <event>:@ blocks with no
-- surrounding @Indent@\/@Dedent@ tokens. Exported so that a grammar whose
-- tool-script bodies aren't indentation-delimited (e.g. a future unified
-- project-script grammar, where an @\@tool@ stanza's @on ...:@ blocks sit
-- flush with the stanza's own header rather than nested beneath it) can
-- reuse the full set of event-block headers without re-deriving them.
toolBlocksGrammar :: forall r
                   . Splices
                  -> Grammar r (Prod r [ParsedToolBlock])
toolBlocksGrammar splices = mdo

  codeBlock <- codeBlockGrammar splices

  colon <- rule (token Colon <?> ":")
  nl    <- rule (token Newline <?> "end of line")

  coordSpec <- rule $
        (ComplexCoordinate <$> ident)
    <|> (RealCoordinates
          <$> (token OpenParen *> ident <* token Comma)
          <*> (ident <* token CloseParen))

  fromTo <- rule $
    (,) <$> (tok "from" *> coordSpec) <*> (tok "to" *> coordSpec)

  quoted <- rule (tokenMatch (\case { Quoted s -> Just s; _ -> Nothing }))
  intLit <- rule (tokenMatch (\case { NumberI n -> Just (fromIntegral n); _ -> Nothing }))

  let dummyStart = ComplexCoordinate "[unused] start"

  onRefresh' <- rule $
    (\body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\u -> noHandlers { onRefresh = Just u })
          (parseUnitScript splices limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "refresh" *> colon *> nl *> codeBlock)

  onActivated' <- rule $
    (\body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\u -> noHandlers { onActivated = Just u })
          (parseUnitScript splices limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "activated" *> colon *> nl *> codeBlock)

  onDeactivated' <- rule $
    (\body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\u -> noHandlers { onDeactivated = Just u })
          (parseUnitScript splices limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "deactivated" *> colon *> nl *> codeBlock)

  onButton' <- rule $
    (\label body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\u -> noHandlers { onButton = Map.singleton label u })
          (parseUnitScript splices limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "button" *> quoted <* colon <* nl)
    <*> codeBlock

  onTimer' <- rule $
    (\name interval body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\u -> noHandlers { onTimer = Map.singleton name (interval, u) })
          (parseUnitScript splices limits defaultCoord mpixel body env))
    <$> (tok "on" *> tok "timer" *> quoted)
    <*> (tok "every" *> intLit <* tok "ms" <* colon <* nl)
    <*> codeBlock

  onClick' <- rule $
    (\mc body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\c -> noHandlers { onClick = Just c })
          (parseClickScript splices (fromMaybe defaultCoord mc) limits mpixel body env))
    <$> (tok "on" *> tok "click" *> optional (tok "at" *> coordSpec) <* colon <* nl)
    <*> codeBlock

  onDoubleClick' <- rule $
    (\mc body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        bimap snd (\c -> noHandlers { onDoubleClick = Just c })
          (parseClickScript splices (fromMaybe defaultCoord mc) limits mpixel body env))
    <$> (tok "on" *> tok "double" *> token Minus *> tok "click"
          *> optional (tok "at" *> coordSpec) <* colon <* nl)
    <*> codeBlock

  onClickOrDrag' <- rule $
    (\mc body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        let c = fromMaybe defaultCoord mc
        in bimap snd (\(ch, dh) -> noHandlers { onClick = Just ch, onDrag = Just dh })
             ((,) <$> parseClickScript splices c limits mpixel body env
                  <*> parseDragScript  splices c dummyStart limits mpixel body env))
    <$> (tok "on" *> tok "click" *> token Minus *> token Or_ *> token Minus *> tok "drag"
          *> optional (tok "at" *> coordSpec) <* colon <* nl)
    <*> codeBlock

  onDrag' <- rule $
    (\mft body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        let (startC, curC) = fromMaybe (dummyStart, defaultCoord) mft
        in bimap snd (\d -> noHandlers { onDrag = Just d })
             (parseDragScript splices curC startC limits mpixel body env))
    <$> (tok "on" *> tok "drag" *> optional fromTo <* colon <* nl)
    <*> codeBlock

  onDragFinished' <- rule $
    (\mft body -> ParsedToolBlock $ \defaultCoord limits mpixel env ->
        let (startC, curC) = fromMaybe (dummyStart, defaultCoord) mft
        in bimap snd (\d -> noHandlers { onDragDone = Just d })
             (parseDragScript splices curC startC limits mpixel body env))
    <$> (tok "on" *> tok "drag" *> token Minus *> tok "finished" *> optional fromTo <* colon <* nl)
    <*> codeBlock

  toolBlock <- ruleChoice
    [ onClick', onDoubleClick', onClickOrDrag', onDragFinished', onDrag'
    , onRefresh', onActivated', onDeactivated', onButton', onTimer'
    ]

  rule (many toolBlock)

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

data Coordinate = ComplexCoordinate String | RealCoordinates String String

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
