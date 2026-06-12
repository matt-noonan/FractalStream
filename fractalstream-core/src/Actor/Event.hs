module Actor.Event
  ( Event(..)
  , type EventDependencies

  , SingleEventHandler(..)
  , ClickHandler(..)
  , DragHandler(..)
  , TimerHandler(..)
  , ButtonHandler(..)
  , EventArgument_
  , EventArgument(..)

  , Coordinate(..)
  , buildHandler
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

  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Typecheck
import Language.Code
import Language.Parser.SourceRange
import Language.Value.Typecheck
import Language.Code.Parser
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
import Actor.Layout (CodeString(..))
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

data SingleEventHandler
  = OnClick ClickHandler
  | OnDoubleClick ClickHandler
  | OnClickOrDrag (ClickHandler, DragHandler)
  | OnDrag DragHandler
  | OnDragDone DragHandler
  | OnTimer TimerHandler
  | OnButton ButtonHandler
  | OnRefresh UnitHandler
  | OnActivated UnitHandler
  | OnDeactivated UnitHandler

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

-- | How a single tool event handler's 'Code' is executed: given a witness for
-- its environment, the argument context, and the code, run it.  The interpreter
-- ignores the proxy and uses 'run'; the LLVM backend uses it to JIT the handler.
type ToolExec =
  forall e. EnvironmentProxy e -> Context EventArgument_ e -> Code e -> IO ()

makeEventHandler :: forall env
                  . (MissingClickArgs env, MissingDragArgs env, MissingUnitArgs env)
                 => ToolExec
                 -> Context EventArgument_ env
                 -> CombinedEventHandler env
                 -> Double
                 -> Event
                 -> Maybe (IO ())
makeEventHandler exec ctx CombinedEventHandler{..} = \px -> \case
  Click (x, y) -> onClick <&> \code ->
    let c = constArg x # constArg y # constArg px # ctx in exec (contextToEnv c) c code
  DoubleClick (x, y) -> onDoubleClick <&> \code ->
    let c = constArg x # constArg y # constArg px # ctx in exec (contextToEnv c) c code
  Drag (x, y) (x', y') -> onDrag <&> \code ->
    let c = constArg x # constArg y # constArg x' # constArg y' # constArg px # ctx in exec (contextToEnv c) c code
  DragDone (x, y) (x', y') -> onDragDone <&> \code ->
    let c = constArg x # constArg y # constArg x' # constArg y' # constArg px # ctx in exec (contextToEnv c) c code
  Timer name -> Map.lookup name onTimer <&> \(_, code) ->
    let c = constArg px # ctx in exec (contextToEnv c) c code
  ButtonPressed name -> Map.lookup name onButton <&> \code ->
    let c = constArg px # ctx in exec (contextToEnv c) c code
  Refresh -> onRefresh <&> \code ->
    let c = constArg px # ctx in exec (contextToEnv c) c code
  Activated -> onActivated <&> \code ->
    let c = constArg px # ctx in exec (contextToEnv c) c code
  Deactivated -> onDeactivated <&> \code ->
    let c = constArg px # ctx in exec (contextToEnv c) c code

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

singleToCombined :: EnvironmentProxy env -> SingleEventHandler -> IO (Either String (CombinedEventHandler env))
singleToCombined env = \case
  OnClick ClickHandler{..} -> getDynamic (dyn chScript) <&> \(SomeClickHandler script) ->
    bimap snd (\h -> noHandlers { onClick = Just h }) (script env)
  OnDoubleClick ClickHandler{..} -> getDynamic (dyn chScript) <&> \(SomeClickHandler script) ->
    bimap snd (\h -> noHandlers { onDoubleClick = Just h }) (script env)
  OnDrag DragHandler{..} -> getDynamic (dyn dhScript) <&> \(SomeDragHandler script) ->
    bimap snd (\h -> noHandlers { onDrag = Just h }) (script env)
  OnDragDone DragHandler{..} -> getDynamic (dyn dhScript) <&> \(SomeDragHandler script) ->
    bimap snd (\h -> noHandlers { onDragDone = Just h }) (script env)
  OnClickOrDrag (ClickHandler{..}, DragHandler{..}) ->
    ((,) <$> getDynamic (dyn chScript) <*> getDynamic (dyn dhScript)) <&>
      \(SomeClickHandler cscript, SomeDragHandler dscript) ->
        bimap snd (\(ch, dh) -> noHandlers { onClick = Just ch
                                           , onDrag  = Just dh
                                           }) ((,) <$> cscript env <*> dscript env)
  OnTimer TimerHandler{..} -> do
    name <- getDynamic (dyn thName)
    interval <- getDynamic (dyn thInterval)
    SomeUnitHandler script <- getDynamic (dyn thScript)
    pure $ bimap snd (\h -> noHandlers { onTimer = Map.singleton name (interval, h) }) (script env)
  OnButton ButtonHandler{..} -> do
    name <- getDynamic (dyn bhName)
    SomeUnitHandler script <- getDynamic (dyn bhScript)
    pure $ bimap snd (\h -> noHandlers { onButton = Map.singleton name h }) (script env)
  OnRefresh (UnitHandler code) -> do
    SomeUnitHandler script <- getDynamic (dyn code)
    pure $ bimap snd (\h -> noHandlers { onRefresh = Just h }) (script env)
  OnActivated (UnitHandler code) -> do
    SomeUnitHandler script <- getDynamic (dyn code)
    pure $ bimap snd (\h -> noHandlers { onActivated = Just h }) (script env)
  OnDeactivated (UnitHandler code) -> do
    SomeUnitHandler script <- getDynamic (dyn code)
    pure $ bimap snd (\h -> noHandlers { onDeactivated = Just h }) (script env)

-- | Parse a tool's event handlers and build its event dispatcher, with handler
-- execution supplied by the given 'ToolExec' (interpreter or JIT-compiled).
buildHandlerWith :: ToolExec
                 -> SomeContext EventArgument_
                 -> [SingleEventHandler]
                 -> IO (Either String (Double -> Event -> Maybe (IO ())))
buildHandlerWith exec (SomeContext ctx) handlers = do
  let env = contextToEnv ctx
  ecombined <- foldl' combineEventHandlers (Right noHandlers) <$> mapM (singleToCombined env) handlers
  case ecombined of
    Left err -> pure (Left err)
    Right combined0 ->
      assertMissingClickArgs env $
      assertMissingDragArgs env $
      assertMissingUnitArgs env $
      let combined = case (onDrag combined0, onDragDone combined0) of
            (Just _, Nothing) -> combined0 { onDragDone = Just NoOp }
            _ -> combined0
      in pure (pure $ makeEventHandler exec ctx combined)

-- | The interpreter handler builder: execute each handler through 'run'.
buildHandler :: DrawHandler ScalarIORefM
             -> SomeContext EventArgument_
             -> [SingleEventHandler]
             -> IO (Either String (Double -> Event -> Maybe (IO ())))
buildHandler draw = buildHandlerWith (\_ c code -> run draw c code)

-- | How a backend turns a tool's parsed event handlers into the event
-- dispatcher the UI calls.  Given the per-layer draw handler (from the tool
-- draw-command accumulator), the layer number, the tool's variable context, and
-- the parsed handlers, it produces (or fails to produce) a
-- @Double -> Event -> Maybe (IO ())@ dispatcher.  The pure/interpreter backend
-- uses 'defaultToolRunner'; the LLVM backend supplies one that JIT-compiles the
-- handlers.
newtype ToolRunner = ToolRunner
  { runTool :: (Int -> DrawSink)
            -> Int
            -> SomeContext EventArgument_
            -> [SingleEventHandler]
            -> IO (Either String (Double -> Event -> Maybe (IO ()))) }

-- | The interpreter tool runner: evaluate draw values through the pure
-- interpreter and emit them into the given layer's 'DrawSink'.
defaultToolRunner :: ToolRunner
defaultToolRunner = ToolRunner $ \sink layer ctx handlers ->
  buildHandlerWith (\_ c code -> run (drawHandlerForSink (sink layer)) c code) ctx handlers

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

instance CodecWith EventDependencies SingleEventHandler where
  codecWith_ ctx = match
    [ Fragment OnClick (\case { OnClick h -> Just h; _ -> Nothing}) $
        "event" `mustBe` "click" $ codecWith ctx
    , Fragment OnDoubleClick (\case { OnDoubleClick h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "double-click" $ codecWith ctx
    , Fragment OnClickOrDrag (\case { OnClickOrDrag h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "click-or-drag" $ codecWith ctx
    , Fragment OnDrag (\case { OnDrag h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "drag" $ codecWith ctx
    , Fragment OnDragDone (\case { OnDragDone h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "drag-finished" $ codecWith ctx
    , Fragment OnTimer (\case { OnTimer h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "timer" $ codecWith ctx
    , Fragment OnButton (\case { OnButton h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "button" $ codecWith ctx
    , Fragment OnRefresh (\case { OnRefresh h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "refresh" $ codecWith ctx
    , Fragment OnActivated (\case { OnActivated h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "activated" $ codecWith ctx
    , Fragment OnDeactivated (\case { OnDeactivated h -> Just h; _ -> Nothing}) $ do
        "event" `mustBe` "deactivated" $ codecWith ctx
    ]

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

data ClickHandler = ClickHandler
  { chCoord  :: Variable (Maybe Coordinate)
  , chCanUpdateViewer :: Variable Bool
  , chScript :: Mapped CodeString SomeClickHandler }

instance CodecWith EventDependencies ClickHandler where
  codecWith_ ctx = do
    coord  <-coerce . chCoord-< fmap coerce <$> codec @(Variable MCoordinate)
    update <-chCanUpdateViewer-< keyWithDefaultValue False "can-update-viewer-coords"
    script <-chScript-< mapped (key "code") $ \use -> do
      let (dsplices, dvc, dlimits, dmpx) = use ctx
          complain err = pure (\_ -> SomeClickHandler (\_ -> Left (NoSourceRange, err)))
      dvc >>= \case
        Left err -> complain err
        Right vc -> dsplices >>= \case
          Left err -> complain err
          Right splices -> dmpx >>= \case
            Left err -> complain err
            Right mpx -> do
              pt <- fromMaybe vc <$> dyn (use coord)
              dlimits >>= \case
                Left err -> complain err
                Right limits -> pure (fmap SomeClickHandler $ parseClickScript splices pt limits mpx)
    build ClickHandler coord update script

newtype SomeClickHandler = SomeClickHandler (forall env. EnvironmentProxy env -> Either (SourceRange, String) (Code (ClickHandlerEnv env)))

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

parseClickScript :: Splices
                 -> Coordinate
                -> (ParsedValue, ParsedValue, ParsedValue)
                 -> Maybe String
                 -> CodeString
                 -> (forall env. EnvironmentProxy env
                     -> Either (SourceRange, String) (Code (ClickHandlerEnv env)))
parseClickScript splices0 clickCoord limits mpixel (CodeString src)
  (env0 :: EnvironmentProxy env) = do

  env1 <- (declareE InternalX  RealType
       <=< declareE InternalY  RealType
       <=< declareE InternalPx RealType) env0

  case clickCoord of
    RealCoordinates p q -> case (someSymbolVal p, someSymbolVal q) of
      (SomeSymbol xcoord, SomeSymbol ycoord) -> do
        withBookkeeping env1 splices0 limits $ \env splices -> do
          x <- getVar InternalX RealType env
          bindOrDeclare xcoord RealType x env $ \env' -> do
            y <- getVar InternalY RealType env'
            bindOrDeclare ycoord RealType y env' $ \env'' -> do
              SomeSymbol (px :: Proxy px) <- pure (someSymbolVal $ fromMaybe "[unused] pixel size" mpixel)
              case lookupEnv px RealType env'' of
                Absent pf -> recallIsAbsent pf $ do
                  let env3 = BindingProxy px RealType env''
                  ip <- getVar InternalPx RealType env''
                  code <- left (errorLocation &&& unlines . pp) (parseCode env3 splices src)
                  pure (Let (bindName px RealType pf) px ip code)
                _ -> Left (NoSourceRange, "The pixel size variable `" ++ symbolVal px ++ "` was redefined.")

    ComplexCoordinate c -> case someSymbolVal c of
      SomeSymbol coord -> do

        withBookkeeping env1 splices0 limits $ \env splices -> do
          let i = Const (Scalar ComplexType (0 :+ 1))
          x <- getVar InternalX RealType env
          y <- getVar InternalY RealType env
          bindOrDeclare coord ComplexType (R2C x + i * R2C y) env $ \env' -> do
              SomeSymbol (px :: Proxy px) <- pure (someSymbolVal $ fromMaybe "[unused] pixel size" mpixel)
              case lookupEnv px RealType env' of
                Absent pf -> recallIsAbsent pf $ do
                  let env2 = BindingProxy px RealType env'
                  ip <- getVar InternalPx RealType env'
                  code <- left (errorLocation &&& unlines . pp) (parseCode env2 splices src)
                  pure (Let (bindName px RealType pf) px ip code)
                _ -> Left (NoSourceRange, "The pixel size variable `" ++ symbolVal px ++ "` was redefined.")

------------------------------------------------------------
-- Drag-type event handler
------------------------------------------------------------

type InternalOldX  = "[internal] oldx"
type InternalOldY  = "[internal] oldy"

data DragHandler = DragHandler
  { dhCoord  :: Variable (Maybe Coordinate)
  , dhStart  :: Variable (Maybe Coordinate)
  , dhCanUpdateViewer :: Variable Bool
  , dhScript :: Mapped CodeString SomeDragHandler }

instance CodecWith EventDependencies DragHandler where
  codecWith_ ctx = do
    coord  <-coerce . dhCoord-< fmap coerce <$> codec @(Variable MCoordinate)
    start  <-coerce . dhStart-< fmap coerce <$> codec @(Variable SCoordinate)
    update <-dhCanUpdateViewer-< keyWithDefaultValue False "can-update-viewer-coords"
    script <-dhScript-< mapped (key "code") $ \use -> do
      let (dsplices, dvc, dlimits, dmpx) = use ctx
          complain err = pure (\_ -> SomeDragHandler (\_ -> Left (NoSourceRange, err)))
      ((\x1 x2 x3 x4 -> (,,,) <$> x1 <*> x2 <*> x3 <*> x4)
        <$> dvc <*> dsplices <*> dlimits <*> dmpx) >>= \case
        Left err -> complain err
        Right (vc, splices, limits, mpx) -> do
          pt  <- fromMaybe vc <$> dyn (use coord)
          pt' <- fromMaybe (ComplexCoordinate "[unused] start") <$> dyn (use start)
          pure (fmap SomeDragHandler $ parseDragScript splices pt pt' limits mpx)
    build DragHandler coord start update script

newtype SomeDragHandler = SomeDragHandler (forall env. EnvironmentProxy env -> Either (SourceRange, String) (Code (DragHandlerEnv env)))

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
                -> CodeString
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (DragHandlerEnv env)))
parseDragScript splices curCoord oldCoord (pMaxIters, pMaxRadius, pMinRadius) mpixel (CodeString src)
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
              Nothing -> left (errorLocation &&& unlines . pp) (parseCode env2 splices src)
              Just (SomeSymbol (px :: Proxy px)) -> do
                case lookupEnv px RealType env2 of
                  Absent pf -> recallIsAbsent pf $ do
                    let env3 = BindingProxy px RealType env2
                    p <- getVar InternalPx RealType env2
                    c <- left (errorLocation &&& unlines . pp) (parseCode env3 splices src)
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

newtype SomeUnitHandler = SomeUnitHandler (forall env. EnvironmentProxy env -> Either (SourceRange, String) (Code (UnitHandlerEnv env)))

newtype UnitHandler = UnitHandler (Mapped CodeString SomeUnitHandler)

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

instance CodecWith EventDependencies (Mapped CodeString SomeUnitHandler) where
  codecWith_ ctx = mapped (key "code") $ \use -> do
    let (dsplices, dvc, dlimits, dpx) = use ctx
    ((\x1 x2 x3 x4 -> (,,,) <$> x1 <*> x2 <*> x3 <*> x4)
      <$> dvc <*> dsplices <*> dlimits <*> dpx) >>= \case
      Left err -> pure (\_ -> SomeUnitHandler $ \_ -> Left (NoSourceRange, err))
      Right (vc, splices, limits, px) -> do
        pure (fmap SomeUnitHandler $ parseUnitScript splices limits vc px)

instance CodecWith EventDependencies UnitHandler where
  codecWith_ ctx = do
    script <-coerce-< fmap coerce <$> codecWith @(Mapped CodeString SomeUnitHandler) ctx
    build UnitHandler script

data TimerHandler = TimerHandler
  { thName     :: Variable String
  , thInterval :: Variable Int
  , thScript   :: Mapped CodeString SomeUnitHandler }

instance CodecWith EventDependencies TimerHandler where
  codecWith_ ctx = do
    name     <-thName-<     key "name"
    interval <-thInterval-< key "interval"
    script   <-thScript-<   codecWith ctx
    build TimerHandler name interval script

data ButtonHandler = ButtonHandler
  { bhName   :: Variable String
  , bhScript :: Mapped CodeString SomeUnitHandler }

instance CodecWith EventDependencies ButtonHandler where
  codecWith_ ctx = do
    name   <-bhName-<   key "label"
    script <-bhScript-< codecWith ctx
    build ButtonHandler name script

parseUnitScript :: Splices
                -> (ParsedValue, ParsedValue, ParsedValue)
                -> Coordinate
                -> Maybe String
                -> CodeString
                -> (forall env. EnvironmentProxy env
                    -> Either (SourceRange, String) (Code (UnitHandlerEnv env)))
parseUnitScript splices (pMaxIters, pMaxRadius, pMinRadius) _vc mpx (CodeString src)
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

        code <- left (errorLocation &&& unlines . pp) (parseCode env' splices src)

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

            code0 <- left (errorLocation &&& unlines . pp) (parseCode env'' splices src)
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
