{-# language OverloadedStrings #-}
module Actor.Event
  ( Event(..)
  , EventHandlers(..)
  , ParsedEventHandlers(..)
  , ComplexParsedEventHandlers(..)
  , convertComplexToRealEventHandlers
  , combineEventHandlers
  , noEventHandlers
  , noComplexEventHandlers
  , type SomeEventHandler
  , handleEvent
  , toEventHandlers
  , prependHandlerCode
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Value.Evaluator (HaskellTypeOfBinding)
import Language.Code
import Language.Value.Parser
import Language.Value.Typecheck (internalIterations, internalStuck)
import Language.Code.Parser
import Language.Code.InterpretIO
import Language.Draw

import Data.DynamicValue

import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Aeson

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

data EventHandlers env = EventHandlers
  { ehOnClick       :: Maybe (SomeEventHandler env '[ 'RealT, 'RealT ])
  , ehOnDoubleClick :: Maybe (SomeEventHandler env '[ 'RealT, 'RealT ])
  , ehOnDrag        :: Maybe (SomeEventHandler env '[ 'RealT, 'RealT, 'RealT, 'RealT ])
  , ehOnDragDone    :: Maybe (SomeEventHandler env '[ 'RealT, 'RealT, 'RealT, 'RealT ])
  , ehOnTimer       :: Map String (Int, SomeEventHandler env '[])
  , ehOnButton      :: Map String (SomeEventHandler env '[])
  , ehOnRefresh     :: Maybe (SomeEventHandler env '[])
  , ehOnActivated   :: Maybe (SomeEventHandler env '[])
  , ehOnDeactivated :: Maybe (SomeEventHandler env '[])
  }

data ParsedEventHandlers = ParsedEventHandlers
  { pehOnClick :: Maybe (String, String, Bool, String)
  , pehOnDoubleClick :: Maybe (String, String, Bool, String)
  , pehOnDrag :: Maybe (String, String, String, String, Bool, String)
  , pehOnDragDone :: Maybe (String, String, String, String, Bool, String)
  , pehOnTimer :: Map String (Int, String)
  , pehOnButton :: Map String String
  , pehOnRefresh :: Maybe String
  , pehOnActivated :: Maybe String
  , pehOnDeactivated :: Maybe String
  }
  deriving Show

data ComplexParsedEventHandlers = ComplexParsedEventHandlers
  { cpehOnClick :: Maybe (Either String String, Bool, String)
  , cpehOnDoubleClick :: Maybe (Either String String, Bool, String)
  , cpehOnDrag :: Maybe (Either String String, String, Bool, String)
  , cpehOnDragDone :: Maybe (Either String String, String, Bool, String)
  , cpehOnTimer :: Map String (Int, String)
  , cpehOnButton :: Map String String
  , cpehOnRefresh :: Maybe String
  , cpehOnActivated :: Maybe String
  , cpehOnDeactivated :: Maybe String
  }
  deriving Show

prependHandlerCode :: String -> ParsedEventHandlers -> ParsedEventHandlers
prependHandlerCode prefix p = ParsedEventHandlers
  { pehOnClick = fmap (prefix ++) <$> pehOnClick p
  , pehOnDoubleClick = fmap (prefix ++) <$> pehOnDoubleClick p
  , pehOnDrag = fmap (prefix ++) <$> pehOnDrag p
  , pehOnDragDone = fmap (prefix ++) <$> pehOnDragDone p
  , pehOnTimer = fmap (prefix ++) <$> pehOnTimer p
  , pehOnButton = (prefix ++) <$> pehOnButton p
  , pehOnRefresh = (prefix ++) <$> pehOnRefresh p
  , pehOnActivated = (prefix ++) <$> pehOnActivated p
  , pehOnDeactivated = (prefix ++) <$> pehOnDeactivated p
  }

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

toEventHandlers :: forall env
                 . EnvironmentProxy env
                -> Set String
                -> Splices
                -> ParsedEventHandlers
                -> Either String (Set String, EventHandlers env)
toEventHandlers env viewerVars splices ParsedEventHandlers{..} = fmap swap $ flip runStateT Set.empty $ do
  let parse :: EnvironmentProxy e -> String -> StateT (Set String) (Either String) (Code e)
      parse e i = do
        SomeSymbol (it :: Proxy iterations) <- pure (someSymbolVal internalIterations)
        SomeSymbol (stuck :: Proxy stuck) <- pure (someSymbolVal internalStuck)
        case lookupEnv' it e of
          Absent' pf -> withEnvironment e $ recallIsAbsent pf $ do
            let e' = declare @iterations IntegerType e
            case lookupEnv' stuck e' of
              Absent' pf' -> withEnvironment e' $ recallIsAbsent pf' $ do
                let e'' = declare @stuck BooleanType e'
                c <- lift $ first (`ppFullError` i) $ parseCode e'' splices i
                modify' (execState (usedVarsInCode c))
                pure (let_ 0 $ let_ (Const (Scalar BooleanType False)) $ c)
              Found'{} -> lift (Left "internal stuck status already defined")
          Found'{} -> lift (Left "internal iteration count already defined")

      allUpdatesOk, noUpdatesToViewerVar :: Context (K Bool) env
      allUpdatesOk = envToContext env (\_ _ -> True)
      noUpdatesToViewerVar = envToContext env (\n _ ->
                                                 not $ symbolVal n `Set.member` viewerVars)

      toHandler' :: forall args. (Bool, SomeEventHandler_ env args)
                -> SomeEventHandler env args
      toHandler' = \(canUpdate, h) ->
        if canUpdate
        then SomeEventHandler h allUpdatesOk
        else SomeEventHandler h noUpdatesToViewerVar

      toHandler :: forall args. SomeEventHandler_ env args -> SomeEventHandler env args
      toHandler h = SomeEventHandler h allUpdatesOk

      mmaybe :: forall m a b
              . Applicative m
             => Maybe a
             -> (a -> m b)
             -> m (Maybe b)
      mmaybe m f = maybe (pure Nothing) (fmap Just . f) m

  ehOnClick <- mmaybe (pehOnClick) $ fmap toHandler' . \(x, y, allowUpdate, code) -> fmap (allowUpdate,) $
    bind y RealType env $ \env' ->
    bind x RealType env' $ \env'' ->
        ( WithArg Proxy RealType . WithArg Proxy RealType . WithNoArgs )
          <$> parse env'' code

  ehOnDoubleClick <- mmaybe (pehOnDoubleClick) $ fmap toHandler' . \(x, y, allowUpdate, code) -> fmap (allowUpdate,) $
    bind y RealType env $ \env' ->
    bind x RealType env' $ \env'' ->
        ( WithArg Proxy RealType . WithArg Proxy RealType . WithNoArgs )
          <$> parse env'' code

  ehOnDrag <- mmaybe (pehOnDrag) $ fmap toHandler' . \(x1, y1, x2, y2, allowUpdate, code) -> fmap (allowUpdate,) $
    bind y2 RealType env  $ \env1 ->
    bind x2 RealType env1 $ \env2 ->
    bind y1 RealType env2 $ \env3 ->
    bind x1 RealType env3 $ \env4 ->
    ( WithArg Proxy RealType . WithArg Proxy RealType
      . WithArg Proxy RealType . WithArg Proxy RealType . WithNoArgs )
          <$> parse env4 code

  ehOnDragDone <- mmaybe (pehOnDragDone) $ fmap toHandler' . \(x1, y1, x2, y2, allowUpdate, code) -> fmap (allowUpdate,) $
    bind y2 RealType env  $ \env1 ->
    bind x2 RealType env1 $ \env2 ->
    bind y1 RealType env2 $ \env3 ->
    bind x1 RealType env3 $ \env4 ->
    ( WithArg Proxy RealType . WithArg Proxy RealType
      . WithArg Proxy RealType . WithArg Proxy RealType . WithNoArgs )
          <$> parse env4 code

  ehOnTimer <-
    (traverse (\(ms, code) -> (ms,) . (toHandler . WithNoArgs) <$> parse env code)
      pehOnTimer)

  ehOnRefresh <- mmaybe (pehOnRefresh) (fmap (toHandler . WithNoArgs) . parse env)

  ehOnActivated <- mmaybe (pehOnActivated) (fmap (toHandler . WithNoArgs) . parse env)

  ehOnDeactivated <- mmaybe (pehOnDeactivated) (fmap (toHandler . WithNoArgs) . parse env)

  ehOnButton <- traverse (fmap (toHandler . WithNoArgs) . parse env) pehOnButton

  pure EventHandlers{..}


bind :: String
     -> TypeProxy ty
     -> EnvironmentProxy env
     -> (forall name. (KnownSymbol name, NotPresent name env)
        => EnvironmentProxy ( '(name, ty) ': env) -> StateT s (Either String) t)
     -> StateT s (Either String) t
bind nameStr ty env k = case someSymbolVal nameStr of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' proof -> recallIsAbsent proof (k (bindNameEnv name ty proof env))
    _ -> lift $ Left (symbolVal name <> " is defined twice")

noEventHandlers :: ParsedEventHandlers
noEventHandlers = ParsedEventHandlers Nothing Nothing Nothing Nothing Map.empty Map.empty Nothing Nothing Nothing

noComplexEventHandlers :: ComplexParsedEventHandlers
noComplexEventHandlers = ComplexParsedEventHandlers Nothing Nothing Nothing Nothing Map.empty Map.empty Nothing Nothing Nothing

convertComplexToRealEventHandlers ::
  ComplexParsedEventHandlers -> ParsedEventHandlers

convertComplexToRealEventHandlers ComplexParsedEventHandlers{..}
    = ParsedEventHandlers{..}
  where

    initOrSet :: Either String String -> String -> String -> String
    initOrSet lr x y = concat $ case lr of
      Left  v -> [v, " <- ", x, " + i ", y, "\n"]
      Right v -> [v, " : C <- ", x, " + i ", y, "\n"]

    cplx :: (Either String String, Bool, String)
         -> (String, String, Bool, String)
    cplx (z, mutable, code) =
      let zre = "INTERNAL__" ++ either id id z ++ "__re"
          zim = "INTERNAL__" ++ either id id z ++ "__im"
          code' = concat
            [ initOrSet z zre zim
            , code ]
      in (zre, zim, mutable, code')

    cplx2 :: (Either String String, String, Bool, String)
          -> (String, String, String, String, Bool, String)
    cplx2 (z, w, mutable, code) =
      let zre = "INTERNAL__" ++ either id id z ++ "__re"
          zim = "INTERNAL__" ++ either id id z ++ "__im"
          wre = "INTERNAL__" ++ w ++ "__re"
          wim = "INTERNAL__" ++ w ++ "__im"
          code' = concat
            [ initOrSet z zre zim
            , initOrSet (Right w) wre wim
            , code ]
      in (zre, zim, wre, wim, mutable, code')

    pehOnClick = cplx <$> cpehOnClick
    pehOnDoubleClick = cplx <$> cpehOnDoubleClick
    pehOnDrag = cplx2 <$> cpehOnDrag
    pehOnDragDone = cplx2 <$> cpehOnDragDone
    pehOnTimer = cpehOnTimer
    pehOnRefresh = cpehOnRefresh
    pehOnActivated = cpehOnActivated
    pehOnDeactivated = cpehOnDeactivated
    pehOnButton = cpehOnButton

instance FromJSON (String -> String -> ParsedEventHandlers) where
  parseJSON = withObject "event handler" $ \o -> do
    let handler = noEventHandlers
    event :: String <- o .: "event"
    case event of
      "click" -> do
        xVar <- o .:? "x-coord"
        yVar <- o .:? "y-coord"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coords" .!= False
        pure (\x y -> handler { pehOnClick = Just (fromMaybe x xVar, fromMaybe y yVar, allowUpdate, code) })

      "click-or-drag" -> do
        xVar <- o .:? "x-coord"
        yVar <- o .:? "y-coord"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coords" .!= False
        let x0Var = "INTERNAL__drag_x_start"
            y0Var = "INTERNAL__drag_y_start"
        pure (\x y -> handler { pehOnClick = Just (fromMaybe x xVar, fromMaybe y yVar, allowUpdate, code)
                              , pehOnDrag  = Just (fromMaybe x xVar, fromMaybe y yVar, x0Var, y0Var, allowUpdate, code)
                              , pehOnDragDone = Just (fromMaybe x xVar, fromMaybe y yVar, x0Var, y0Var, allowUpdate, code)
                              })

      "double-click" -> do
        xVar <- o .:? "x-coord"
        yVar <- o .:? "y-coord"
        allowUpdate <- o .:? "can-update-viewer-coords" .!= False
        code <- o .: "code"
        pure (\x y -> handler { pehOnDoubleClick = Just (fromMaybe x xVar, fromMaybe y yVar, allowUpdate, code) })

      "drag" -> do
        xVar <- o .:? "x-coord"
        yVar <- o .:? "y-coord"
        x0Var <- o .:? "x-start" .!= "INTERNAL__drag_x_start"
        y0Var <- o .:? "y-start" .!= "INTERNAL__drag_y_start"
        allowUpdate <- o .:? "can-update-viewer-coords" .!= False
        code <- o .: "code"
        pure (\x y -> handler { pehOnDrag = Just (fromMaybe x xVar, fromMaybe y yVar, x0Var, y0Var, allowUpdate, code) })

      "drag-finished" -> do
        xVar <- o .:? "x-coord"
        yVar <- o .:? "y-coord"
        x0Var <- o .:? "x-start" .!= "INTERNAL__drag_x_start"
        y0Var <- o .:? "y-start" .!= "INTERNAL__drag_y_start"
        allowUpdate <- o .:? "can-update-viewer-coords" .!= False
        code <- o .: "code"
        pure (\x y -> handler { pehOnDragDone = Just (fromMaybe x xVar, fromMaybe y yVar, x0Var, y0Var, allowUpdate, code) })

      "timer" -> do
        name <- o .: "name"
        interval <- o .: "interval"
        code <- o .: "code"
        pure (\_ _ -> handler { pehOnTimer = Map.singleton name (interval, code) })

      "refresh" -> do
        code <- o .: "code"
        pure (\_ _ -> handler { pehOnRefresh = Just code })

      "activated" -> do
        code <- o .: "code"
        pure (\_ _ -> handler { pehOnActivated = Just code })

      "deactivated" -> do
        code <- o .: "code"
        pure (\_ _ -> handler { pehOnDeactivated = Just code })

      "button" -> do
        btn <- o .: "label"
        code <- o .: "code"
        pure (\_ _ -> handler { pehOnButton = Map.singleton btn code })

      etc -> fail ("unknown event `" ++ etc ++ "`")

instance FromJSON (String -> ComplexParsedEventHandlers) where
  parseJSON = withObject "event handler" $ \o -> do
    let handler = noComplexEventHandlers
        lr x Nothing  = Left x
        lr _ (Just y) = Right y
    event <- o .: "event"
    case event of
      "click" -> do
        zVar <- o .:? "coord"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coord" .!= False
        pure (\z -> handler { cpehOnClick = Just (lr z zVar, allowUpdate, code) })

      "click-or-drag" -> do
        zVar <- o .:? "coord"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coord" .!= False
        let z0Var = "INTERNAL__drag_start"
        pure (\z -> handler { cpehOnClick = Just (lr z zVar, allowUpdate, code)
                            , cpehOnDrag = Just (lr z zVar, z0Var, allowUpdate, code)
                            , cpehOnDragDone = Just (lr z zVar, z0Var, allowUpdate, code)
                            })

      "double-click" -> do
        zVar <- o .:? "coord"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coord" .!= False
        pure (\z -> handler { cpehOnDoubleClick = Just (lr z zVar, allowUpdate, code) })

      "drag" -> do
        zVar <- o .:? "coord"
        z0Var <- o .:? "start" .!= "INTERNAL__drag_start"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coord" .!= False
        pure (\z -> handler { cpehOnDrag = Just (lr z zVar, z0Var, allowUpdate, code) })

      "drag-finished" -> do
        zVar <- o .:? "coord"
        z0Var <- o .:? "start" .!= "INTERNAL__drag_start"
        code <- o .: "code"
        allowUpdate <- o .:? "can-update-viewer-coord" .!= False
        pure (\z -> handler { cpehOnDragDone = Just (lr z zVar, z0Var, allowUpdate, code) })

      "timer" -> do
        name <- o .: "name"
        interval <- o .: "interval"
        code <- o .: "code"
        pure (\_ -> handler { cpehOnTimer = Map.singleton name (interval, code) })

      "refresh" -> do
        code <- o .: "code"
        pure (\_ -> handler { cpehOnRefresh = Just code })

      "activated" -> do
        code <- o .: "code"
        pure (\_ -> handler { cpehOnActivated = Just code })

      "deactivated" -> do
        code <- o .: "code"
        pure (\_ -> handler { cpehOnDeactivated = Just code })

      "button" -> do
        btn <- o .: "label"
        code <- o .: "code"
        pure (\_ -> handler { cpehOnButton = Map.singleton btn code })

      etc -> fail ("unknown event `" ++ etc ++ "`")

combineEventHandlers :: Either String ParsedEventHandlers
                     -> ParsedEventHandlers
                     -> Either String ParsedEventHandlers
combineEventHandlers e@(Left _) _ = e
combineEventHandlers (Right lhs) rhs = do
  let bad name = Left ("more than one handler for the `"
                      ++ name ++ "` event")
      combine :: forall a
               . (ParsedEventHandlers -> Maybe a)
              -> String
              -> Either String (Maybe a)
      combine getter name = case (getter rhs, getter lhs) of
        (Nothing, Nothing) -> Right Nothing
        (Nothing, x)       -> Right x
        (x, Nothing)       -> Right x
        (Just _, Just _)   -> bad name

      combineTimers = sequence (Map.unionWithKey repeatedTimer
                                (pure <$> pehOnTimer lhs)
                                (pure <$> pehOnTimer rhs))
      repeatedTimer = (\k _ _ -> bad ("timer " ++ k))

      combineButtons = sequence (Map.unionWithKey repeatedButton
                                (pure <$> pehOnButton lhs)
                                (pure <$> pehOnButton rhs))
      repeatedButton = (\k _ _ -> bad ("button " ++ k))

  ParsedEventHandlers
    <$> combine pehOnClick "click"
    <*> combine pehOnDoubleClick "double-click"
    <*> combine pehOnDrag "drag"
    <*> combine pehOnDragDone "drag-finished"
    <*> combineTimers
    <*> combineButtons
    <*> combine pehOnRefresh "refresh"
    <*> combine pehOnActivated "activated"
    <*> combine pehOnDeactivated "deactivated"

handleEvent :: forall env
             . Context Variable_ env
            -> Bool
            -> DrawHandler ScalarIORefM
            -> EventHandlers env
            -> Event
            -> Maybe (IO ())
handleEvent ctx refreshCanUpdate draw EventHandlers{..} =
  let run :: forall args
           . Maybe (SomeEventHandler env args)
          -> ArgList args
          -> Maybe (IO ())
      run mh args = runEventHandler True ctx draw <$> mh <*> pure args
  in \case
    Click (x, y) ->
      run ehOnClick (Arg y $ Arg x $ EndOfArgs)
    DoubleClick (x, y) ->
      run ehOnDoubleClick (Arg y $ Arg x $ EndOfArgs)
    Drag (x1,y1) (x2, y2) ->
      run ehOnDrag (Arg y2 $ Arg x2 $ Arg y1 $ Arg x1 $ EndOfArgs)
    DragDone (x1,y1) (x2, y2) ->
      run ehOnDragDone (Arg y2 $ Arg x2 $ Arg y1 $ Arg x1 $ EndOfArgs)
    Timer t ->
      run (snd <$> Map.lookup t ehOnTimer) EndOfArgs
    Refresh ->
      runEventHandler refreshCanUpdate ctx draw <$> ehOnRefresh <*> pure EndOfArgs
    Activated -> run ehOnActivated EndOfArgs
    Deactivated -> run ehOnDeactivated EndOfArgs
    ButtonPressed name -> run (Map.lookup name ehOnButton) EndOfArgs

data SomeEventHandler_ env args where
  WithNoArgs :: forall env
              . Code env
             -> SomeEventHandler_ env '[]

  WithArg :: forall name ty env args
           . (KnownSymbol name, NotPresent name env)
          => Proxy name
          -> TypeProxy ty
          -> SomeEventHandler_ ( '(name, ty) ': env) args
          -> SomeEventHandler_ env (ty ': args)

data SomeEventHandler env args = SomeEventHandler
  { theEventHandler :: SomeEventHandler_ env args
  , mutableArgs :: Context (K Bool) env }

data ArgList (args :: [FSType]) where
  EndOfArgs :: ArgList '[]
  Arg :: forall ty args
       . HaskellType ty
      -> ArgList args
      -> ArgList (ty ': args)

-- runEvt :: Handlers (HandlerEffects env0) ScalarIORefM
--        -> Context IORefTypeOfBinding env
--        -> SomeEventHandler env0 env args
--        -> ArgList args
--       -> IO ()
runEvt :: DrawHandler ScalarIORefM
       -> Context IORefTypeOfBinding env
       -> SomeEventHandler_ env args
       -> ArgList args
       -> IO ()
runEvt draw ctx eh args = case eh of
  WithNoArgs code ->
    void (runStateT (interpretToIO draw code) ctx)
  WithArg name ty eh' -> case args of
    Arg arg args' -> do
      ref <- newIORef arg
      let ctx' = Bind name ty ref ctx
      runEvt draw ctx' eh' args'


runEventHandler :: forall env args
                 . Bool
                -> Context Variable_ env
                -> DrawHandler ScalarIORefM
                -> SomeEventHandler env args
                -> ArgList args
                -> IO ()
runEventHandler allowUpdates ctx draw SomeEventHandler{..} args = do

  -- Copy the current environment into a bunch of IORefs
  iorefs :: Context IORefTypeOfBinding env <-
    mapContextM (\_ _ d -> getDynamic d >>= newIORef) ctx

  -- Create the initial variable bindings
  inValues :: Context HaskellTypeOfBinding env <-
    mapContextM (\_ _ -> readIORef) iorefs

  -- Run the code and then read values back from the `iorefs`
  runEvt draw iorefs theEventHandler args

  outValues :: Context HaskellTypeOfBinding env <-
    mapContextM (\_ _ -> readIORef) iorefs

  -- Find values that were updated by an output effect, and
  -- update the corresponding dynamic values
  when allowUpdates $ do
    let finalCtx :: Context ((HaskellTypeOfBinding :**: HaskellTypeOfBinding :**: K Bool)
                                :**: Variable_) env
        finalCtx = zipContext (zipContext (zipContext inValues outValues) mutableArgs) ctx
    fromContextM_ (\_ ty (((old, new), canUpdate), v) ->
                     if Scalar ty old == Scalar ty new || not canUpdate
                     then pure ()
                     else void (setValue' v new))
                  finalCtx

data K :: Type -> Symbol -> FSType -> Exp Type
type instance Eval (K t _ _) = t
