{- |
Module       : Interpreter
Description  : Execute a parsed scenario against a loaded project

Runs a list of 'Scenario.Command's against an 'Ensemble': maintains the
"current viewer"\/"current tool" state 'select'\/'deselect' commands set,
resolves value expressions and routes synthetic 'Event's through exactly
the same machinery the live UI uses (@Actor.Ensemble.buildToolContext@
and a 'Viewer''s own @vBuildToolHandler@), and reports the first error
encountered, stopping there (scenarios don't have error recovery).

See @SCENARIO_LANGUAGE.md@ for what each command means.
-}
module Interpreter
  ( runScenario
  ) where

import FractalStream.Prelude

import Scenario (Command(..), Point(..), RGB)

import Actor.Ensemble (Ensemble, runEnsemble, buildToolContext, layoutToArgs)
import Actor.UI (UI(..))
import Actor.Viewer
  ( Backend, Viewer(..)
  , withRenderedViewerBuffer, snapshotToFile
  )
import Actor.Viewer.Overlay (compositeDrawLayers, compositeDrawLayersToFile)
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (Event(..), EventArgument_, EventArgument(..))
import Actor.Layout (Dimensions(..))

import Language.Environment
  ( SomeContext(..), SomeContext'(..), Context(..)
  , FoundBinding(..), lookupSomeContextByName
  )
import Language.Type (TypeProxy(RealType, ComplexType), HaskellType, withKnownType)
import Language.Value (Value)
import Language.Value.Typecheck (ParsedValue, atType)
import Language.Value.Evaluator (evaluate)
import Language.Typecheck (TC(..))

import Data.DynamicValue (getDynamic, setValue', dyn, source)

import Control.Exception (SomeException, try, displayException)

import Codec.Picture (Image, PixelRGBA8(..), imageWidth, imageHeight, pixelAt)

import qualified Data.Map as Map
import Data.IORef

-- | Everything the interpreter threads from one command to the next.
data InterpState = InterpState
  { isViewers       :: Map String Viewer
  , isEnsembleCtx   :: SomeContext EventArgument_
  , isCurrentViewer :: Maybe String
  , isCurrentTool   :: Maybe (String, Double -> Event -> Maybe (IO ()))
    -- ^ the selected tool's name, and its already-built event dispatcher
    -- (built once on selection, not re-built per event -- matters more
    -- than it might look, since building it JIT-compiles the tool's
    -- script under the LLVM backend).
  }

-- | Run every command in order against the given project, stopping at
-- (and reporting) the first failure.
runScenario :: Backend -> Ensemble -> [Command] -> IO (Either String ())
runScenario backend ensemble cmds = do
  viewersRef <- newIORef Map.empty
  ctxRef     <- newIORef Nothing
  runEnsemble backend (collectingUI viewersRef ctxRef) ensemble
  viewers <- readIORef viewersRef
  readIORef ctxRef >>= \case
    Nothing  -> pure (Left "this project has no viewers")
    Just ctx -> go (InterpState viewers ctx Nothing Nothing) cmds
  where
    go _  []           = pure (Right ())
    go st (cmd : rest) = execCommand st cmd >>= \case
      Left err  -> pure (Left err)
      Right st' -> go st' rest

-- | A 'UI' that only collects each viewer (and the ensemble-wide context
-- every viewer is handed) as 'runEnsemble' constructs them, with no
-- actual GUI -- the same shape as @Main.snapshotUI@\/@DeepCheck.hs@'s
-- mock, generalized to keep every viewer instead of rendering one.
collectingUI :: IORef (Map String Viewer) -> IORef (Maybe (SomeContext EventArgument_)) -> UI
collectingUI viewersRef ctxRef = UI
  { newEnsemble = pure ()
  , makeLayout  = \_ _ _ -> pure (pure ())
  , makeViewer  = \_ _ _ ctx _ viewer -> do
      title <- getDynamic (vTitle viewer)
      modifyIORef' viewersRef (Map.insert title viewer)
      writeIORef ctxRef (Just ctx)
      pure (pure ())
  }

execCommand :: InterpState -> Command -> IO (Either String InterpState)
execCommand st = \case

  SelectViewer name -> case Map.lookup name (isViewers st) of
    Nothing -> pure (Left ("no viewer named " ++ show name))
    Just _  -> do
      deactivateCurrentTool st
      pure (Right st { isCurrentViewer = Just name, isCurrentTool = Nothing })

  SelectTool name -> withCurrentViewer st $ \viewer -> do
    findTool viewer name >>= \case
      Nothing -> pure (Left ("no tool named " ++ show name ++ " in the current viewer"))
      Just tool -> do
        deactivateCurrentTool st
        buildDispatcher (isEnsembleCtx st) viewer tool >>= \case
          Left err -> pure (Left ("couldn't select tool " ++ show name ++ ": " ++ err))
          Right dispatcher -> do
            fireOn viewer dispatcher Activated
            pure (Right st { isCurrentTool = Just (name, dispatcher) })

  DeselectTool -> do
    deactivateCurrentTool st
    pure (Right st { isCurrentTool = Nothing })

  SetParameter var pv -> case lookupSomeContextByName var (isEnsembleCtx st) of
    Nothing -> pure (Left ("no configuration parameter named " ++ show var))
    Just (FoundBinding ty arg) -> setArgument var ty arg pv >> pure (Right st)

  SetToolParameter var pv -> withCurrentTool st $ \viewer toolName ->
    findTool viewer toolName >>= \case
      Nothing -> pure (Left ("tool " ++ show toolName ++ " disappeared"))
      Just tool -> getDynamic (toolConfig tool) >>= \case
        Nothing -> pure (Left ("tool " ++ show toolName ++ " has no configuration panel of its own"))
        Just cfg -> layoutToArgs (coContents cfg) >>= \case
          SomeContext' (Left err) -> pure (Left err)
          SomeContext' (Right toolCtx) -> case lookupSomeContextByName var toolCtx of
            Nothing -> pure (Left ("tool " ++ show toolName ++ " has no parameter named " ++ show var))
            Just (FoundBinding ty arg) -> setArgument var ty arg pv >> pure (Right st)

  ResizeViewer w h -> withCurrentViewer st $ \viewer -> do
    setValue' (vSize viewer) (Dimensions (w, h))
    pure (Right st)

  SetViewerCenter pt -> withCurrentViewer st $ \viewer ->
    withResolved (resolvePoint pt) $ \(x, y) -> do
      setValue' (vCenter viewer) (x, y)
      pure (Right st)

  SetViewerPixelSize pv -> withCurrentViewer st $ \viewer ->
    withResolved (resolveReal pv) $ \px -> do
      setValue' (vPixelSize viewer) px
      pure (Right st)

  ClickAt pt -> withCurrentDispatcher st $ \viewer dispatcher ->
    withResolved (resolvePoint pt) $ \xy -> do
      fireOn viewer dispatcher (Click xy)
      pure (Right st)

  DoubleClickAt pt -> withCurrentDispatcher st $ \viewer dispatcher ->
    withResolved (resolvePoint pt) $ \xy -> do
      fireOn viewer dispatcher (DoubleClick xy)
      pure (Right st)

  DragFromTo p1 p2 -> withCurrentDispatcher st $ \viewer dispatcher ->
    withResolved ((,) <$> resolvePoint p1 <*> resolvePoint p2) $ \(xy1, xy2) -> do
      fireOn viewer dispatcher (Drag xy1 xy2)
      fireOn viewer dispatcher (DragDone xy1 xy2)
      pure (Right st)

  PressButton label -> withCurrentDispatcher st $ \viewer dispatcher -> do
    fireOn viewer dispatcher (ButtonPressed label)
    pure (Right st)

  TriggerRefresh -> withCurrentDispatcher st $ \viewer dispatcher -> do
    fireOn viewer dispatcher Refresh
    pure (Right st)

  TriggerTimer name -> withCurrentDispatcher st $ \viewer dispatcher -> do
    fireOn viewer dispatcher (Timer name)
    pure (Right st)

  WaitForViewer name
    | Map.member name (isViewers st) -> pure (Right st)
    | otherwise -> pure (Left ("no viewer named " ++ show name))

  CaptureViewer name path -> withNamedViewer st name $ \viewer -> do
    r <- withRenderedViewerBuffer viewer True $ \center pixelSize dims buf -> do
      layers <- vDrawCmds viewer
      compositeDrawLayersToFile center pixelSize layers dims buf path
    pure (either Left (const (Right st)) r)

  CaptureViewerBase name path -> withNamedViewer st name $ \viewer ->
    snapshotToFile viewer True path >>= \case
      Nothing  -> pure (Right st)
      Just err -> pure (Left err)

  AssertPixel name (px, py) expected mtol -> withNamedViewer st name $ \viewer -> do
    r <- withRenderedViewerBuffer viewer True $ \center pixelSize dims buf -> do
      layers <- vDrawCmds viewer
      compositeDrawLayers center pixelSize layers dims buf
    case r of
      Left err  -> pure (Left err)
      Right img -> pure (checkPixel name img (px, py) expected mtol st)

---------------------------------------------------------------------------
-- Current viewer / current tool plumbing
---------------------------------------------------------------------------

withCurrentViewer :: InterpState -> (Viewer -> IO (Either String InterpState)) -> IO (Either String InterpState)
withCurrentViewer st k = case isCurrentViewer st of
  Nothing    -> pure (Left "no viewer selected (use `select viewer \"...\"`)")
  Just vname -> k (isViewers st Map.! vname)

withCurrentTool :: InterpState -> (Viewer -> String -> IO (Either String InterpState)) -> IO (Either String InterpState)
withCurrentTool st k = case (isCurrentViewer st, isCurrentTool st) of
  (Just vname, Just (toolName, _)) -> k (isViewers st Map.! vname) toolName
  (Nothing, _) -> pure (Left "no viewer selected (use `select viewer \"...\"`)")
  (_, Nothing) -> pure (Left "no tool selected (use `select tool \"...\"`)")

withCurrentDispatcher
  :: InterpState
  -> (Viewer -> (Double -> Event -> Maybe (IO ())) -> IO (Either String InterpState))
  -> IO (Either String InterpState)
withCurrentDispatcher st k = case (isCurrentViewer st, isCurrentTool st) of
  (Just vname, Just (_, dispatcher)) -> k (isViewers st Map.! vname) dispatcher
  (Nothing, _) -> pure (Left "no viewer selected (use `select viewer \"...\"`)")
  (_, Nothing) -> pure (Left "no tool selected (use `select tool \"...\"`)")

withNamedViewer :: InterpState -> String -> (Viewer -> IO (Either String InterpState)) -> IO (Either String InterpState)
withNamedViewer st name k = case Map.lookup name (isViewers st) of
  Nothing     -> pure (Left ("no viewer named " ++ show name))
  Just viewer -> k viewer

deactivateCurrentTool :: InterpState -> IO ()
deactivateCurrentTool st = case (isCurrentViewer st, isCurrentTool st) of
  (Just vname, Just (_, dispatcher)) -> fireOn (isViewers st Map.! vname) dispatcher Deactivated
  _ -> pure ()

findTool :: Viewer -> String -> IO (Maybe Tool)
findTool viewer name = do
  tools <- getDynamic (vTools viewer)
  named <- mapM (\t -> (, t) <$> getDynamic (source (tiName (toolInfo t)))) tools
  pure (lookup name named)

-- | Build a tool's event dispatcher: its own configuration panel merged
-- with the ensemble-wide context, compiled (or interpreted) once via the
-- active backend's 'Actor.Viewer.vBuildToolHandler'.
-- | Builds a tool's dispatcher, catching any exception the backend
-- raises rather than letting it crash the whole run. This matters in
-- practice: compiling a tool's script is exactly where the LLVM backend
-- can hit an unsupported construct (e.g. it doesn't compile @write@
-- yet -- see "Language.Draw"), and it reports that via a plain Haskell
-- 'error' rather than through this very function's own
-- @Either String@ result, so without this a scenario selecting such a
-- tool would otherwise take the whole process down instead of failing
-- that one command cleanly.
buildDispatcher :: SomeContext EventArgument_ -> Viewer -> Tool -> IO (Either String (Double -> Event -> Maybe (IO ())))
buildDispatcher ensembleCtx viewer tool = safely $ do
  mcfg <- getDynamic (toolConfig tool)
  buildToolContext ensembleCtx mcfg >>= \case
    Left err -> pure (Left err)
    Right ctx -> do
      layer  <- getDynamic (toolDrawLayer tool)
      script <- getDynamic (dyn (toolScript tool))
      vBuildToolHandler viewer layer ctx script

-- | Run an action that's supposed to report failure via @Either String@,
-- but also catch any exception it raises instead and fold that into the
-- same channel -- backend code doesn't always honor that contract for
-- every failure (see 'buildDispatcher').
safely :: IO (Either String a) -> IO (Either String a)
safely action = try action >>= \case
  Left (e :: SomeException) -> pure (Left (displayException e))
  Right result              -> pure result

-- | Fire an event on a dispatcher, with the current viewer's pixel size
-- (every event handler gets this, regardless of event kind -- see
-- 'Actor.Event.makeEventHandler').
fireOn :: Viewer -> (Double -> Event -> Maybe (IO ())) -> Event -> IO ()
fireOn viewer dispatcher ev = do
  px <- getDynamic (vPixelSize viewer)
  sequence_ (dispatcher px ev)

---------------------------------------------------------------------------
-- Resolving value expressions
---------------------------------------------------------------------------

withResolved :: Either String a -> (a -> IO (Either String InterpState)) -> IO (Either String InterpState)
withResolved (Left err) _ = pure (Left err)
withResolved (Right x)  k = k x

-- | Resolve a (closed -- scenario-language values never reference project
-- variables) value expression at a known type.
resolveAtType :: forall t. TypeProxy t -> ParsedValue -> Either String (HaskellType t)
resolveAtType ty pv = withKnownType ty $ case (atType pv ty :: TC (Value '( '[], t))) of
  TC (Left e)  -> Left (unlines (pp e))
  TC (Right v) -> Right (evaluate v EmptyContext)

resolveReal :: ParsedValue -> Either String Double
resolveReal = resolveAtType RealType

resolveComplex :: ParsedValue -> Either String (Complex Double)
resolveComplex = resolveAtType ComplexType

resolvePoint :: Point -> Either String (Double, Double)
resolvePoint (CartesianPoint px py) = (,) <$> resolveReal px <*> resolveReal py
resolvePoint (ComplexPoint pz) = (\z -> (realPart z, imagPart z)) <$> resolveComplex pz

-- | Resolve a configuration variable's new value at its own declared
-- type, and set it.
setArgument :: String -> TypeProxy t -> EventArgument t -> ParsedValue -> IO (Either String ())
setArgument name ty arg pv = withKnownType ty $ case resolveAtType ty pv of
  Left err -> pure (Left ("couldn't set " ++ name ++ ": " ++ err))
  Right v  -> case argSetValue arg of
    Nothing     -> pure (Left (name ++ " cannot be set"))
    Just setter -> Right <$> setter ty v

---------------------------------------------------------------------------
-- Assertions
---------------------------------------------------------------------------

checkPixel :: String -> Image PixelRGBA8 -> (Int, Int) -> RGB -> Maybe Int -> InterpState -> Either String InterpState
checkPixel name img (px, py) (er, eg, eb) mtol st
  | px < 0 || py < 0 || px >= imageWidth img || py >= imageHeight img =
      Left ("pixel (" ++ show px ++ ", " ++ show py ++ ") is out of bounds for viewer " ++ show name)
  | ok = Right st
  | otherwise = Left $
      "assertion failed: viewer " ++ show name ++ " pixel (" ++ show px ++ ", " ++ show py ++
      ") is (" ++ show ar ++ ", " ++ show ag ++ ", " ++ show ab ++
      "), expected (" ++ show er ++ ", " ++ show eg ++ ", " ++ show eb ++ ")" ++
      maybe "" (\t -> " within " ++ show t) mtol
  where
    PixelRGBA8 ar ag ab _ = pixelAt img px py
    tol = fromMaybe 0 mtol
    close a e = abs (fromIntegral a - e) <= tol
    ok = close ar er && close ag eg && close ab eb
