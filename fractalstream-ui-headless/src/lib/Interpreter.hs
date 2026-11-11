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
  , runPreviewCommands
  ) where

import FractalStream.Prelude

import Scenario (Command(..), Point(..), RGB, MovieOptions(..), MovieAnimation(..))

import Actor.Movie
import Data.Movie (MovieFormat(..))
import System.FilePath (takeExtension)
import Data.Char (toLower)

import Actor.SavedTables (readSavedTables, applySavedTables)
import Actor.Ensemble (Ensemble(..), runEnsemble, ensembleTables, ensembleCommonDefs, buildToolContext, layoutToArgs, ensembleFields, FieldOwner(..))
import Actor.Layout (Field(..))
import Actor.UI (UI(..))
import Actor.Viewer
  ( Backend, Viewer(..)
  , withRenderedViewerBuffer, snapshotToFile, switchView, centerViewOn, refreshOverlay
  , ViewerIssues, describeViewerIssues
  )
import Actor.Viewer.Overlay (compositeDrawLayers, compositeDrawLayersToFile)
import Actor.Viewer.Preview (renderPreview)
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (Event(..), EventArgument_, EventArgument(..), Coordinate(..),
                    ToolDispatcher, runHandlerNow)
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
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.IORef
import System.IO (hPutStrLn, stderr)

-- | Everything the interpreter threads from one command to the next.
data InterpState = InterpState
  { isViewers       :: Map String Viewer
  , isEnsembleCtx   :: SomeContext EventArgument_
  , isCurrentViewer :: Maybe String
  , isCurrentTool   :: Maybe (String, ToolDispatcher)
    -- ^ the selected tool's name, and its already-built event dispatcher
    -- (built once on selection, not re-built per event -- matters more
    -- than it might look, since building it JIT-compiles the tool's
    -- script under the LLVM backend).
  , isEnsemble      :: Ensemble
  , isPreviews      :: Maybe (IORef [ByteString])
    -- ^ Where @capture ... as preview@ puts its image, when running a
    -- project's @preview-commands@ ('runPreviewCommands'); 'Nothing'
    -- for an ordinary scenario, where that command is an error.
  }

-- | Run every command in order against the given project, stopping at
-- (and reporting) the first failure.
runScenario :: Backend -> Ensemble -> [Command] -> IO (Either String ())
runScenario backend ensemble = runCommands backend ensemble Nothing

-- | Run a project's @metadata: preview-commands:@, returning the image
-- its @capture ... as preview@ command made. It must make exactly one.
runPreviewCommands :: Backend -> Ensemble -> [Command] -> IO (Either String ByteString)
runPreviewCommands backend ensemble cmds = do
  previews <- newIORef []
  runCommands backend ensemble (Just previews) cmds >>= \case
    Left err -> pure (Left err)
    Right () -> readIORef previews >>= \case
      [png] -> pure (Right png)
      []    -> pure (Left "the preview commands never ran `capture viewer \"...\" as preview`")
      _     -> pure (Left "the preview commands ran `capture viewer \"...\" as preview` more than once")

runCommands :: Backend -> Ensemble -> Maybe (IORef [ByteString]) -> [Command] -> IO (Either String ())
runCommands backend ensemble previews cmds = do
  viewersRef <- newIORef Map.empty
  ctxRef     <- newIORef Nothing
  runEnsemble backend (collectingUI viewersRef ctxRef) ensemble
  viewers <- readIORef viewersRef
  readIORef ctxRef >>= \case
    Nothing  -> pure (Left "this project has no viewers")
    Just ctx -> go (InterpState viewers ctx Nothing Nothing ensemble previews) cmds
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

  -- A colorizer's settings are configuration variables with names no
  -- script can write (see "Actor.Colorizer"), one set per viewer.
  SetColorizerParameter var value -> withCurrentViewer st $ \viewer -> do
    title <- getDynamic (vTitle viewer)
    let hidden = "[colorizer] " ++ title ++ ": " ++ var
        noSuch = Left ("the colorizer of viewer " ++ show title ++ " has no parameter named " ++ show var)
    case value of
      Right pv -> case lookupSomeContextByName hidden (isEnsembleCtx st) of
        Nothing -> pure noSuch
        Just (FoundBinding ty arg) -> fmap (const st) <$> setArgument var ty arg pv
      Left txt -> do
        fields <- ensembleFields (isEnsemble st)
        case [ f | (ProjectConfig, f) <- fields, fieldName f == hidden ] of
          [] -> pure noSuch
          f : _ -> setFieldText f txt <&> \case
            Left err -> Left ("couldn't set " ++ var ++ ": " ++ err)
            Right () -> Right st

  ResizeViewer w h -> withCurrentViewer st $ \viewer -> do
    setValue' (vSize viewer) (Dimensions (w, h))
    pure (Right st)

  SetViewerCenter pt -> withCurrentViewer st $ \viewer ->
    withResolved (resolvePoint pt) $ \xy -> do
      -- (on the sphere, the globe turns to face it)
      centerViewOn viewer xy
      pure (Right st)

  SetViewerView kind -> withCurrentViewer st $ \viewer -> case vCoordinate viewer of
    RealCoordinates _ _ -> pure (Left "only a complex viewer (one with `var=`) can be shown on the sphere")
    ComplexCoordinate _ -> switchView viewer kind >> pure (Right st)

  SetViewerPixelSize pv -> withCurrentViewer st $ \viewer ->
    withResolved (resolveReal pv) $ \px -> do
      setValue' (vPixelSize viewer) px
      pure (Right st)

  SetViewerAspect pv -> withCurrentViewer st $ \viewer -> case vCoordinate viewer of
    ComplexCoordinate _ -> pure (Left "only a real viewer (one with `xvar=` and `yvar=`) has an \
                                      \aspect ratio: a complex viewer's pixels are square")
    RealCoordinates _ _ -> withResolved (resolveReal pv) $ \aspect ->
      if aspect > 0
        then setValue' (vAspect viewer) aspect >> pure (Right st)
        else pure (Left "a viewer's aspect ratio must be positive")

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

  LoadSavedTables mnames -> do
    props <- getDynamic (ensembleFrontendProperties (isEnsemble st))
    tables <- ensembleTables (isEnsemble st)
    case readSavedTables props of
      Left err -> pure (Left ("the project's saved `tables:` can't be read: " ++ err))
      Right saved -> case mnames of
        Nothing -> load saved
        Just names -> case [ n | n <- names, not (Map.member n saved) ] of
          [] -> load (Map.restrictKeys saved (Set.fromList names))
          missing -> pure (Left ("the project file has no saved " ++ plural missing "table" "tables"
                                 ++ " " ++ intercalate ", " (map (\n -> "`" ++ n ++ "`") missing)))
        where
          plural xs one many' = if length xs == 1 then one else many'
          load which = applySavedTables tables which <&> \case
            [] -> Right st
            problems -> Left (intercalate "; " problems)

  CaptureViewer name path -> withNamedViewer st name $ \viewer -> do
    r <- withRenderedViewerBuffer viewer True $ \view dims buf -> do
      refreshOverlay viewer
      layers <- vDrawCmds viewer
      compositeDrawLayersToFile view layers dims buf path
    traverse (warnIssues name . fst) r <&> (st <$)

  CaptureViewerBase name path -> withNamedViewer st name $ \viewer ->
    snapshotToFile viewer True path >>= traverse (warnIssues name) <&> (st <$)

  CaptureViewerAsPreview name baseOnly -> case isPreviews st of
    Nothing -> pure (Left "`capture viewer \"...\" as preview` can only be used in a project's \
                          \`metadata: preview-commands:`, run with `fsrun update-preview --project <project.fs>`")
    Just previews -> withNamedViewer st name $ \viewer ->
      renderPreview baseOnly viewer >>= \case
        Left err  -> pure (Left err)
        Right png -> do
          modifyIORef' previews (png :)
          pure (Right st)


  AssertPixel name (px, py) expected mtol -> withNamedViewer st name $ \viewer -> do
    r <- withRenderedViewerBuffer viewer True $ \view dims buf -> do
      refreshOverlay viewer
      layers <- vDrawCmds viewer
      compositeDrawLayers view layers dims buf
    case r of
      Left err  -> pure (Left err)
      Right (_, img) -> pure (checkPixel name img (px, py) expected mtol st)

  RecordMovie name path MovieOptions{..} animation -> withNamedViewer st name $ \viewer ->
    case map toLower (takeExtension path) of
      ext | Just fmt <- lookup ext [(".mov", QuickTimeMJPEG), (".avi", AviMJPEG)] -> do
        let settings = defaultMovieSettings
              { msFrames = moFrames, msFps = moFps, msLoop = moLoop
              , msWithDrawings = not moBase, msFormat = fmt }
        anim <- case animation of
          MovieScript script -> case moInset of
            Just _ -> pure (Left "an inset marks where along a path each frame is, so it needs a path (`moving ... along ...`)")
            Nothing -> ensembleCommonDefs (isEnsemble st) >>= \case
              Left err -> pure (Left err)
              Right defs -> fmap (, Nothing) <$> scriptAnimation defs (isEnsembleCtx st) script
          MoviePath var pts -> pure $ do
            path' <- mapM resolvePoint pts
            animate <- pathAnimation (isEnsembleCtx st) var moLoop path'
            inset <- forM moInset $ \(insetName, corner) -> case Map.lookup insetName (isViewers st) of
              Nothing -> Left ("no viewer named " ++ show insetName ++ " for the inset")
              Just v -> Right (MovieInset v (pathPoint moLoop path') corner)
            pure (animate, inset)
        case anim of
          Left err -> pure (Left ("couldn't record a movie of viewer " ++ show name ++ ": " ++ err))
          Right (animate, inset) -> recordMovie settings viewer inset animate (const (pure ())) path >>= \case
            Left err -> pure (Left ("couldn't record a movie of viewer " ++ show name ++ ": " ++ err))
            Right issues -> warnIssues name issues >> pure (Right st)
      _ -> pure (Left ("a movie's file name must end in .mov (QuickTime) or .avi, not " ++ show path))

-- | Warn (on stderr) about any issues a viewer's render ran into.
warnIssues :: String -> ViewerIssues -> IO ()
warnIssues name issues = forM_ (describeViewerIssues issues) $ \msg ->
  hPutStrLn stderr ("warning: viewer " ++ show name ++ ": " ++ msg)

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
  -> (Viewer -> ToolDispatcher -> IO (Either String InterpState))
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
buildDispatcher :: SomeContext EventArgument_ -> Viewer -> Tool -> IO (Either String ToolDispatcher)
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
fireOn :: Viewer -> ToolDispatcher -> Event -> IO ()
fireOn viewer dispatcher ev = do
  px <- getDynamic (vPixelSize viewer)
  aspect <- getDynamic (vAspect viewer)
  -- (run at once: a scenario has no one to show progress to, or stop it)
  mapM_ runHandlerNow (dispatcher (px, px * aspect) ev)

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
