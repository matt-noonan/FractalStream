{-# language IncoherentInstances #-}
module UI.WxWidgets
  ( WX
  -- Settings widget
  , SettingsWidget(..)
  , readSetting
  -- Viewer widget
  , ViewerWidget(..)
  ) where

import Data.IORef
import Control.Monad

import UI
import Actor
import Actor.Settings
import Actor.Viewer
import Actor.Tool
import qualified Event as FS
import Language.Type
import Language.Code hiding (set, get)
import Language.Effect.Draw
import Language.Effect.Provide
import Language.Effect.Render
import Language.Value.Evaluator (ScalarTypeOfBinding)
import Language.Code.InterpretIO
import Language.Code.Simulator

import Control.Monad.State hiding (get)
import qualified Control.Monad.State as State
import Data.Traversable (for)

import Graphics.UI.WX hiding (when, tool)
import Data.Maybe (fromMaybe)
import qualified Data.Color as FS

import Fcf (Exp, Eval)
import GHC.TypeLits

----------------------------------------------------------------
-- The WxWidgets UI tag
-- WxHaskell ain't fancy. It just needs good old IO.
----------------------------------------------------------------

data WX :: UI
type instance UIMonad WX = IO

----------------------------------------------------------------
-- Event handler type for Actors
----------------------------------------------------------------

attachActor :: Frame () -> UIActor WX -> IO ()
attachActor f (UIActor handler) = do
  set f [ on mouse := \case
            MouseMotion p modifiers
              | isNoShiftAltControlDown modifiers -> do
                  void $ maybe (\_ _ -> pure ()) toFunction (handler FS.Motion)
                           (fromIntegral $ pointX p)
                           (fromIntegral $ pointY p)
            _ -> pure ()
        ]

----------------------------------------------------------------
-- Settings widget
----------------------------------------------------------------

data ScalarIORef :: Symbol -> Type -> Exp *
type instance Eval (ScalarIORef name t) = (Maybe String, IORef (ScalarType t))

data SettingsWidget (env :: Environment) = SettingsWidget
  { settingsDialog :: Dialog ()
  , settingsRefs   :: Context ScalarIORef env
  , settingsActor  :: UIActor WX
  }

readSetting :: forall name t env
             . (KnownSymbol name, KnownType t)
            => SettingsWidget env
            -> NameIsPresent name t env
            -> Proxy name
            -> IO (ScalarType t)
readSetting sw pf _name = readIORef (snd $ getBinding (settingsRefs sw) pf)


makeSettings :: Context (Pure2 Setting) env -> IO (Context ScalarIORef env)
makeSettings = mapContextM $ \_ _ (Setting _ (Scalar _ v) mn) -> do
    ref <- newIORef v
    pure (fst <$> mn, ref)

instance ToUI WX (Settings env '[]) where
  -- Require a reference to the parent frame when creating the settings widget
  type CreationContext WX (Settings env '[]) = Frame ()

  -- Resulting UI object
  type UIObject WX (Settings env '[]) = SettingsWidget env

  -- Construct a SettingsWidget
  toUI Settings{..} parentFrame = do
    settingsDialog <- dialog parentFrame
      [ text := settingsTitle
      , resizeable := True
      , on resize := propagateEvent ]

    settingsRefs <- makeSettings settingsList
    let addSetting :: forall name t. Proxy name -> TypeProxy t -> Setting name t -> IO [Layout]
        addSetting _ _ (Setting _ (Scalar ty v) mn) = do
          let name = fromMaybe (showType ty) (fst <$> mn)
          ctrl <- case ty of
                  -- Represent boolean settings with a checkbox
                  BooleanType -> widget <$>
                      checkBox settingsDialog
                        [ checkable := True
                        , checked := v ]
                  -- Color picker, triggered by a colored button
                  ColorType -> widget <$>
                      button settingsDialog
                        [ text := showValue ty v
                        , on command := do
                            _ <- colorDialog settingsDialog (toWxColor v)
                            pure ()
                        ]
                  -- Represent other settings with a text entry box
                  _ -> widget <$> textEntry settingsDialog
                                    [ text := showValue ty v ]

          pure [label name, hfill ctrl]
    widgets <- case settingsList of
      EmptyContext -> (:[]) . (:[]) . widget <$> button settingsDialog [ text := "FIXME" ]
      _ -> fromContextM addSetting settingsList
    set settingsDialog
      [ layout := margin 10 (grid 10 10 widgets)
      , visible := True ]

    -- Build the 'onChanged' handler
    settingsActor <- do
      let refs = mapContext (\_ _ -> snd) settingsRefs
          build :: forall args
                 . Code '[Provide env] args 'VoidT
                -> IO (Context ScalarTypeOfBinding args -> IO ())
          build code = pure $ \ctx -> do
            let prog = interpretedInIO (provide refs NoHandler) code
            ctx' <- mapContextM (\_ _ -> newIORef) ctx
            prog ctx'
      onChangedHandler <- for onChanged build
      pure (UIActor $ \case
               FS.Update -> onChangedHandler
               _         -> Nothing)

    pure SettingsWidget{..}

toWxColor :: FS.Color -> Color
toWxColor c =
  let (r,g,b) = FS.colorToRGB c
  in rgb r g b

type DrawScalarIORefM = ScalarIORefMWith [DrawCommand]

provide :: forall env effs
         . Context IORefTypeOfBinding env
        -> Handlers effs ScalarIORefM
        -> Handlers (Provide env ': effs) ScalarIORefM
provide providedCtx = Handler (Handle Proxy handle)
  where
    handle :: forall e t
            . EnvironmentProxy e
           -> TypeProxy t
           -> Provide env ScalarIORefM '(e,t)
           -> StateT (Context IORefTypeOfBinding e) IO (ScalarType t)
    handle _ _ (Provide _ _ _ code) = do
      ctx <- State.get
      let ctx' = contextAppend ctx providedCtx
      lift (evalStateT code ctx')

provideD :: forall env effs
         . Context IORefTypeOfBinding env
        -> Handlers effs DrawScalarIORefM
        -> Handlers (Provide env ': effs) DrawScalarIORefM
provideD providedCtx = Handler (Handle Proxy handle)
  where
    handle :: forall e t
            . EnvironmentProxy e
           -> TypeProxy t
           -> Provide env DrawScalarIORefM '(e,t)
           -> StateT (Context IORefTypeOfBinding e, [DrawCommand]) IO (ScalarType t)
    handle _ _ (Provide _ _ _ code) = do
      (ctx, cmds) <- State.get
      let ctx' = contextAppend ctx providedCtx
      lift (evalStateT code (ctx', cmds))

drawHandler :: forall effs
             . Handlers effs DrawScalarIORefM
            -> Handlers (Draw ': effs) DrawScalarIORefM
drawHandler = Handler (Handle Proxy handle)
  where
    emit :: DrawCommand -> StateT (Context IORefTypeOfBinding e, [DrawCommand]) IO ()
    emit cmd = modify' (\(ctx, cmds) -> (ctx, cmd : cmds))

    handle :: forall e t
            . EnvironmentProxy e
           -> TypeProxy t
           -> Draw (ScalarIORefMWith [DrawCommand]) '(e,t)
           -> StateT (Context IORefTypeOfBinding e, [DrawCommand]) IO (ScalarType t)
    handle _ _ = \case
      DrawPoint _env pv -> do
        p <- eval pv
        emit (DrawPoint EmptyEnvProxy p)
      DrawCircle _env doFill rv pv -> do
        r    <- eval rv
        p    <- eval pv
        emit (DrawCircle EmptyEnvProxy doFill r p)
      DrawLine _env fromv tov -> do
        from <- eval fromv
        to   <- eval tov
        emit (DrawLine EmptyEnvProxy from to)
      DrawRect _env doFill fromv tov -> do
        from <- eval fromv
        to   <- eval tov
        emit (DrawRect EmptyEnvProxy doFill from to)
      SetStroke _env cv -> do
        c <- eval cv
        emit (SetStroke EmptyEnvProxy c)
      SetFill _env cv -> do
        c <- eval cv
        emit (SetStroke EmptyEnvProxy c)
      Clear _env -> emit (Clear EmptyEnvProxy)

render :: forall e t
        . (forall env
             . Code '[] env 'ColorT
            -> IO (Context ScalarTypeOfBinding env -> IO FS.Color))
       -> EnvironmentProxy e
       -> TypeProxy t
       -> Render ScalarIORefM '(e,t)
       -> StateT (Context IORefTypeOfBinding e) IO (ScalarType t)
render _compileCode _ _ = \case
  Render _ _ _ _ _ _dim _corner _dz _actionCode -> lift $ do
    putStrLn "render"
    pure 42

  HaltRender _ _bitmap -> lift (putStrLn "halt")
  Blit _ _bitmap _corner _scale _alpha -> lift (putStrLn "blit")
  ClearTo _ _c -> lift (putStrLn "clear")

interpretedInIO :: Handlers effs ScalarIORefM
                -> Code effs env t
                -> Context IORefTypeOfBinding env
                -> IO (ScalarType t)
interpretedInIO handlers code = evalStateT (interpretToIO handlers code)

interpretedInIO_ :: Handlers effs (ScalarIORefMWith s)
                 -> Code effs env t
                 -> s
                 -> Context IORefTypeOfBinding env
                 -> IO (ScalarType t, s)
interpretedInIO_ handlers code s =
  fmap (fmap snd) . runStateT (interpretToIO_ handlers code) . (,s)


----------------------------------------------------------------
-- Tools widget
----------------------------------------------------------------

data ToolWidget where
  ToolWidget :: forall env.
    { toolSettingsWidget :: SettingsWidget env
    , toolParentFrame :: Frame ()
    , toolActor :: UIActor WX
    } -> ToolWidget

instance ToUI WX (Tool '[Draw]) where
  -- We need to know the parent frame, where the tool's
  -- events and drawing commands will go to.
  type CreationContext WX (Tool '[Draw]) = Frame ()

  -- Resulting UI object
  type UIObject WX (Tool '[Draw]) = ToolWidget

  -- Construct a ToolWidget
  toUI Tool{..} toolParentFrame = do
    toolSettingsWidget :: SettingsWidget env <- toUI @WX toolSettings toolParentFrame
    set (settingsDialog toolSettingsWidget) [ visible := False ]

    let refs = mapContext (\_ _ -> snd) (settingsRefs toolSettingsWidget)
        provideEffect = provideD refs
        build :: forall args
               . Code '[Provide env, Draw] args 'VoidT
              -> IO (Context ScalarTypeOfBinding args -> IO ())
        build code = pure $ \ctx -> do
          let prog = interpretedInIO_ (provideEffect (drawHandler NoHandler)) code []
          ctx' <- mapContextM (\_ _ -> newIORef) ctx
          (result, draws) <- prog ctx'
          putStrLn ("saw " ++ show (length draws) ++ " draw commands")
          pure result

    onClickHandler     <- for onClick (build @FS.PosEnv)
    onMouseDownHandler <- for onMouseDown (build @FS.PosEnv)
    onMouseUpHandler   <- for onMouseUp (build @FS.PosEnv)
    onMotionHandler    <- for onMotion (build @FS.PosEnv)
    onDragHandler      <- for onDrag (build @FS.DragEnv)

    let toolActor = UIActor $ \evt -> case evt of
          FS.Click     -> onClickHandler
          FS.MouseDown -> onMouseDownHandler
          FS.MouseUp   -> onMouseUpHandler
          FS.Motion    -> onMotionHandler
          FS.Drag      -> onDragHandler
          _            -> Nothing
    pure ToolWidget{..}

----------------------------------------------------------------
-- Viewer widget
----------------------------------------------------------------

data ViewerWidget where
  ViewerWidget :: forall env.
    { settingsWidget :: SettingsWidget env
    , viewerFrame    :: Frame ()
    , viewerActor    :: UIActor WX
    } -> ViewerWidget

instance ToUI WX Viewer where
  -- We don't need any extra data to construct a viewer
  type CreationContext WX Viewer = ()

  -- Resulting UI object
  type UIObject WX Viewer = ViewerWidget

  -- Construct a ViewerWidget
  toUI Viewer{..} _ = do
    -- Make the frame
    viewerFrame <- frame [ text := "Viewer"
                         , clientSize := sz 512 512 ]
    settingsWidget :: SettingsWidget settingsEnv <- toUI @WX viewerSettings viewerFrame
    set (settingsDialog settingsWidget) [ visible := False ]

    -- Build the Tools menu
    toolsMenu <- menuPane [text := "&Tools"]
    case viewerTools of
      [] -> void $ menuItem toolsMenu [ text := "None", enabled := False ]
      tools -> forM_ @_ @_ @_ @() tools $ \tool -> do
        ToolWidget{..} <- toUI @WX tool viewerFrame
        set (settingsDialog toolSettingsWidget) [ visible := False ]
        void $ menuItem toolsMenu
          [ text := toolName tool
          , enabled := True
          , on command := do
              set (settingsDialog toolSettingsWidget) [ visible := True ]
              attachActor viewerFrame toolActor
          ]

    let refs = mapContext (\_ _ -> snd) (settingsRefs settingsWidget)
        provideEffect = provide refs
        build :: forall args
               . Code '[Provide settingsEnv, Render] args 'VoidT
              -> IO (Context ScalarTypeOfBinding args -> IO ())
        build code = pure $ \ctx -> do
          let prog = interpretedInIO (provideEffect (Handler (Handle Proxy (render (\c -> pure (pure . evalState (simulate NoHandler c) . (,()))))) NoHandler)) code
          ctx' <- mapContextM (\_ _ -> newIORef) ctx
          prog ctx'
          -- pure . maybe (pure (const True)) . (\c -> interpretedInIO (provideEffect _) c EmptyContext)
    viewerActor <- do
      onRefreshHandler <- for onRefresh (build @'[])
      onResizeHandler  <- for onResize (build @FS.ResizeEnv)
      onTimerHandler   <- for onTimer (build @'[])
      pure $ UIActor $ \case
          FS.Refresh -> onRefreshHandler
          FS.Resize  -> onResizeHandler
          FS.Timer   -> onTimerHandler
          _          -> Nothing

    -- Build the Viewer menu
    viewerMenu <- menuPane [text := "&Viewer"]
    menuItem viewerMenu
      [ text := "Settings..."
      , on command :=
        set (settingsDialog settingsWidget) [ visible := True ]
      ]
    menuLine viewerMenu
    menuItem viewerMenu
      [ text := "Copy view to clipboard"
      , on command := putStrLn "TODO"
      ]
    menuItem viewerMenu
      [ text := "Copy view to file..."
      , on command := putStrLn "TODO"
      ]

    -- onResizeTimer is a one-shot timer that fires 100ms after the
    -- frame has been resized. If another resize event comes in during
    -- that interval, the timer is reset to 100ms. When the timer fires,
    -- we kick off a new rendering task to build the contents of the
    -- window. Using a timer lets us avoid starting hundreds of rendering
    -- tasks while the user adjusts their window size.
    onResizeTimer <- timer viewerFrame [ interval := 100
                                       , enabled := False ]
    pendingResize <- variable [value := False]
    set onResizeTimer [ on command := do
                          set onResizeTimer [enabled := False] -- one-shot
                          needResize <- get pendingResize value
                          when needResize $ do
                            set pendingResize [value := False]
                            putStrLn "completed resize event"
                      ]
    set viewerFrame [ menuBar := [viewerMenu, toolsMenu]
                    , on resize := do
                        set onResizeTimer [enabled := False]
                        set pendingResize [value := True]
                        set onResizeTimer [enabled := True]
                        propagateEvent
                    ]

    _ <- button viewerFrame
              [ text := "Settings..."
              , on command := set (settingsDialog settingsWidget)
                                  [ visible := True ]
              ]
    pure ViewerWidget{..}
