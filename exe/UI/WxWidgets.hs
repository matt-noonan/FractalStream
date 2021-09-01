{-# language IncoherentInstances #-}
module UI.WxWidgets
  ( WX
  -- Settings widget
  , sampleSettings
  , SettingsWidget(..)
  , readSetting
  -- Viewer widget
  , ViewerWidget(..)
  , sampleViewer
  ) where

import Data.IORef
import Control.Monad

import UI
import Actor.Settings
import Actor.Viewer
import Language.Code hiding (set, get)

import Graphics.UI.WX hiding (when)
import Data.Maybe (fromMaybe)
import qualified Color.Color as FSColor

data WX :: UI
type instance UIMonad WX = IO

----------------------------------------------------------------
-- Settings widget
----------------------------------------------------------------

data UiSetting (env :: Environment) where
  UiSetting :: forall name t env
             . NotPresent name env
            => Proxy name
            -> IORef (Scalar t)
            -> UiSetting env
            -> UiSetting ( '(name, t) ': env)
  LastUiSetting :: UiSetting '[]

data SettingsWidget (env :: Environment) = SettingsWidget
  { settingsDialog :: Dialog ()
  , settingsRefs   :: UiSetting env
  }

readSetting :: forall name t env
             . HasSetting name t env
            => SettingsWidget env
            -> Proxy name
            -> IO (Scalar t)
readSetting sw name = readIORef (getSetting (settingsRefs sw) name)

class HasSetting (name :: Symbol) (t :: Type) (env :: Environment) where
  getSetting :: UiSetting env -> Proxy name -> IORef (Scalar t)

instance HasSetting name t ( '(name, t) ': env) where
  getSetting (UiSetting _ ref _) _ = ref

instance HasSetting name t env => HasSetting name t ( x ': env) where
  getSetting (UiSetting _ _ ss) pxy = getSetting ss pxy

makeSettings :: SettingsList env -> IO (UiSetting env)
makeSettings = \case
  NoSettings -> pure LastUiSetting
  AddSetting (Setting name v _) more -> do
    ref <- newIORef v
    UiSetting name ref <$> makeSettings more

instance ToUI WX (Settings env NoEffects) where
  -- Require a reference to the parent frame when creating the settings widget
  type CreationContext WX (Settings env NoEffects) = Frame ()

  -- Resulting UI object
  type UIObject WX (Settings env NoEffects) = SettingsWidget env

  -- Construct a SettingsWidget
  toUI Settings{..} parentFrame = do
    settingsDialog <- dialog parentFrame
      [ text := settingsTitle
      , resizeable := True
      , on resize := propagateEvent ]

    settingsRefs <- makeSettings settingsList
    let addSetting :: forall name t. Setting name t -> IO [Layout]
        addSetting (Setting _ (Scalar ty v) mn) = do
          let name = fromMaybe (showType ty) (fst <$> mn)
          ctrl <- case ty of
                  -- Represent boolean settings with a checkbox
                  BooleanProxy -> widget <$>
                      checkBox settingsDialog
                        [ checkable := True
                        , checked := v ]
                  -- Color picker, triggered by a colored button
                  ColorProxy -> widget <$>
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
    widgets <- sequence (mapSetting addSetting settingsList)
    set settingsDialog
      [ layout := margin 10 (grid 10 10 widgets)
      , visible := True ]

    pure SettingsWidget{..}

toWxColor :: FSColor.Color -> Color
toWxColor c =
  let (r,g,b) = FSColor.colorToRGB c
  in rgb r g b

type MySettings =
  '[ '("x", 'RealT)
   , '("y", 'IntegerT)
   , '("z", 'BooleanT)
   , '("c", 'ColorT)
   ]

sampleSettings :: Settings MySettings NoEffects
sampleSettings = Settings{..}
  where
    onChanged = NoOp
    parentActor = error "todo"
    settingsTitle = "Settings widget demo"
    settingsList
      = AddSetting (Setting Proxy (Scalar RealProxy    3.141592)       (Just ("pi", [])))
      $ AddSetting (Setting Proxy (Scalar IntegerProxy 42)              Nothing)
      $ AddSetting (Setting Proxy (Scalar BooleanProxy True)           (Just ("toggle", [])))
      $ AddSetting (Setting Proxy (Scalar ColorProxy   FSColor.violet) (Just ("shade", [])))
      $ NoSettings

----------------------------------------------------------------
-- Viewer widget
----------------------------------------------------------------

data ViewerWidget where
  ViewerWidget :: forall env.
    { settingsWidget :: SettingsWidget env
    , viewerFrame    :: Frame ()
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
    settingsWidget <- toUI @WX viewerSettings viewerFrame
    set (settingsDialog settingsWidget) [ visible := False ]

    -- Build the Tools menu
    toolsMenu <- menuPane [text := "&Tools"]
    case viewerTools of
      [] -> void $ menuItem toolsMenu [ text := "None", enabled := False ]
      tools -> forM_ tools $ \_tool -> pure ()

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

sampleViewer :: Viewer
sampleViewer = Viewer
  { viewerSettings = sampleSettings
  , onResize = undefined
  , onTimer = Nothing
  , viewerTools = []
  }
