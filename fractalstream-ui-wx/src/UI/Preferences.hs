-- | The Preferences window, for FractalStream's own settings
-- ("FractalStream.Settings"). Each change is saved as soon as it's made.
module UI.Preferences
  ( preferencesWindow
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import FractalStream.Library (projectsDirectory)
import FractalStream.Settings

import Graphics.UI.WX hiding (pt, when, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore (windowRaise)
import System.IO (hPutStrLn, stderr)
import Control.Exception (IOException, try)

-- | Show a new Preferences window for the settings in the given
-- variable, returning it.
preferencesWindow :: Variable Settings -> IO (Frame ())
preferencesWindow settingsVar = do
  f <- frame [ text := "Preferences", resizeable := True ]
  p <- panel f []

  folderText <- staticText p [ text := "" ]
  chooseButton <- button p [ text := "Choose..." ]
  useDefaultButton <- button p [ text := "Use the default" ]
  note <- staticText p [ text := "Changing the folder doesn't move any projects: it only \
                                 \changes where the library browser looks for them." ]

  let showSettings = do
        settings <- getDynamic settingsVar
        folder <- projectsDirectory settings
        set folderText [ text := folder ++ maybe " (the default)" (const "") (setProjectsFolder settings) ]
        set useDefaultButton [ enabled := isJust (setProjectsFolder settings) ]
        windowReLayout f
      change k = do
        modifyValue settingsVar k
        settings <- getDynamic settingsVar
        try (saveSettings settings) >>= \case
          Left (e :: IOException) -> do
            hPutStrLn stderr ("Couldn't save the settings: " ++ show e)
            errorDialog f "Couldn't save the settings" (show e)
          Right () -> pure ()
        showSettings

  set chooseButton [ on command := do
                       current <- projectsDirectory =<< getDynamic settingsVar
                       dirOpenDialog f True "Choose the folder for your own projects" current
                         >>= mapM_ (\folder -> change (\s -> s { setProjectsFolder = Just folder })) ]
  set useDefaultButton [ on command := change (\s -> s { setProjectsFolder = Nothing }) ]

  -- The folder's path can grow and wrap sideways; the buttons and the
  -- note keep their size. (See CLAUDE_NOTES.md on wxHaskell's layout.)
  set f [ layout := container p $ margin 12 $ column 8
            [ label "My projects folder:"
            , hfill (widget folderText)
            , row 6 [ widget chooseButton, widget useDefaultButton, hglue ]
            , hstretch (widget note)
            ]
        ]
  showSettings
  windowRaise f
  pure f
