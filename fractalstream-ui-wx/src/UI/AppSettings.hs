-- | The settings FractalStream is running with ("FractalStream.Settings"),
-- shared by the whole app: read at startup ('Main'), changed in the
-- Preferences window, and consulted wherever a setting matters -- in a
-- code editor's keys, for example -- without passing them down to
-- every window that might need them.
module UI.AppSettings
  ( appSettings
  ) where

import Data.DynamicValue (Variable, newVariable)
import FractalStream.Settings (Settings, defaultSettings)
import System.IO.Unsafe (unsafePerformIO)

-- | The app's settings: the defaults until 'Main' has read the settings
-- file.
appSettings :: Variable Settings
appSettings = unsafePerformIO (newVariable defaultSettings)
{-# NOINLINE appSettings #-}
