module UI.ProjectActions
  ( ProjectActions(..)
  , projectFileTypes
  ) where

import Data.DynamicValue
import UI.Session
import FractalStream.Library (LibraryEntry)

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  , projectOpenLibrary :: LibraryEntry -> IO ()
    -- ^ Open a copy of a project from the built-in library: it has no
    -- file of its own until it's saved.
  , showLibrary :: IO ()
    -- ^ Show the library browser, opening it again if it was closed.
  , showPreferences :: IO ()
  , projectsFolder :: IO FilePath
    -- ^ Where the user's own projects are, as the settings have it now.


  , projectNew  :: IO ()
  , activeSessions :: Variable [SessionInfo]
  , closeSession :: SessionInfo -> IO ()
  , hideSession  :: SessionInfo -> IO ()
  , showSession  :: SessionInfo -> IO ()
  }

-- | The file types to offer in every dialog that opens or saves a
-- project.
projectFileTypes :: [(String, [String])]
projectFileTypes =
  [ ("FractalStream project files", ["*.fs"])
  , ("All files", ["*.*"])
  ]

