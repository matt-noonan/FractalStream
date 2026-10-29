module UI.ProjectActions
  ( ProjectActions(..)
  ) where

import Data.DynamicValue
import UI.Session

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  , projectNew  :: IO ()
  , activeSessions :: Variable [SessionInfo]
  , closeSession :: SessionInfo -> IO ()
  , hideSession  :: SessionInfo -> IO ()
  , showSession  :: SessionInfo -> IO ()
  , editSession  :: SessionInfo -> IO ()
  }
