module UI.ProjectActions
  ( ProjectActions(..)
  ) where

import Data.DynamicValue
import Actor.Ensemble (Ensemble)
import UI.Session

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  , projectOpenTemplate :: String -> Ensemble -> IO ()
  , projectNew  :: IO ()
  , activeSessions :: Variable [SessionInfo]
  , closeSession :: SessionInfo -> IO ()
  , hideSession  :: SessionInfo -> IO ()
  , showSession  :: SessionInfo -> IO ()
  , editSession  :: SessionInfo -> IO ()
  }
