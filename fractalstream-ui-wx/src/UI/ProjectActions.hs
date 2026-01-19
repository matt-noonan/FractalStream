module UI.ProjectActions
  ( ProjectActions(..)
  , SessionHandle(..)
  , SessionInfo(..)
  ) where

import Data.DynamicValue
--import Actor.Ensemble (Template)

import Graphics.UI.WX (Frame)

newtype SessionHandle = SessionHandle (Frame ())
  deriving (Eq, Ord, Show)

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  --, projectOpenTemplate :: String -> Template -> IO ()
  , projectEdit :: FilePath -> IO ()
  , projectNew  :: IO ()
  , activeSessions :: Variable [SessionInfo]
  , closeSession :: SessionInfo -> IO ()
  , hideSession  :: SessionInfo -> IO ()
  , showSession  :: SessionInfo -> IO ()
  , editSession  :: SessionInfo -> IO ()
  }

data SessionInfo = SessionInfo
  { sessionName :: Variable String
  , sessionHandle :: SessionHandle
  , sessionVisible :: Variable Bool
  , sessionUnsaved :: Variable Bool
  }
