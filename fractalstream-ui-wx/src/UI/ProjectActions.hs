module UI.ProjectActions
  ( ProjectActions(..)
  , projectFileTypes
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
  }

-- | The file types to offer in every dialog that opens or saves a
-- project.
projectFileTypes :: [(String, [String])]
projectFileTypes =
  [ ("FractalStream project files", ["*.fs"])
  , ("All files", ["*.*"])
  ]

