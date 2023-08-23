module Project
  ( FractalStreamScript(..)
  , SettingDefinition(..)
  , OldScript(..)
  , Script(..)
  , ProjectSource(..)
  ) where

import Language.Type

data FractalStreamScript
  = OldStyle OldScript
  | NewStyle Script

data OldScript = OldScript
  { oldScriptName :: String
  , oldScript     :: String
  , oldScriptDesc :: String
  , derivedScript :: Maybe Script
  }

data Script = Script
  { scriptName   :: String
  , scriptSource :: String
  , scriptParams :: [SettingDefinition]
  }

data ProjectSource = ProjectSource
  { projectName  :: String
  , projectDesc  :: String
  , viewerScript :: Script
  , toolScripts  :: [Script]
  }

data SettingDefinition =
  SettingDefinition String FSType
