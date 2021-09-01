module Actor.Tool
  ( Tool(..)
  ) where

import Language.Code
import Actor.Settings

data Tool effs where
  Tool :: forall env effs posEnv pos2Env
        . ( NotPresent "posX" env
          , NotPresent "posY" env
          , NotPresent "posX0" env
          , NotPresent "posY0" env
          , posEnv  ~ ( '("posX",  'RealT) ': '("posY",  'RealT) ': env )
          , pos2Env ~ ( '("posX0", 'RealT) ': '("posY0", 'RealT) ': posEnv )
          )
       =>
    { toolSettings :: Settings env effs
    , onClick :: Maybe (Code effs posEnv 'VoidT)
    , onMouseDown :: Maybe (Code effs posEnv 'VoidT)
    , onMouseUp   :: Maybe (Code effs posEnv 'VoidT)
    , onMotion :: Maybe (Code effs posEnv 'VoidT)
    , onDrag :: Maybe (Code effs pos2Env 'VoidT)
    , onButton :: [(String, Code effs env 'VoidT)]
    } -> Tool effs
