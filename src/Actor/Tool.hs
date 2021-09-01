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
    , onClick :: Maybe (Code posEnv effs 'VoidT)
    , onMouseDown :: Maybe (Code posEnv effs 'VoidT)
    , onMouseUp   :: Maybe (Code posEnv effs 'VoidT)
    , onMotion :: Maybe (Code posEnv effs 'VoidT)
    , onDrag :: Maybe (Code pos2Env effs 'VoidT)
    , onButton :: [(String, Code env effs 'VoidT)]
    } -> Tool effs
