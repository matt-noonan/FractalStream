module Actor.Tool
  ( Tool(..)
  ) where

import Language.Code
--import Actor
import Actor.Settings
import Event
import Language.Effect.Provide

data Tool effs where
  Tool :: forall env effs
        . ( PosEnv  `CanAppendTo` env
          , DragEnv `CanAppendTo` env
          ) =>
    { toolSettings :: Settings env '[]
    , toolName :: String
    , toolHelp :: String
    , onClick     :: Maybe (Code (Provide env ': effs) PosEnv 'VoidT)
    , onMouseDown :: Maybe (Code (Provide env ': effs) PosEnv 'VoidT)
    , onMouseUp   :: Maybe (Code (Provide env ': effs) PosEnv 'VoidT)
    , onMotion    :: Maybe (Code (Provide env ': effs) PosEnv 'VoidT)
    , onDrag      :: Maybe (Code (Provide env ': effs) DragEnv 'VoidT)
    , buttons     :: [(String, Code (Provide env ': effs) '[] 'VoidT)]
    } -> Tool effs

{-
instance Actor (Tool effs) where
  handle Tool{..} _ = \case
    Click     -> onClick
    MouseDown -> onMouseDown
    MouseUp   -> onMouseUp
    Motion    -> onMotion
    Drag      -> onDrag
    _         -> Nothing
-}
