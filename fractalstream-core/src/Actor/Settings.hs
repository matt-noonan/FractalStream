module Actor.Settings
  ( Setting(..)
  , Settings(..)
  , InputValidator(..)
  , Pure2
  ) where

import Language.Effect.Provide
import Language.Code
--import Event

import GHC.TypeLits
import Fcf (Pure2)

data Setting (name :: Symbol) (t :: Type) where
  Setting :: forall name ty
           . KnownSymbol name
          => Proxy name -- ^ Variable name
          -> Scalar ty  -- ^ Initial value
          -> Maybe (String, [InputValidator name ty])
             -- ^ User-facing name, and input validators
          -> Setting name ty

data InputValidator name ty =
  InputValidator String (Value '( '[ '(name, ty) ], 'BooleanT))

data Settings (env :: Environment) (effs :: [Effect]) where
  Settings :: forall env effs.
    ( KnownEnvironment env
    ) =>
    { settingsList :: Context (Pure2 Setting) env
    , settingsTitle :: String
    , settingsEnv :: EnvironmentProxy env
    , onChanged :: Maybe (Code (Provide env ': effs) '[] 'VoidT)
    } -> Settings env effs

{-
instance Actor (Settings env effs) (Provide env ': effs) where
  handle Settings{..} _ = \case
    Refresh -> onChanged
    _       -> Nothing
-}
