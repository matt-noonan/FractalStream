module Actor.Settings
  ( Setting(..)
  , Settings(..)
  , Pure2
  ) where

import GHC.TypeLits
import Language.Code
import Actor
import Event
import Fcf (Pure2)

data Setting (name :: Symbol) (t :: Type) where
  Setting :: forall name ty
           . KnownSymbol name
          => Proxy name -- ^ Variable name
          -> Scalar ty  -- ^ Initial value
          -> Maybe (String
                   , [ ( String
                       , Code NoEffects '[ '(name, ty) ] 'BooleanT )]
                   ) -- ^ User-facing name, and input validators
          -> Setting name ty

data Settings (env :: Environment) (effs :: [Effect]) where
  Settings :: forall env effs.
    ( KnownEnvironment env
    ) =>
    { settingsList :: Context (Pure2 Setting) env
    , settingsTitle :: String
    , parentActor :: SomeActor
    , onChanged :: Code effs env 'VoidT
    } -> Settings env effs

instance Actor (Settings env effs) where
  handle evt Settings{..} = case evt of
    Refresh -> Just (SomeCode onChanged)
    _       -> Nothing
