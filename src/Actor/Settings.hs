module Actor.Settings
  ( Setting(..)
  , SettingsList(..)
  , Settings(..)
  , mapSetting
  ) where

import Language.Code
import Actor
import Event

data Setting (name :: Symbol) (t :: Type) where
  Setting :: forall name ty
           . Proxy name -- ^ Variable name
          -> Scalar ty  -- ^ Initial value
          -> Maybe (String
                   , [ ( String
                       , Code '[ '(name, ty) ] NoEffects 'BooleanT) ]
                   ) -- ^ User-facing name, and input validators
          -> Setting name ty

data SettingsList (env :: Environment) where
  AddSetting :: forall name ty env
              . NotPresent name env
             => Setting name ty
             -> SettingsList env
             -> SettingsList ( '(name, ty) ': env)
  NoSettings :: SettingsList '[]

mapSetting :: (forall name ty. Setting name ty -> a) -> SettingsList env -> [a]
mapSetting f = \case
  NoSettings      -> []
  AddSetting x xs -> f x : mapSetting f xs

data Settings (env :: Environment) (effs :: [Effect]) where
  Settings :: forall env effs.
    { settingsList :: SettingsList env
    , settingsTitle :: String
    , parentActor :: SomeActor
    , onChanged :: Code env effs 'VoidT
    } -> Settings env effs

instance Actor (Settings env effs) where
  handle evt Settings{..} = case evt of
    Refresh -> Just (SomeCode onChanged)
    _       -> Nothing
