module Actor
  ( Actor(..)
  , SomeActor(..)
  , CodeWithEffects(..)
  ) where

import Event
import Language.Code

data SomeActor where
  SomeActor :: forall actor. Actor actor => actor -> SomeActor

class Actor actor where

  handle :: actor -> Event args -> Maybe (CodeWithEffects args)
  handle _ _ = Nothing
{-
data EventHandler where
  EventHandler :: forall args
                . Event args
               -> Settings env settingsEffs
               -> Code effs (args `EnvAppend` env) 'VoidT
-}
data CodeWithEffects (args :: Environment) where
  CodeWithEffects :: forall effs args
                   . Code effs args 'VoidT
                  -> CodeWithEffects args
