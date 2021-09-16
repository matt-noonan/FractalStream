module Language.Effect.ThrowEvent where
{-  ( Event(..)
  , ThrowEvent(..)
  ) where

import Language.Value
import Actor
import Event
import Fcf (Exp)


data ThrowEvent (code :: (Environment, Type) -> Exp *)
                (et :: (Environment, Type)) where
  Throw :: forall et args code
         . EnvTypeProxy et
        -> SomeActor
        -> Event args
        -> ThrowEvent code et
-}
