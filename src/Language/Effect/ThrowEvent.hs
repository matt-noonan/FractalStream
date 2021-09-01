module Language.Effect.ThrowEvent
  ( Event(..)
  , ThrowEvent(..)
  ) where

import Language.Value
import Actor
import Event

data ThrowEvent (env :: Environment) (ty :: Type) where
  Throw :: forall env. SomeActor -> Event (Value env) -> ThrowEvent env 'VoidT
