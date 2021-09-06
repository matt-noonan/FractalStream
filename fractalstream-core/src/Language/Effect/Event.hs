module Language.Effect.ThrowEvent
  ( Event(..)
  , ThrowEvent(..)
  , SomeActor(..)
  ) where

import Event
import Actor

data ThrowEvent (env :: Environment) (ty :: Type) where
  Throw :: forall env. SomeActor -> Event (Value env) -> ThrowEvent env 'VoidT
