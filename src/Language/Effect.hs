{-# language UndecidableInstances #-}

module Language.Effect
  ( type Effect
  , type NoEffects
  , type HasEffect
  , NoEffect(..)
  ) where

import Language.Type
import GHC.TypeLits
import GHC.Exts (Constraint)

type Effect = Environment -> Type -> *

type family HasEffect (e :: Effect) (es :: [Effect]) :: Constraint where
  HasEffect e (e ': _)  = ()
  HasEffect e (_ ': es) = HasEffect e es
  HasEffect e '[]       = TypeError ('Text "The effect " ':<>: 'ShowType e
                                    ':<>: 'Text " cannot be used here")

type NoEffects = '[NoEffect]

data NoEffect (env :: Environment) (t :: Type) = NoEffect
