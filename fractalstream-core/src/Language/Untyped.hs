{-# language PolyKinds, UndecidableInstances #-}
module Language.Untyped
  ( Untyped(..)
  , UFix(..)
  ) where

import Data.Indexed.Functor
import Fcf (Exp, Eval)
import Data.Functor.Const

data K :: * -> k -> Exp *
type instance Eval (K t i) = t

data Untyped (f :: (k -> Exp *) -> (k -> *)) (t :: *) where
  Untyped :: forall f i t
           . f (K t) i
          -> Untyped f t

instance IFunctor f => Functor (Untyped f) where
  fmap f (Untyped x) = Untyped (imap (const f) x)

instance Traversable (Untyped f) => Foldable (Untyped f) where
  foldMap f = getConst . traverse (Const . f)

instance ITraversable f => Traversable (Untyped f) where
  sequenceA (Untyped x) = Untyped <$> isequence (imap (const id) x)

newtype UFix f = UFix (f (UFix f))
{-
infer :: UFix (Untyped f) -> ScalarProxy t -> Maybe (Fix f t)
infer ut  = error "todo"

inferValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> UFix (Untyped (ValueF env))
           -> Maybe (Value env t)
-}
