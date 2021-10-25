{-# language UndecidableInstances #-}
module Data.Recursive
  ( Fix(..)
  , Fix1(..)
  , Fixpoint(..)
  , fold
  , foldM
  , unfold
  , unfoldM
  , AnnF(..)
  , type Ann
  , pattern Ann
  , annotation
  ) where

import Data.Coerce
import Control.Monad hiding (foldM)
import Data.Bifunctor

---------------------------------------------------------------------------------
-- Fixpoint class
---------------------------------------------------------------------------------

class Functor f => Fixpoint t f | t -> f where
  unroll :: t -> f t
  default unroll :: Coercible t (f t) => t -> f t
  unroll = coerce
  reroll :: f t -> t
  default reroll :: Coercible t (f t) => f t -> t
  reroll = coerce

---------------------------------------------------------------------------------
-- Fixpoint of a functor
---------------------------------------------------------------------------------

newtype Fix f = Fix (f (Fix f))

deriving instance Eq   (f (Fix f)) => Eq   (Fix f)
deriving instance Ord  (f (Fix f)) => Ord  (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

instance Functor f => Fixpoint (Fix f) f

---------------------------------------------------------------------------------
-- Fixpoint of a bifunctor, which will itself be a functor
---------------------------------------------------------------------------------

newtype Fix1 f a = Fix1 (f a (Fix1 f a))

deriving instance Eq   (f a (Fix1 f a)) => Eq   (Fix1 f a)
deriving instance Ord  (f a (Fix1 f a)) => Ord  (Fix1 f a)
deriving instance Show (f a (Fix1 f a)) => Show (Fix1 f a)

instance Bifunctor f => Functor (Fix1 f) where
  fmap f = \(Fix1 x) -> Fix1 (bimap f (fmap f) x)

instance Functor (f a) => Fixpoint (Fix1 f a) (f a)

---------------------------------------------------------------------------------
-- Annotated recursive types
---------------------------------------------------------------------------------

-- | If @t@ is a recursive type that can be represented as
-- a fixpoint of the functor @f@, then @Ann f a@ represents
-- @t@ with each node annotated with a value of type @a@.
type Ann f = Fix1 (AnnF f)

data AnnF f a x = AnnF a (f x)
  deriving (Show, Functor, Foldable, Traversable)

pattern Ann :: forall (f :: * -> *) a
             . a
            -> f (Fix1 (AnnF f) a)
            -> Fix1 (AnnF f) a
pattern Ann a v = Fix1 (AnnF a v)
{-# COMPLETE Ann #-}

annotation :: Ann f a -> a
annotation (Ann a _) = a

---------------------------------------------------------------------------------
-- Folds and unfolds
---------------------------------------------------------------------------------

fold :: forall t f a. Fixpoint t f => (f a -> a) -> t -> a
fold f = let go = f . fmap go . unroll in go

foldM :: forall t f a m. (Fixpoint t f, Traversable f, Monad m)
      => (f a -> m a) -> t -> m a
foldM f = let go = (f <=< traverse go) . unroll in go

unfold :: forall t f a. Fixpoint t f => (a -> f a) -> a -> t
unfold f = let go = reroll . fmap go . f in go

unfoldM :: forall t f a m. (Fixpoint t f, Traversable f, Monad m)
        => (a -> m (f a)) -> a -> m t
unfoldM f = let go = fmap reroll . join . fmap (traverse go) . f in go
