{-# language PolyKinds #-}
module Data.Indexed.Functor
  ( IFunctor(..)
  , IFixpoint(..)
  , (:.:)
  , ITraversable(..)
  , indexedFold
  , indexedFoldM
  ) where

import Control.Monad
import Fcf

class IFunctor (f :: (k -> Exp *) -> (k -> *)) where
  type IndexProxy :: k -> *
  imap :: forall a b i
        . (forall j. IndexProxy j -> Eval (a j) -> Eval (b j))
       -> f a i
       -> f b i
  toIndex :: forall a i. f a i -> IndexProxy i

class IFunctor f => IFixpoint (t :: k -> *) (f :: (k -> Exp *) -> (k -> *)) where
  unrollIx :: forall i. t i -> f (Pure1 t) i
  rerollIx :: forall i. f (Pure1 t) i -> t i

data (:.:) (m :: * -> *) (a :: k -> Exp *) (i :: k) :: Exp *
type instance Eval ((m :.: a) i) = m (Eval (a i))

class IFunctor f => ITraversable (f :: (k -> Exp *) -> (k -> *)) where

  isequence :: forall i m a
             . Applicative m
            => f (m :.: a) i
            -> m (f a i)
  isequence = itraverse (const id)

  itraverse :: forall i m a b
             . Applicative m
            => (forall j. IndexProxy j -> Eval (a j) -> m (Eval (b j)))
            -> f a i
            -> m (f b i)
  itraverse f = isequence . imap f

indexedFold :: forall a t f i
             . IFixpoint t f
            => (forall j. f a j -> Eval (a j))
            -> t i
            -> Eval (a i)
indexedFold f x =
  let go :: forall k. IndexProxy k -> t k -> Eval (a k)
      go _ = f . imap go . unrollIx
  in go (toIndex (unrollIx @_ @t @f x)) x

indexedFoldM :: forall a t f i m
              . (IFixpoint t f, ITraversable f, Monad m)
             => (forall j. f a j -> m (Eval (a j)))
             -> t i
             -> m (Eval (a i))
indexedFoldM f x =
  let go :: forall k. IndexProxy k -> t k -> m (Eval (a k))
      go _ = f <=< (itraverse go . unrollIx)
  in go (toIndex (unrollIx @_ @t @f x)) x
