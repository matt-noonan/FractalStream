module FractalStream.Prelude
  (
    -- * Re-exports
      module Data.Kind
    , module GHC.TypeLits
    , module Control.Monad
    , module Control.Applicative
    , module Control.Monad.State
    , module Control.Monad.Reader
    , module Control.Monad.Except
    , module Data.Functor
    , module Data.Bifunctor
    , module Data.String
    , module Data.Complex
    , module Data.Coerce
    , module Data.Int
    , module Data.Word
    , module Data.Maybe
    , (***)
    , (&&&)
    , left
    , right
    , Exp
    , Eval
    , Pure1
    , Id
    , Map
    , Set
    , (:~:)(..)
    , Proxy(..)
    , intercalate
    , isPrefixOf
    , isSuffixOf
    , sortOn
    , unsnoc
    , fromRight
    , fromLeft
    , An(..)
    , PrettyPrint(..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Kind
import Data.Coerce
import Data.Complex
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits hiding (LTI, GTI)
import Control.Monad
import Control.Applicative hiding (Const(..))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor hiding (unzip)
import Data.Bifunctor
import Data.String
import Fcf (Exp, Eval, Pure1)
import Lens.Micro hiding (ix, set, to)
import Lens.Micro.TH (makeLenses)

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn, unsnoc)
import Data.Int
import Data.Word

import Control.Arrow ((***), (&&&), left, right)

fromRight :: (a -> b) -> Either a b -> b
fromRight f = either f id

fromLeft :: (a -> b) -> Either b a -> b
fromLeft f = either id f

class An a where
  an :: a -> String

instance An String where
  an x = case x of
    "" -> "???"
    (c:_)
      | elem @[] c "aeiouAEIOU" -> "an " ++ x
      | otherwise -> "a " ++ x

class PrettyPrint a where
  pp :: a -> [String]

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pp = either pp pp

data Id a :: Exp Type
type instance Eval (Id x) = x
