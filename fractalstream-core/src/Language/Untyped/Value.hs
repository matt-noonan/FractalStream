module Language.Untyped.Value
  ( type Value
  , ValueF(..)
  , Fun(..)
  , ArithOp(..)
  , CmpOp(..)
  , type ValueWith
  ) where

import Prelude hiding (GT, LT)
--import Control.Monad.State.Strict
import Data.Color
import Data.Recursive
import Data.Kind

type Value = Fix ValueF

type ValueWith = Ann ValueF

data Fun = Abs | Exp | Log | Sqrt | Sin | Cos | Tan | Sinh | Cosh | Tanh
         | Arcsin | Arccos | Arctan | Arcsinh | Arccosh | Arctanh
         | Arg | Re | Im | Conj
         | Neg
  deriving (Eq, Ord, Show)

data ArithOp = Add | Sub | Mul | Div | Pow | Mod
             | Arctan2
  deriving (Eq, Ord, Show)

data CmpOp = LE | LT | GE | GT
  deriving (Eq, Ord, Show)

data ValueF (value :: Type)
  = ConstB Bool
  | ConstI Integer
  | ConstF Double
  | ConstC Double Double
  | ConstColor Color
  | Var String
  | PairV value value
  | ProjV1 value
  | ProjV2 value
  | Arith ArithOp value value
  | Ap1 Fun value
  | Or value value
  | And value value
  | Not value
  | ITE value value value
  | RGB value value value
  | Blend value value value
  | InvertRGB value
  | Eql value value
  | NEq value value
  | Cmp CmpOp value value
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
