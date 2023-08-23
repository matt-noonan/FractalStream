module Language.Value.Weaken
  ( weaken
  ) where

import Language.Value
import Data.Indexed.Functor
import Fcf (Eval, Exp, Pure1)

data Pattern env t where
  Any :: forall env t. Value env t -> Pattern env t
  Match :: forall env t. ValueF env (Pure1 (Pattern env)) t -> Pattern env t
  Const :: forall env t. ScalarType t -> Pattern env t
  AnyPositive :: forall env t. Value env t -> Pattern env t

Match (PowF (Match (AbsC (Any @"z"))) (Const 2)) -> _
Match (LTF (Match (PowF (Match (AbsC (Any z))))) (AnyPositive x))
  -> Fix (LTF)

|{any:z}|^2 -> re({z})*
