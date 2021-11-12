module Language.Effect.Provide
  ( Provide(..)
  ) where

import Language.Value
import Data.Indexed.Functor
import Fcf (Exp, Eval)

data Provide (env :: Environment)
             (code :: (Environment, Type) -> Exp *)
             (et :: (Environment, Type)) where

  Provide :: forall env t code env2
           . env `CanAppendTo` env2
          => EnvironmentProxy env
          -> EnvironmentProxy env2
          -> TypeProxy t
          -> Eval (code '(env `EnvAppend` env2, t))
          -> Provide env2 code '(env, t)

instance IFunctor (Provide env) where
  type IndexProxy (Provide env) = EnvTypeProxy
  imap f (Provide env env2 ty code) =
    Provide env env2 ty (f (envTypeProxy (env `envAppend` env2) ty) code)
  toIndex (Provide env _ ty _) = envTypeProxy env ty

instance ITraversable (Provide env) where
  isequence (Provide env env2 ty mcode) = Provide env env2 ty <$> mcode
