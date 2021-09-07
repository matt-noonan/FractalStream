{-# language UndecidableInstances #-}

module Language.Effect
  ( type Effect
  , HasEffect(..)
  , EffectHandler(..)
  , Handlers(..)
  , type NoEffects
  , KnownHandlers(..)
  ) where

import Language.Type
import Language.Environment

import Fcf (Exp, Eval)
import Data.Proxy

type Effect = Environment -> Type -> *

type NoEffects = '[] :: [Effect]

data EffectHandler (eff :: Effect) (result :: Environment -> Type -> Exp *) where
  Handle :: forall result eff
          . Proxy result
         -> (forall env t
              . KnownEnvironment env
             => Proxy env
             -> ScalarProxy t
             -> eff env t
             -> Eval (result env t))
         -> EffectHandler eff result

data Handlers (effs :: [Effect]) (result :: Environment -> Type -> Exp *) where
  Handler :: forall eff effs result
           . EffectHandler eff result
          -> Handlers effs result
          -> Handlers (eff ': effs) result
  NoHandler :: forall result. Handlers '[] result

class HasEffect (e :: Effect) (es :: [Effect]) where
  getHandler :: forall result
              . Proxy e
             -> Handlers es result
             -> EffectHandler e result

instance HasEffect e (e ': es) where
  getHandler _ (Handler h _) = h

instance HasEffect e es => HasEffect e (e' ': es) where
  getHandler e (Handler _ hs) = getHandler e hs

class KnownHandlers (effs :: [Effect]) (result :: Environment -> Type -> Exp *) where
  knownHandlers :: Handlers effs result
{-
class KnownHandler (e :: Effect) (result :: Environment -> Type -> Exp *) where
  knownHandler :: Handler e result
-}
