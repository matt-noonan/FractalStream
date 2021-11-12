{-# language UndecidableInstances #-}

module Language.Effect
  ( type Effect
  , HasEffect(..)
  , EffectHandler(..)
  , Handlers(..)
  , type NoEffects
  , EffectParser(..)
  , EffectParsers(..)
  , EffectParsers_(..)
  , noParser
  , mapHandlers
  ) where

import Language.Type
import Language.Environment
import Language.Parser
import Data.Indexed.Functor

import Fcf (Exp, Eval)
import Data.Proxy
import Data.Type.Equality ((:~:)(..))

-- | An @Effect@ takes a family of nested code types, and produces
-- an AST of effects that may include nested code. Effects are a
-- kind of "@Code@ mix-in", so they should have the same kind
-- signature as @Code effects@.
type Effect = ((Environment, Type) -> Exp *) -> (Environment, Type) -> *

type NoEffects = '[] :: [Effect]

data EffectHandler (eff :: Effect) (result :: (Environment, Type) -> Exp *) where
  Handle :: forall result eff
          . Proxy result
         -> (forall env t
              . EnvironmentProxy env
             -> TypeProxy t
             -> eff result '(env, t)
             -> Eval (result '(env, t)))
         -> EffectHandler eff result

data Handlers (effs :: [Effect]) (result :: (Environment, Type) -> Exp *) where
  Handler :: forall eff effs result
           . (IFunctor eff, IndexProxy eff ~ EnvTypeProxy)
          => EffectHandler eff result
          -> Handlers effs result
          -> Handlers (eff ': effs) result
  NoHandler :: forall result. Handlers '[] result

mapHandlers :: forall effs a b
             . (forall env t
                 . EnvironmentProxy env
                -> TypeProxy t
                -> Eval (a '(env,t))
                -> Eval (b '(env,t)))
            -> (forall env t
                 . EnvironmentProxy env
                -> TypeProxy t
                -> Eval (b '(env,t))
                -> Eval (a '(env,t)))
            -> Handlers effs a
            -> Handlers effs b
mapHandlers f unF = \case
  NoHandler -> NoHandler
  Handler (Handle _ h) hs ->
    Handler (Handle (Proxy @b) (\e t x -> f e t (h e t (imap unF' x)))) (mapHandlers @_ @a @b f unF hs)
 where
    unF' :: forall et. EnvTypeProxy et -> Eval (b et) -> Eval (a et)
    unF' (et :: EnvTypeProxy et) = case lemmaEnvTy @et of
      Refl -> withEnvType et unF


class (IndexProxy e ~ EnvTypeProxy, ITraversable e) => HasEffect (e :: Effect) (es :: [Effect]) where
  getHandler :: forall result
              . Proxy e
             -> Handlers es result
             -> EffectHandler e result

instance {-# OVERLAPS #-} (IndexProxy e ~ EnvTypeProxy, ITraversable e) => HasEffect e (e ': es) where
  getHandler _ (Handler h _) = h

instance {-# OVERLAPPABLE #-} HasEffect e es => HasEffect e (e' ': es) where
  getHandler e (Handler _ hs) = getHandler e hs

data EffectParser (eff :: Effect) where
  EffectParser :: forall eff
                . Proxy eff
               -> (forall et code
                   . EnvTypeProxy et
                  -> (forall et'
                      . EnvTypeProxy et'
                     -> Parser (Eval (code et')))
                  -> Parser (eff code et))
               -> EffectParser eff

newtype EffectParsers effs = EP (EffectParsers_ effs effs)

data EffectParsers_ (effs :: [Effect]) (effs0 :: [Effect]) where
  ParseEff :: forall eff effs effs0
            . HasEffect eff effs0
           => EffectParser eff
           -> EffectParsers_ effs effs0
           -> EffectParsers_ (eff ': effs) effs0
  NoEffs :: forall effs0. EffectParsers_ '[] effs0

noParser :: EffectParser eff
noParser = EffectParser Proxy (\_ _ -> mzero)
