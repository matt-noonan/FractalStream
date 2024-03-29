{-# language OverloadedStrings #-}
module Language.Effect.Output
  ( Output(..)
  , outputEffectParser
  ) where

import Language.Value
import Language.Effect
import Language.Parser
import Language.Value.Parser
import Data.Indexed.Functor
import GHC.TypeLits
import Fcf (Exp)
import Data.Type.Equality ((:~:)(..))
import Data.Kind

import Debug.Trace

-- | An 'Output' effect introduces a set of typed write-only variables.
data Output (outputs :: Environment) (code :: (Environment, FSType) -> Exp Type) (et :: (Environment, FSType)) where
  Output :: forall name ty outputs env code
          . (KnownSymbol name, KnownType ty)
         => EnvironmentProxy env
         -> NameIsPresent name ty outputs
         -> Proxy name
         -> Value '(env, ty)
         -> Output outputs code '(env, 'VoidT)

instance IFunctor (Output outputs) where
  type IndexProxy (Output outputs) = EnvTypeProxy
  imap _ (Output env pf name v) = Output env pf name v
  toIndex (Output env _ _ _) = envTypeProxy env VoidType

instance ITraversable (Output outputs) where
  isequence (Output env pf name v) = pure (Output env pf name v)

-- | output VALUE to NAME
outputEffectParser :: forall outputs
                    . EnvironmentProxy outputs
                   -> EffectParser (Output outputs)
outputEffectParser outputs = EffectParser (Proxy @(Output outputs)) $
  \(et :: EnvTypeProxy et) _code -> case lemmaEnvTy @et of
    Refl -> withEnvType et $ \env -> \case
      VoidType -> do
        tok_ "output"
        -- Grab the tokens corresponding to VALUE, but don't
        -- parse them yet. We want to peek at the type of
        -- NAME first, to know what type to parse them at.
        valueTokens <- manyTill anyToken (tok_ "to")

        Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
        case someSymbolVal n of
          SomeSymbol name -> case lookupEnv' name outputs of
            Absent' _ -> mzero
            Found' t pf -> do
              traceM ("got here, t = " ++ showType t)
              v <- nest (parseValueFromTokens env EmptyContext t valueTokens)
              traceM ("parsed v = " ++ pprint v)
              eol
              pure (withKnownType t $ Output env pf name v)
      _ -> mzero
