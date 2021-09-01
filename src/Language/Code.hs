{-# language AllowAmbiguousTypes #-}

module Language.Code
  ( module Language.Value
  , module Language.Effect
  , Code(..)
  , SomeCode(..)
  , let_
  , set
  ) where

import Language.Value
import Language.Effect

data SomeCode where
  SomeCode :: forall env effs ty. Code env effs ty -> SomeCode

data Code (env :: Environment) (effs :: [Effect]) (t :: Type) where
  Let :: forall name ty env effs result
       . NotPresent name env
      => Proxy (name :: Symbol)
      -> Value env ty
      -> Code ('(name, ty) ': env) effs result
      -> Code env effs result
  Set :: forall name ty env effs
       . Required name env ~ ty
      => Proxy name
      -> Value env ty
      -> Code env effs 'VoidT

  -- | Invoke another chunk of 'Code', using a copy of the environment.
  -- Variable mutations within the called code will not be reflected
  -- back in the caller.
  Call :: forall env effs ty
        . Code env effs ty
       -> Code env effs ty

  Block :: forall env effs ty
         . [Code env effs 'VoidT]
        -> Code env effs ty
        -> Code env effs ty
  Pure :: forall env effs t. Value env t -> Code env effs t
  NoOp :: forall env effs. Code env effs 'VoidT

  DoWhile :: forall env effs
           . Code env effs 'BooleanT
          -> Code env effs 'VoidT

  IfThenElse :: forall env effs t
       . Value env 'BooleanT
      -> Code env effs t
      -> Code env effs t
      -> Code env effs t

  Effect :: forall env effs eff t
          . HasEffect eff effs
         => eff env t
         -> Code env effs t

-- | Bind a name to a value.
-- This is the same as using the 'Let' constructor directly,
-- except that it saves you the syntactic noise of using
-- a 'Proxy', by using type applications instead.
--
--     let_ @"foo" value code
--
--     Let (Proxy :: Proxy "foo") value code
--
let_ :: forall name env effs ty result
      . NotPresent name env
     => Value env ty
     -> Code ('(name, ty) ': env) effs result
     -> Code env effs result
let_ = Let (Proxy @name)


-- | Set the value of a variable.
-- This is the same as using the 'Set' constructor directly,
-- except that it saves you the syntactic noise of using a
-- 'Proxy', by using type applications instead.
--
--     set @"foo" value
--
-- vs
--
--     Set (Proxy :: Proxy "foo") value
--
set :: forall name env effs ty
     . Required name env ~ ty
    => Value env ty
    -> Code env effs 'VoidT
set = Set (Proxy @name)
