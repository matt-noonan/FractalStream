{-# language AllowAmbiguousTypes #-}

module Language.Code
  ( module Language.Value
  , module Language.Effect
  , Code
  , Fix(..)
  , CodeF(..)
  , SomeCode(..)
  , let_
  , set
  ) where

import Language.Value
import Language.Effect
import Data.Indexed.Functor
import Fcf
import GHC.TypeLits
-- import Data.Function ((&))

data SomeCode where
  SomeCode :: forall effs env t. Code effs env t -> SomeCode

---------------------------------------------------------------------------------
-- Code
---------------------------------------------------------------------------------

type Code effs env t = Fix (CodeF effs) '(env, t)

-- | The Code type is used recursively at different Type parameters,
-- and also at different Environments (in a Let binding). That means
-- we need to use both the environment *and* the type as indices
-- in order to make an indexed functor.
data CodeF (effs :: [Effect]) (code :: (Environment, Type) -> Exp *) (et :: (Environment, Type)) where

  Let :: forall name ty env result effs code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> Value env ty
      -> ScalarProxy result
      -> Eval (code '( '(name, ty) ': env, result))
      -> CodeF effs code '(env, result)

  Set :: forall name ty env effs code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty env
      -> Proxy name
      -> Value env ty
      -> CodeF effs code '(env, 'VoidT)

  -- | Invoke another chunk of 'Code', using a copy of the environment.
  -- Variable mutations within the called code will not be reflected
  -- back in the caller.
  Call :: forall effs env ty code
        . KnownEnvironment env
       => ScalarProxy ty
       -> Eval (code '(env, ty))
       -> CodeF effs code '(env, ty)

  -- | A block of statements with VoidT type, followed by a
  -- statement with any time. The type of the block is the
  -- type of the final statement.
  Block :: forall effs env ty code
         . KnownEnvironment env
        => ScalarProxy ty
        -> [Eval (code '(env, 'VoidT))]
        -> Eval (code '(env, ty))
        -> CodeF effs code '(env, ty)

  -- | Lift a pure value to a bit of code
  Pure :: forall effs env ty code
        . KnownEnvironment env
       => Value env ty
       -> CodeF effs code '(env, ty)

  NoOp :: forall env effs code
        . KnownEnvironment env
       => CodeF effs code '(env, 'VoidT)

  -- | Do-while loop
  DoWhile :: forall env effs code
           . KnownEnvironment env
          => Eval (code '(env, 'BooleanT))
          -> CodeF effs code '(env, 'VoidT)

  -- | If/else statement
  IfThenElse :: forall env effs t code
       . KnownEnvironment env
      => ScalarProxy t
      -> Value env 'BooleanT
      -> Eval (code '(env, t))
      -> Eval (code '(env, t))
      -> CodeF effs code '(env, t)

  Effect :: forall env effs eff t code
          . (HasEffect eff effs, KnownEnvironment env)
         => ScalarProxy t
         -> eff env t
         -> CodeF effs code '(env, t)

---------------------------------------------------------------------------------
-- Indexed functor instance for Code
---------------------------------------------------------------------------------

type family Env (et :: (Environment, Type)) where Env '(env, t) = env

-- | Proxy the type index directly, and the environment
-- index implicitly through the KnownEnvironment constraint
data EnvTypeProxy (et :: (Environment, Type)) where
  EnvType :: forall env t
           . KnownEnvironment env
          => ScalarProxy t
          -> EnvTypeProxy '(env, t)

instance IFunctor (CodeF eff) where

  type IndexProxy (CodeF eff) = EnvTypeProxy

  toIndex = \case
    Let _ _ _ t _ -> EnvType t
    Set _ _ _     -> EnvType VoidProxy
    Call t _      -> EnvType t
    Block t _ _   -> EnvType t
    Pure v        -> EnvType (typeOfValue v)
    NoOp          -> EnvType VoidProxy
    DoWhile _     -> EnvType VoidProxy
    IfThenElse t _ _ _ -> EnvType t
    Effect t _    -> EnvType t

  imap :: forall a b et
        . (forall env' t'. EnvTypeProxy '(env', t') -> Eval (a '(env', t')) -> Eval (b '(env', t')))
       -> CodeF eff a et
       -> CodeF eff b et
  imap f x =
    let env :: EnvironmentProxy (Env et)
        env = case toIndex x of { EnvType _ -> envProxy (Proxy @(Env et)) }
        index :: forall i. ScalarProxy i -> EnvTypeProxy '(Env et, i)
        index i = withEnvironment env (EnvType i)
    in case x of
      Let {} -> error "todo"
      {-
      Let pf n v t b -> (n, typeOfValue v) & \(_ :: Proxy name, tv :: ScalarProxy ty) ->
        let env' = withKnownType tv
                 $ withEnvironment env
                 $ BindingProxy n tv (Proxy @(Env et))
        in Let pf n v t (f (withEnvironment env' (EnvType @( '(name, ty) ': Env et) t)) b)
-}
      Set pf n v -> Set pf n v
      Call t c -> Call t (f (index t) c)
      Block t cs c -> Block t (map (f (index VoidProxy)) cs) (f (index t) c)
      Pure v -> Pure v
      NoOp -> NoOp
      DoWhile c -> DoWhile (f (index BooleanProxy) c)
      IfThenElse t v yes no -> IfThenElse t v (f (index t) yes) (f (index t) no)
      Effect t c -> Effect t c

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

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
      . (NotPresent name env, KnownSymbol name, KnownEnvironment env)
     => Value env ty
     -> ScalarProxy result
     -> Code effs ('(name, ty) ': env) result
     -> Code effs env result
let_ v t = Fix . Let bindingEvidence (Proxy @name) v t


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
     . ( Required name env ~ ty, NotPresent name (env `Without` name)
       , KnownSymbol name, KnownEnvironment env)
    => Value env ty
    -> Code effs env 'VoidT
set = Fix . Set bindingEvidence (Proxy @name)
