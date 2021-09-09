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

  LetBind :: forall name ty env result effs code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> ScalarProxy ty
      -> Eval (code '(env, ty))
      -> ScalarProxy result
      -> Eval (code '( '(name, ty) ': env, result))
      -> CodeF effs code '(env, result)

  Set :: forall name ty env effs code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty env
      -> Proxy name
      -> Value env ty
      -> CodeF effs code '(env, 'VoidT)

  SetBind :: forall name ty env effs code
        . (KnownSymbol name, KnownEnvironment env)
       => NameIsPresent name ty env
       -> Proxy name
       -> ScalarProxy ty
       -> Eval (code '(env, ty))
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

  -- | Embedded effect sub-language
  Effect :: forall env effs eff t code
          . (HasEffect eff effs, KnownEnvironment env)
         => Proxy eff
         -> Proxy env
         -> ScalarProxy t
         -> eff code '(env, t)
         -> CodeF effs code '(env, t)

---------------------------------------------------------------------------------
-- Indexed functor instance for Code
---------------------------------------------------------------------------------

instance IFunctor (CodeF eff) where

  type IndexProxy (CodeF eff) = EnvTypeProxy

  toIndex = \case
    Let _ _ _ t _ -> EnvType t
    LetBind _ _ _ _ t _ -> EnvType t
    Set _ _ _     -> EnvType VoidProxy
    SetBind {}    -> EnvType VoidProxy
    Call t _      -> EnvType t
    Block t _ _   -> EnvType t
    Pure v        -> EnvType (typeOfValue v)
    NoOp          -> EnvType VoidProxy
    DoWhile _     -> EnvType VoidProxy
    IfThenElse t _ _ _ -> EnvType t
    Effect _ _ t _ -> EnvType t

  imap :: forall a b et
        . (forall et'. EnvTypeProxy et' -> Eval (a et') -> Eval (b et'))
       -> CodeF eff a et
       -> CodeF eff b et
  imap f x =
    let env :: EnvironmentProxy (Env et)
        env = case toIndex x of { EnvType _ -> envProxy (Proxy @(Env et)) }
        index :: forall i. ScalarProxy i -> EnvTypeProxy '(Env et, i)
        index i = withEnvironment env (EnvType i)
    in case x of
      Let pf (n :: Proxy name) (v :: Value (Env et) vt) t b ->
        recallIsAbsent (removeName @name pf) $
          let env' :: EnvironmentProxy ( '(name, vt) ': Env et)
              env' = withKnownType (typeOfValue v)
                     $ withEnvironment env
                     $ BindingProxy n (typeOfValue v) (envProxy (Proxy @(Env et)))
              index' :: forall i r
                      . ScalarProxy i
                     -> ScalarProxy r
                     -> EnvTypeProxy '( '(name, i) ': Env et, r)
              index' i r = withKnownType i
                         $ withEnvironment env' (EnvType @( '(name, i) ': Env et) r)
          in withEnvironment env' (Let pf n v t (f (index' (typeOfValue v) t) b))
      LetBind pf (n :: Proxy name) (vt :: ScalarProxy vt) cv t b ->
        recallIsAbsent (removeName @name pf) $
          let env' :: EnvironmentProxy ( '(name, vt) ': Env et)
              env' = withKnownType vt
                     $ withEnvironment env
                     $ BindingProxy n vt (envProxy (Proxy @(Env et)))
              index' :: forall i r
                      . ScalarProxy i
                     -> ScalarProxy r
                     -> EnvTypeProxy '( '(name, i) ': Env et, r)
              index' i r = withKnownType i
                         $ withEnvironment env' (EnvType @( '(name, i) ': Env et) r)
          in withEnvironment env' (LetBind pf n vt (f (index vt) cv) t (f (index' vt t) b))
      Set pf n v -> Set pf n v
      SetBind pf n ty c -> SetBind pf n ty (f (index ty) c)
      Call t c -> Call t (f (index t) c)
      Block t cs c -> Block t (map (f (index VoidProxy)) cs) (f (index t) c)
      Pure v -> Pure v
      NoOp -> NoOp
      DoWhile c -> DoWhile (f (index BooleanProxy) c)
      IfThenElse t v yes no -> IfThenElse t v (f (index t) yes) (f (index t) no)
      Effect e en t c -> Effect e en t (imap f c)

---------------------------------------------------------------------------------
-- Indexed traversable instance
---------------------------------------------------------------------------------

instance ITraversable (CodeF effs) where
  isequence = \case
    Let pf n v t c -> Let pf n v t <$> c
    LetBind pf n vt cv t c -> LetBind pf n vt <$> cv <*> pure t <*> c
    Set pf n v -> pure (Set pf n v)
    SetBind pf n ty c -> SetBind pf n ty <$> c
    Call t c -> Call t <$> c
    Block t block final -> Block t <$> sequenceA block <*> final
    Pure v -> pure (Pure v)
    NoOp -> pure NoOp
    DoWhile body -> DoWhile <$> body
    IfThenElse t v yes no -> IfThenElse t v <$> yes <*> no
    Effect eff env t e -> Effect eff env t <$> isequence e

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
