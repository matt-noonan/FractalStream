{-# language AllowAmbiguousTypes #-}

module Language.Code
  ( module Language.Value
  , module Language.Effect
  , Code
  , Fix(..)
  , CodeF(..)
  , SomeCode(..)
  , mapValues
  , transformValues
  , set
  , let_
  ) where

import Language.Value
import Language.Effect
import Data.Indexed.Functor
import Fcf
import GHC.TypeLits
import Data.Kind

data SomeCode where
  SomeCode :: forall effs env t. Code effs env t -> SomeCode

---------------------------------------------------------------------------------
-- Code
---------------------------------------------------------------------------------

type Code effs env t = Fix (CodeF effs (Pure1 Value)) '(env, t)

instance Show (Code effs env t) where show _ = "<code>"

-- | The Code type is used recursively at different Type parameters,
-- and also at different Environments (in a Let binding). That means
-- we need to use both the environment *and* the type as indices
-- in order to make an indexed functor.
data CodeF (effs :: [Effect])
           (value :: (Environment, FSType) -> Exp Type)
           (code :: (Environment, FSType) -> Exp Type)
           (et :: (Environment, FSType)) where

  Let :: forall name ty env result effs code value
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> TypeProxy ty
      -> Eval (value '(env, ty))
      -> TypeProxy result
      -> Eval (code '( '(name, ty) ': env, result))
      -> CodeF effs value code '(env, result)

  LetBind :: forall name ty env result effs code value
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> TypeProxy ty
      -> Eval (code '(env, ty))
      -> TypeProxy result
      -> Eval (code '( '(name, ty) ': env, result))
      -> CodeF effs value code '(env, result)

  Set :: forall name ty env effs code value
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty env
      -> Proxy name
      -> TypeProxy ty
      -> Eval (value '(env, ty))
      -> CodeF effs value code '(env, 'VoidT)

  SetBind :: forall name ty env effs code value
        . (KnownSymbol name, KnownEnvironment env)
       => NameIsPresent name ty env
       -> Proxy name
       -> TypeProxy ty
       -> Eval (code '(env, ty))
       -> CodeF effs value code '(env, 'VoidT)

  -- | Invoke another chunk of 'Code', using a copy of the environment.
  -- Variable mutations within the called code will not be reflected
  -- back in the caller.
  Call :: forall effs env ty code value
        . KnownEnvironment env
       => TypeProxy ty
       -> Eval (code '(env, ty))
       -> CodeF effs value code '(env, ty)

  -- | A block of statements with VoidT type, followed by a
  -- statement with any time. The type of the block is the
  -- type of the final statement.
  Block :: forall effs env ty code value
         . KnownEnvironment env
        => TypeProxy ty
        -> [Eval (code '(env, 'VoidT))]
        -> Eval (code '(env, ty))
        -> CodeF effs value code '(env, ty)

  -- | Lift a pure value to a bit of code
  Pure :: forall effs env ty code value
        . KnownEnvironment env
       => TypeProxy ty
       -> Eval (value '(env, ty))
       -> CodeF effs value code '(env, ty)

  NoOp :: forall env effs code value
        . KnownEnvironment env
       => CodeF effs value code '(env, 'VoidT)

  -- | Do-while loop
  DoWhile :: forall env effs code value
           . KnownEnvironment env
          => Eval (code '(env, 'BooleanT))
          -> CodeF effs value code '(env, 'VoidT)

  -- | If/else statement
  IfThenElse :: forall env effs t code value
       . KnownEnvironment env
      => TypeProxy t
      -> Eval (value '(env, 'BooleanT))
      -> Eval (code '(env, t))
      -> Eval (code '(env, t))
      -> CodeF effs value code '(env, t)

  -- | Embedded effect sub-language
  Effect :: forall env effs eff t code value
          . (HasEffect eff effs, KnownEnvironment env)
         => Proxy eff
         -> Proxy env
         -> TypeProxy t
         -> eff code '(env, t)
         -> CodeF effs value code '(env, t)

---------------------------------------------------------------------------------
-- Indexed functor instance for Code
---------------------------------------------------------------------------------

instance IFunctor (CodeF eff value) where

  type IndexProxy (CodeF eff value) = EnvTypeProxy

  toIndex = \case
    Let _ _ _ _ t _ -> EnvType t
    LetBind _ _ _ _ t _ -> EnvType t
    Set _ _ _ _   -> EnvType VoidType
    SetBind {}    -> EnvType VoidType
    Call t _      -> EnvType t
    Block t _ _   -> EnvType t
    Pure t _      -> EnvType t
    NoOp          -> EnvType VoidType
    DoWhile _     -> EnvType VoidType
    IfThenElse t _ _ _ -> EnvType t
    Effect _ _ t _ -> EnvType t

  imap :: forall a b et
        . (forall et'. EnvTypeProxy et' -> Eval (a et') -> Eval (b et'))
       -> CodeF eff value a et
       -> CodeF eff value b et
  imap f x =
    let env :: EnvironmentProxy (Env et)
        env = case toIndex x of { EnvType _ -> envProxy (Proxy @(Env et)) }
        index :: forall i. TypeProxy i -> EnvTypeProxy '(Env et, i)
        index i = withEnvironment env (EnvType i)
    in case x of
      Let pf (n :: Proxy name) (vt :: TypeProxy vt) v t b ->
            recallIsAbsent (removeName @name pf) $
              let env' :: EnvironmentProxy ( '(name, vt) ': Env et)
                  env' = withKnownType vt
                       $ withEnvironment env
                       $ BindingProxy n vt (envProxy (Proxy @(Env et)))
                  index' :: forall i r
                          . TypeProxy i
                         -> TypeProxy r
                         -> EnvTypeProxy '( '(name, i) ': Env et, r)
                  index' i r = withKnownType i
                             $ withEnvironment env'
                             $ EnvType @( '(name, i) ': Env et) r
              in withEnvironment env'
                   (Let pf n vt v t (f (index' vt t) b))
      LetBind pf (n :: Proxy name) (vt :: TypeProxy vt) cv t b ->
        recallIsAbsent (removeName @name pf) $
          let env' :: EnvironmentProxy ( '(name, vt) ': Env et)
              env' = withKnownType vt
                     $ withEnvironment env
                     $ BindingProxy n vt (envProxy (Proxy @(Env et)))
              index' :: forall i r
                      . TypeProxy i
                     -> TypeProxy r
                     -> EnvTypeProxy '( '(name, i) ': Env et, r)
              index' i r = withKnownType i
                         $ withEnvironment env' (EnvType @( '(name, i) ': Env et) r)
          in withEnvironment env' (LetBind pf n vt (f (index vt) cv) t (f (index' vt t) b))
      Set pf n ty v -> Set pf n ty v
      SetBind pf n ty c -> SetBind pf n ty (f (index ty) c)
      Call t c -> Call t (f (index t) c)
      Block t cs c -> Block t (map (f (index VoidType)) cs) (f (index t) c)
      Pure t v -> Pure t v
      NoOp -> NoOp
      DoWhile c -> DoWhile (f (index BooleanType) c)
      IfThenElse t v yes no -> IfThenElse t v (f (index t) yes) (f (index t) no)
      Effect e en t c -> Effect e en t (imap f c)

---------------------------------------------------------------------------------
-- Mapping over `value`; CodeF is an indexed bifunctor
---------------------------------------------------------------------------------

mapValues :: forall a b eff code et
          . (forall et'. EnvTypeProxy et' -> Eval (a et') -> Eval (b et'))
         -> CodeF eff a code et
         -> CodeF eff b code et
mapValues f x =
  let env :: EnvironmentProxy (Env et)
      env = case toIndex x of { EnvType _ -> envProxy (Proxy @(Env et)) }
      index :: forall i. TypeProxy i -> EnvTypeProxy '(Env et, i)
      index i = withEnvironment env (EnvType i)
  in case x of
      Let pf n vt v t b -> Let pf n vt (f (index vt) v) t b
      LetBind pf n vt cv t b -> LetBind pf n vt cv t b
      Set pf n ty v -> Set pf n ty (f (index ty) v)
      SetBind pf n ty c -> SetBind pf n ty c
      Call t c -> Call t c
      Block t cs c -> Block t cs c
      Pure t v -> Pure t (f (index t) v)
      NoOp -> NoOp
      DoWhile c -> DoWhile c
      IfThenElse t v yes no -> IfThenElse t (f (index BooleanType) v) yes no
      Effect e en t c -> Effect e en t c

transformValues :: forall eff env result
                . (forall et. Value et -> Value et)
               -> Code eff env result
               -> Code eff env result
transformValues f = indexedFold phi
  where
    phi :: forall et
         . CodeF eff (Pure1 Value) (Pure1 (Fix (CodeF eff (Pure1 Value)))) et
        -> Fix (CodeF eff (Pure1 Value)) et
    phi x = Fix (mapValues (const f) x)

---------------------------------------------------------------------------------
-- Indexed traversable instance
---------------------------------------------------------------------------------

instance ITraversable (CodeF effs value) where
  isequence = \case
    Let pf n vt v t c -> Let pf n vt v t <$> c
    LetBind pf n vt cv t c -> LetBind pf n vt <$> cv <*> pure t <*> c
    Set pf n ty v -> pure (Set pf n ty v)
    SetBind pf n ty c -> SetBind pf n ty <$> c
    Call t c -> Call t <$> c
    Block t block final -> Block t <$> sequenceA block <*> final
    Pure t v -> pure (Pure t v)
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
     => Value '(env, ty)
     -> TypeProxy result
     -> Code effs ('(name, ty) ': env) result
     -> Code effs env result
let_ v t = Fix . Let bindingEvidence (Proxy @name) (typeOfValue v) v t


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
    => Value '(env, ty)
    -> Code effs env 'VoidT
set v = Fix (Set bindingEvidence (Proxy @name) (typeOfValue v) v)
