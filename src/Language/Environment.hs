{-# language UndecidableInstances, RoleAnnotations #-}

module Language.Environment
  ( type Environment
  , type Lookup
  , type Required
  , type NotPresent
  , Context(..)
  , mapContext
  , mapContextM
  , forContext
  , forContextM
  , fromContext
  , fromContextM
  , fromContextM_
  , getBinding
  , setBinding
  , EnvironmentProxy(..)
  , KnownEnvironment(..)
  , withEnvironment

  , NameIsPresent
  , NameIsAbsent
  , typeIsUnique
  , bindName
  , withoutName
  , removeName
  , type Without
  , bindingEvidence
  , isAbsent
  , recallIsAbsent

  , lookupEnv
  , LookupEnvResult(..)
  , lookupEnv'
  , LookupEnvResult'(..)

  ) where

import Language.Type

import GHC.TypeLits
import Fcf (Exp, Eval)
import Data.Type.Equality ((:~:)(..))
import Data.Proxy
import Data.Coerce
import Unsafe.Coerce
import Data.Constraint

---------------------------------------------------------------------------------
-- Environments
---------------------------------------------------------------------------------

-- | An 'Environment' is a map from symbols to 'Type's.
type Environment = [(Symbol, Type)]

---------------------------------------------------------------------------------
-- Value-level proxies for the environment
---------------------------------------------------------------------------------

-- | A singleton datatype for Environment type variables.
data EnvironmentProxy (env :: Environment) where
  EmptyEnvProxy :: EnvironmentProxy '[]
  BindingProxy  :: forall name t env
                 . (KnownSymbol name, KnownEnvironment env, NotPresent name env)
                => Proxy name
                -> ScalarProxy t
                -> Proxy env
                -> EnvironmentProxy ( '(name, t) ': env )

-- | An implicit environment
class KnownEnvironment (env :: Environment) where
  envProxy :: Proxy env -> EnvironmentProxy env

instance KnownEnvironment '[] where envProxy _ = EmptyEnvProxy
instance (KnownEnvironment env, KnownSymbol name, KnownType t, NotPresent name env)
  => KnownEnvironment ( '(name, t) ': env ) where
  envProxy _ = BindingProxy Proxy (typeProxy @t) Proxy

-- | Make the given environment implicit.
withEnvironment :: EnvironmentProxy env -> (KnownEnvironment env => a) -> a
withEnvironment pxy k = case pxy of
  EmptyEnvProxy -> k
  BindingProxy _ t _ -> withKnownType t k

---------------------------------------------------------------------------------
-- "Proofs" that names are present in the environment and have the right type.
-- These proofs are not enforced by the type system, but they are protected
-- by the module abstraction barrier. As a result, this is the "trusted core"
-- for the proofs. If the code below is correct, then all external uses of
-- the proofs are sound.
---------------------------------------------------------------------------------

data TrustMe = TrustMe

trustMe :: Coercible TrustMe t => t
trustMe = coerce TrustMe

-- | 'NameIsPresent' represents a proof that the name is present in the
-- environment *exactly one time*, and also has the given type.
newtype NameIsPresent (name :: Symbol) (t :: Type) (env :: Environment)
  = NameIsPresent TrustMe

-- Prevent coercion of the type parameters
type role NameIsPresent nominal nominal nominal

-- | 'NameIsPresent' represents a proof that the name is absent in the
-- environment.
newtype NameIsAbsent  (name :: Symbol) (env :: Environment)
  = NameIsAbsent TrustMe

isAbsent :: NotPresent name env => NameIsAbsent name env
isAbsent = trustMe

recallIsAbsent :: forall name env a. NameIsAbsent name env -> (NotPresent name env => a) -> a
recallIsAbsent _ k = case (unsafeCoerce :: Dict () -> Dict (NotPresent name env)) Dict of
  Dict -> k

-- Prevent coercion of the type parameters
type role NameIsAbsent nominal nominal

type family Without (env :: Environment) (name :: Symbol) :: Environment where
  Without ( '(name,  t) ': env) name = env
  Without ( '(name', t) ': env) name = '(name', t) ': (env `Without` name)

typeIsUnique :: forall name t t' env
              . NameIsPresent name t  env
             -> NameIsPresent name t' env
             -> (t :~: t')
typeIsUnique _ _ = unsafeCoerce (Refl :: () :~: ())

-- | If @name@ and @name'@ are both present in the environment, then either
-- @name@ and @name'@ are identical, or else @name'@ is still present in the
-- environment after @name@ is removed.
withoutName :: forall name name' t t' env
             . (KnownSymbol name, KnownSymbol name')
            => NameIsPresent name  t  env
            -> NameIsPresent name' t' env
            -> Either (name :~: name', t :~: t') (NameIsPresent name' t' (env `Without` name))
withoutName pf1 pf2 = case sameSymbol (Proxy @name) (Proxy @name') of
  Just Refl -> Left (Refl, typeIsUnique pf1 pf2)
  Nothing   -> Right trustMe

removeName :: forall name t env
            . NameIsPresent name t env
           -> NameIsAbsent name (env `Without` name)
removeName _ = trustMe

bindingEvidence :: forall name t env
                 . (Required name env ~ t, NotPresent name (env `Without` name))
                => NameIsPresent name t env
bindingEvidence = trustMe

bindName :: forall name t env
          . Proxy name
         -> ScalarProxy t
         -> NameIsAbsent name env
         -> NameIsPresent name t ( '(name, t) ': env)
bindName _ _ _ = trustMe

data LookupEnvResult name t env
  = Found (NameIsPresent name t env)
  | WrongType -- found, but not with the correct type
  | Absent (NameIsAbsent name env)

lookupEnv :: KnownSymbol name => Proxy name -> ScalarProxy t -> EnvironmentProxy env -> LookupEnvResult name t env
lookupEnv name ty = \case
  BindingProxy name' ty' env' -> case sameSymbol name name' of
    Just Refl -> case sameScalarType ty ty' of
      Just Refl -> Found trustMe
      Nothing   -> WrongType
    Nothing -> case lookupEnv name ty (envProxy env') of
      Found _   -> Found trustMe
      Absent _  -> Absent trustMe
      WrongType -> WrongType
  EmptyEnvProxy -> Absent trustMe


data LookupEnvResult' name env where
  Found'  :: forall name t env. ScalarProxy t -> NameIsPresent name t env -> LookupEnvResult' name env
  Absent' :: forall name env. NameIsAbsent name env -> LookupEnvResult' name env

-- | Look up a name in the environment, when we don't yet know what its type should be.
lookupEnv' :: KnownSymbol name => Proxy name -> EnvironmentProxy env -> LookupEnvResult' name env
lookupEnv' name = \case
  BindingProxy name' ty' env' -> case sameSymbol name name' of
    Just Refl -> Found' ty' trustMe
    Nothing -> case lookupEnv' name (envProxy env') of
      Found' ty _ -> Found' ty trustMe
      Absent' _   -> Absent' trustMe
  EmptyEnvProxy -> Absent' trustMe


---------------------------------------------------------------------------------
-- Constraints to require that a certain name is or is not present
-- in the environment
---------------------------------------------------------------------------------

-- | Look up the 'Type' of a name in the environment.
--
-- >>> :k! Lookup "bar" '[ '("foo", 'BooleanT), '("bar", 'IntegerT) ]
--
-- 'Just 'IntegerT
--
-- >>> :k! Lookup "quux" '[ '("foo", 'BooleanT), '("bar", 'IntegerT) ]
--
-- 'Nothing
type family Lookup
    (name :: Symbol) (env :: [(Symbol, Type)]) :: Maybe Type where
  Lookup _ '[] = 'Nothing
  Lookup name ('(name, t) ': _) = 'Just t
  Lookup name (_ ': env) = Lookup name env

-- | Evaluate to the 'Type' of @name@ in the environment @env@,  or raise a
-- type error at compile-time if the name is not present.
type Required name env = Required_impl name (Lookup name env)

type family Required_impl
    (name :: Symbol) (result :: Maybe Type) :: Type where
  Required_impl name 'Nothing =
    TypeError ('Text "No variable named " ':<>:
               'Text name ':<>: 'Text " is in scope")
  Required_impl name ('Just t) = t

-- | Raise a type error at compile-time if the given name is already present
-- in the environment.
type NotPresent name env = NotPresent_impl name (Lookup name env)

type family NotPresent_impl
    (name :: Symbol) (result :: Maybe Type) :: Constraint where
  NotPresent_impl name ('Just _) =
    TypeError ('Text "The name " ':<>: 'Text name ':<>:
               'Text " shadows another variable in scope")
  NotPresent_impl name 'Nothing = ()

---------------------------------------------------------------------------------
-- Contexts attach a value (with the correct type) to each name
-- defined in the environment.
---------------------------------------------------------------------------------

data Context (value :: Symbol -> Type -> Exp *) (env :: Environment) where
  EmptyContext :: forall value. Context value '[]
  Bind :: forall name ty env value
        . KnownSymbol name
       => Proxy name
       -> ScalarProxy ty
       -> Eval (value name ty)
       -> Context value env
       -> Context value ( '(name, ty) ': env)

-- | Transform each bound value in the context, creating a new context.
mapContext :: forall a b env
            . (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> Eval (b name ty))
           -> Context a env
           -> Context b env
mapContext f = \case
  EmptyContext       -> EmptyContext
  Bind name ty x ctx -> Bind name ty (f name ty x) (mapContext f ctx)

-- | 'mapContext' with the arguments flipped
forContext :: forall a b env
            . Context a env
           -> (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> Eval (b name ty))
           -> Context b env
forContext x f = mapContext f x

-- | Transform each bound value in the context, creating a new context.
mapContextM :: forall a b env m
             . Applicative m
            =>  (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> m (Eval (b name ty)))
           -> Context a env
           -> m (Context b env)
mapContextM f = \case
  EmptyContext       -> pure EmptyContext
  Bind name ty x ctx ->
    Bind name ty <$> f name ty x
                 <*> mapContextM @a @b f ctx

-- | 'mapContextM' with the arguments flipped
forContextM :: forall a b env m
            . Applicative m
           => Context a env
           -> (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> m (Eval (b name ty)))
           -> m (Context b env)
forContextM x f = mapContextM f x

-- | Transform each bound value in the context, creating a list of results.
fromContext :: forall a env t
             . (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> t)
            -> Context a env
            -> [t]
fromContext f = \case
  EmptyContext -> []
  Bind name ty x ctx -> f name ty x : fromContext f ctx

-- | Transform each bound value in the context using applicative effects,
-- creating a list of results.
fromContextM :: forall a env t m
              . Applicative m
             => (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> m t)
             -> Context a env
             -> m [t]
fromContextM f = \case
  EmptyContext -> pure []
  Bind name ty x ctx -> (:) <$> f name ty x <*> fromContextM f ctx

-- | Run an effect for each bound value in the context.
fromContextM_ :: forall a env m
              . Applicative m
             => (forall name ty. Proxy name -> ScalarProxy ty -> Eval (a name ty) -> m ())
             -> Context a env
             -> m ()
fromContextM_ f = \case
  EmptyContext -> pure ()
  Bind name ty x ctx -> f name ty x *> fromContextM_ f ctx

-- | Look up the value associated to a name in the context.
-- You do this by presenting a proof that the name is
-- available in the environment, with the given type.
getBinding :: forall name t env value
            . (KnownSymbol name, KnownType t)
           => Context value env
           -> NameIsPresent name t env
           -> Eval (value name t)
getBinding ctx0 _pf = go ctx0
  where
    go :: forall env'. Context value env' -> Eval (value name t)
    go = \case
      EmptyContext -> error "unreachable due to (Required name env ~ t) constraint"
      Bind name' ty' v ctx ->
        case sameSymbol (Proxy @name) name' of
          Nothing -> go ctx
          Just Refl -> case sameScalarType (typeProxy @t) ty' of
            Just Refl -> v
            Nothing   -> error "unreachable due to (NotPresent name env) constraint in Bind constructor"

-- | Set the value associated to a name in the context.
-- You do this by presenting a proof that the name is
-- available in the environment, with the given type,
-- along with an appropriately-typed value to update with.
setBinding :: forall name t env value
            . (KnownSymbol name, KnownType t)
           => NameIsPresent name t env
           -> Eval (value name t)
           -> Context value env
           -> Context value env
setBinding _pf value = go
  where
    go :: forall env'. Context value env' -> Context value env'
    go = \case
      EmptyContext -> error "unreachable due to (Required name env ~ t) constraint"
      Bind name' ty' v ctx ->
        case sameSymbol (Proxy @name) name' of
          Nothing -> Bind name' ty' v (go ctx)
          Just Refl -> case sameScalarType (typeProxy @t) ty' of
            Just Refl -> Bind name' ty' value ctx
            Nothing   -> error "unreachable due to (NotPresent name env) constraint in Bind constructor"
