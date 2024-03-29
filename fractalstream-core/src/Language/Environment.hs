{-# language UndecidableInstances, RoleAnnotations, AllowAmbiguousTypes #-}

module Language.Environment
  ( type Environment
  , CanAppendTo(..)
  , type Lookup
  , type Required
  , type NotPresent
  , Context(..)
  , type (:**:)
  , contextToEnv
  , envToContext
  , envToContextM
  , mapContext
  , mapContextM
  , forContext
  , forContextM
  , fromContext
  , fromContextM
  , fromContextM_
  , zipContext
  , fromEnvironment
  , fromEnvironmentM
  , getBinding
  , setBinding
  , bindInEnv
  , bindInEnv'
  , EnvironmentProxy(..)
  , EnvTypeProxy(..)
  , envTypeProxy
  , withEnvType
  , bindNameEnv
  , withEnvFromMap
  , envToMap
  , type Env
  , type Ty
  , lemmaEnvTy
  , lemmaEnvTy'
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
  , absentInTail

  , lookupEnv
  , LookupEnvResult(..)
  , lookupEnv'
  , LookupEnvResult'(..)

  , declare
  , endOfDecls

  , sameEnvironment
  , sameEnvType
  ) where

import Language.Type

import GHC.TypeLits
import Fcf (Exp, Eval)
import Data.Type.Equality ((:~:)(..))
import Data.Proxy
import Data.Coerce
import Unsafe.Coerce
import Data.Constraint
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map

---------------------------------------------------------------------------------
-- Environments
---------------------------------------------------------------------------------

-- | An 'Environment' is a map from symbols to 'FSType's. It is represented
-- as a type-level list of pairs; the rest of the API generally requires that
-- each symbol appears one time at most.
type Environment = [(Symbol, FSType)]

---------------------------------------------------------------------------------
-- Value-level proxies for the environment
---------------------------------------------------------------------------------

-- | A singleton datatype for Environment type variables.
data EnvironmentProxy (env :: Environment) where
  EmptyEnvProxy :: EnvironmentProxy '[]
  BindingProxy  :: forall name t env
                 . (KnownSymbol name, NotPresent name env)
                => Proxy name
                -> TypeProxy t
                -> EnvironmentProxy env
                -> EnvironmentProxy ( '(name, t) ': env )

type family Env (et :: (Environment, FSType)) where Env '(env, t) = env
type family Ty  (et :: (Environment, FSType)) where Ty  '(env, t) = t

instance Show (EnvironmentProxy env) where
  show = \case
    BindingProxy name ty env' -> symbolVal name <> ":" <> showType ty <> ", " <> show env'
    EmptyEnvProxy -> "<empty>"

fromEnvironment :: forall env a
                 . EnvironmentProxy env
                -> (forall name t. KnownSymbol name => Proxy name -> TypeProxy t -> a)
                -> [a]
fromEnvironment env0 f = go env0
  where
    go :: EnvironmentProxy env' -> [a]
    go = \case
      EmptyEnvProxy        -> []
      BindingProxy n t env -> f n t : go env

fromEnvironmentM :: forall env m a
                  . Applicative m
                 => EnvironmentProxy env
                 -> (forall name t. KnownSymbol name => Proxy name -> TypeProxy t -> m a)
                 -> m [a]
fromEnvironmentM env0 f = go env0
  where
    go :: EnvironmentProxy env' -> m [a]
    go = \case
      EmptyEnvProxy        -> pure []
      BindingProxy n t env -> (:) <$> f n t <*> go env

declare :: forall name ty env
         . (KnownSymbol name, NotPresent name env)
        => TypeProxy ty
        -> EnvironmentProxy env
        -> EnvironmentProxy ( '(name, ty) ': env)
declare = BindingProxy (Proxy @name)

endOfDecls :: EnvironmentProxy '[]
endOfDecls = EmptyEnvProxy

lemmaEnvTy :: forall et. (et :~: '(Env et, Ty et))
lemmaEnvTy = (unsafeCoerce :: (Int :~: Int) -> (et :~: '(Env et, Ty et))) Refl

lemmaEnvTy' :: forall et. EnvTypeProxy et -> (et :~: '(Env et, Ty et))
lemmaEnvTy' _ = lemmaEnvTy @et

-- | Proxy the type index directly, and the environment
-- index implicitly through the KnownEnvironment constraint
data EnvTypeProxy (et :: (Environment, FSType)) where
  EnvType :: forall env t
           . KnownEnvironment env
          => TypeProxy t
          -> EnvTypeProxy '(env, t)

envTypeProxy :: EnvironmentProxy env
             -> TypeProxy t
             -> EnvTypeProxy '(env, t)
envTypeProxy env t = withEnvironment env (EnvType t)

-- TODO: could move this into the trusted core and avoid the quadratic behavior
withEnvFromMap :: forall t
                . Map String SomeType
               -> (forall env. EnvironmentProxy env -> t)
               -> t
withEnvFromMap m k = go EmptyEnvProxy (Map.toList m)
  where
    go :: EnvironmentProxy env -> [(String, SomeType)] -> t
    go env = \case
      [] -> k env
      ((nameStr, SomeType ty) : etc) -> case someSymbolVal nameStr of
        SomeSymbol name -> case lookupEnv' name env of
          Found' _ _ -> error "impossible internal error! duplicated key in map"
          Absent' proof -> recallIsAbsent proof $
            go (BindingProxy name ty env) etc

envToMap :: EnvironmentProxy env -> Map String SomeType
envToMap = Map.fromList . go
  where
    go :: forall e. EnvironmentProxy e -> [(String, SomeType)]
    go = \case
      BindingProxy name ty env -> (symbolVal name, SomeType ty) : go env
      EmptyEnvProxy -> []

withEnvType :: EnvTypeProxy et
            -> ((et ~ '(Env et, Ty et))
                => EnvironmentProxy (Env et)
                -> TypeProxy (Ty et)
                -> a)
            -> a
withEnvType (EnvType t) k = k (envProxy Proxy) t

sameEnvironment :: EnvironmentProxy env1 -> EnvironmentProxy env2 -> Maybe (env1 :~: env2)
sameEnvironment v1 v2 = case v1 of
  EmptyEnvProxy -> case v2 of { EmptyEnvProxy -> Just Refl; _ -> Nothing }
  BindingProxy name t env -> case v2 of
    BindingProxy name' t' env' ->
      case (,,) <$> sameEnvironment env env' <*> sameHaskellType t t' <*> sameSymbol name name' of
      Just (Refl, Refl, Refl) -> Just Refl
      Nothing           -> Nothing
    _ -> Nothing

sameEnvType :: forall et1 et2. EnvTypeProxy et1 -> EnvTypeProxy et2 -> Maybe (et1 :~: et2)
sameEnvType et1 et2 =
  withEnvType et1 $ \env1 t1 ->
    withEnvType et2 $ \env2 t2 ->
      case sameEnvironment env1 env2 of
        Just Refl -> case sameHaskellType t1 t2 of
          Just Refl -> case (lemmaEnvTy @et1, lemmaEnvTy @et2) of
            (Refl, Refl) -> Just Refl
          Nothing -> Nothing
        Nothing -> Nothing

bindNameEnv :: forall name t env
             . KnownSymbol name
            => Proxy name
            -> TypeProxy t
            -> NameIsAbsent name env
            -> EnvironmentProxy env
            -> EnvironmentProxy ( '(name, t) ': env)
bindNameEnv _ t pf env
  = recallIsAbsent pf
  $ withEnvironment env
  $ withKnownType t
  $ envProxy (Proxy @( '(name, t) ': env))

-- | An implicit environment
class KnownEnvironment (env :: Environment) where
  envProxy :: Proxy env -> EnvironmentProxy env

instance KnownEnvironment '[] where envProxy _ = EmptyEnvProxy
instance (KnownEnvironment env, KnownSymbol name, KnownType t, NotPresent name env)
  => KnownEnvironment ( '(name, t) ': env ) where
  envProxy _ = BindingProxy Proxy (typeProxy @t) (envProxy Proxy)

-- | Make the given environment implicit.
withEnvironment :: EnvironmentProxy env -> (KnownEnvironment env => a) -> a
withEnvironment pxy k = case pxy of
  EmptyEnvProxy -> k
  BindingProxy _ t env -> withEnvironment env (withKnownType t k)

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
newtype NameIsPresent (name :: Symbol) (t :: FSType) (env :: Environment)
  = NameIsPresent TrustMe

-- Prevent coercion of the type parameters
type role NameIsPresent nominal nominal nominal

-- | 'NameIsPresent' represents a proof that the name is absent in the
-- environment.
newtype NameIsAbsent  (name :: Symbol) (env :: Environment)
  = NameIsAbsent TrustMe

-- Prevent coercion of the type parameters
type role NameIsAbsent nominal nominal

isAbsent :: NotPresent name env => NameIsAbsent name env
isAbsent = trustMe

recallIsAbsent :: forall name env a. NameIsAbsent name env -> (NotPresent name env => a) -> a
recallIsAbsent _ k = case (unsafeCoerce :: Dict () -> Dict (NotPresent name env)) Dict of
  Dict -> k

absentInTail :: NameIsPresent name t ( '(name, t) ': env)
             -> NameIsAbsent name env
absentInTail _ = trustMe

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
         -> TypeProxy t
         -> NameIsAbsent name env
         -> NameIsPresent name t ( '(name, t) ': env)
bindName _ _ _ = trustMe

data LookupEnvResult name t env
  = Found (NameIsPresent name t env)
  | WrongType SomeType -- found, but not with the correct type
  | Absent (NameIsAbsent name env)

lookupEnv :: KnownSymbol name => Proxy name -> TypeProxy t -> EnvironmentProxy env -> LookupEnvResult name t env
lookupEnv name ty = \case
  BindingProxy name' ty' env' -> case sameSymbol name name' of
    Just Refl -> case sameHaskellType ty ty' of
      Just Refl -> Found trustMe
      Nothing   -> WrongType (SomeType ty')
    Nothing -> case lookupEnv name ty env' of
      Found _     -> Found trustMe
      Absent _    -> Absent trustMe
      WrongType t -> WrongType t
  EmptyEnvProxy -> Absent trustMe


data LookupEnvResult' name env where
  Found'  :: forall name t env. TypeProxy t -> NameIsPresent name t env -> LookupEnvResult' name env
  Absent' :: forall name env. NameIsAbsent name env -> LookupEnvResult' name env

-- | Look up a name in the environment, when we don't yet know what its type should be.
lookupEnv' :: KnownSymbol name => Proxy name -> EnvironmentProxy env -> LookupEnvResult' name env
lookupEnv' name = \case
  BindingProxy name' ty' env' -> case sameSymbol name name' of
    Just Refl -> Found' ty' trustMe
    Nothing -> case lookupEnv' name env' of
      Found' ty _ -> Found' ty trustMe
      Absent' _   -> Absent' trustMe
  EmptyEnvProxy -> Absent' trustMe

---------------------------------------------------------------------------------
-- Concatenating environments
---------------------------------------------------------------------------------

class CanAppendTo (xs :: Environment) (ys :: Environment) where
  type EnvAppend xs ys :: Environment
  envAppend :: EnvironmentProxy xs
            -> EnvironmentProxy ys
            -> EnvironmentProxy (xs `EnvAppend` ys)
  contextAppend :: Context value xs
                -> Context value ys
                -> Context value (xs `EnvAppend` ys)

instance CanAppendTo '[] ys where
  type EnvAppend '[] ys = ys
  envAppend EmptyEnvProxy ys = ys
  contextAppend EmptyContext ys = ys

instance (KnownSymbol name, NotPresent name xs, NotPresent name ys, CanAppendTo xs ys)
    => CanAppendTo ( '(name,t) ': xs) ys where
  type EnvAppend ( '(name,t) ': xs) ys = '(name,t) ': EnvAppend xs ys
  envAppend (BindingProxy name t xs) ys =
    recallNotPresentInEither @name @xs @ys $
      BindingProxy name t (envAppend xs ys)
  contextAppend (Bind name ty v xs) ys =
    recallNotPresentInEither @name @xs @ys $
      Bind name ty v (contextAppend xs ys)

recallNotPresentInEither
  :: forall name xs ys a
   . (NotPresent name xs, NotPresent name ys)
  => (NotPresent name (xs `EnvAppend` ys) => a)
  -> a
recallNotPresentInEither k =
  case (unsafeCoerce :: Dict () -> Dict (NotPresent name (xs `EnvAppend` ys))) Dict of
    Dict -> k

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
    (name :: Symbol) (env :: [(Symbol, FSType)]) :: Maybe FSType where
  Lookup _ '[] = 'Nothing
  Lookup name ('(name, t) ': _) = 'Just t
  Lookup name (_ ': env) = Lookup name env

-- | Evaluate to the 'FSType' of @name@ in the environment @env@,  or raise a
-- type error at compile-time if the name is not present.
type Required name env = Required_impl name (Lookup name env)

type family Required_impl
    (name :: Symbol) (result :: Maybe FSType) :: FSType where
  Required_impl name 'Nothing =
    TypeError ('Text "No variable named " ':<>:
               'Text name ':<>: 'Text " is in scope")
  Required_impl name ('Just t) = t

-- | Raise a type error at compile-time if the given name is already present
-- in the environment.
type NotPresent name env = NotPresent_impl name (Lookup name env)

type family NotPresent_impl
    (name :: Symbol) (result :: Maybe FSType) :: Constraint where
  NotPresent_impl name ('Just _) =
    TypeError ('Text "The name " ':<>: 'Text name ':<>:
               'Text " shadows another variable in scope")
  NotPresent_impl name 'Nothing = ()

---------------------------------------------------------------------------------
-- Contexts attach a value (with the correct type) to each name
-- defined in the environment.
---------------------------------------------------------------------------------

data Context (value :: Symbol -> FSType -> Exp Type) (env :: Environment) where
  EmptyContext :: forall value. Context value '[]
  Bind :: forall name ty env value
        . (KnownSymbol name, NotPresent name env)
       => Proxy name
       -> TypeProxy ty
       -> Eval (value name ty)
       -> Context value env
       -> Context value ( '(name, ty) ': env)

contextToEnv :: Context value env -> EnvironmentProxy env
contextToEnv = \case
  EmptyContext -> EmptyEnvProxy
  Bind name t _ ctx -> BindingProxy name t (contextToEnv ctx)

envToContext :: forall a env
              . EnvironmentProxy env
             -> (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty))
             -> Context a env
envToContext env0 make = go env0
  where
    go :: forall env'. EnvironmentProxy env' -> Context a env'
    go = \case
      EmptyEnvProxy -> EmptyContext
      BindingProxy name ty env -> Bind name ty (make name ty) (go env)

envToContextM :: forall a env m
               . Monad m
              => EnvironmentProxy env
              -> (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> m (Eval (a name ty)))
              -> m (Context a env)
envToContextM env0 make = go env0
  where
    go :: forall env'. EnvironmentProxy env' -> m (Context a env')
    go = \case
      EmptyEnvProxy -> pure EmptyContext
      BindingProxy name ty env -> do
        v <- make name ty
        Bind name ty v <$> go env

-- | Transform each bound value in the context, creating a new context.
mapContext :: forall a b env
            . (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> Eval (b name ty))
           -> Context a env
           -> Context b env
mapContext f = \case
  EmptyContext       -> EmptyContext
  Bind name ty x ctx -> Bind name ty (f name ty x) (mapContext f ctx)

-- | 'mapContext' with the arguments flipped
forContext :: forall a b env
            . Context a env
           -> (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> Eval (b name ty))
           -> Context b env
forContext x f = mapContext f x

-- | Transform each bound value in the context, creating a new context.
mapContextM :: forall a b env m
             . Applicative m
            =>  (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> m (Eval (b name ty)))
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
           -> (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> m (Eval (b name ty)))
           -> m (Context b env)
forContextM x f = mapContextM f x

-- | Transform each bound value in the context, creating a list of results.
fromContext :: forall a env t
             . (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> t)
            -> Context a env
            -> [t]
fromContext f = \case
  EmptyContext -> []
  Bind name ty x ctx -> f name ty x : fromContext f ctx

-- | Transform each bound value in the context using applicative effects,
-- creating a list of results.
fromContextM :: forall a env t m
              . Applicative m
             => (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> m t)
             -> Context a env
             -> m [t]
fromContextM f = \case
  EmptyContext -> pure []
  Bind name ty x ctx -> (:) <$> f name ty x <*> fromContextM f ctx

-- | Run an effect for each bound value in the context.
fromContextM_ :: forall a env m
              . Applicative m
             => (forall name ty. KnownSymbol name => Proxy name -> TypeProxy ty -> Eval (a name ty) -> m ())
             -> Context a env
             -> m ()
fromContextM_ f = \case
  EmptyContext -> pure ()
  Bind name ty x ctx -> f name ty x *> fromContextM_ f ctx

data (:**:) :: (Symbol -> FSType -> Exp Type)
            -> (Symbol -> FSType -> Exp Type)
            -> (Symbol -> FSType -> Exp Type)
type instance Eval ((f :**: g) name ty) = (Eval (f name ty), Eval (g name ty))

-- | Transform each bound value in the context, creating a new context.
zipContext :: forall a b env
            . Context a env
           -> Context b env
           -> Context (a :**: b) env
zipContext xs0 ys0 = case (xs0, ys0) of
  (EmptyContext, EmptyContext) -> EmptyContext
  (Bind name ty x ctx1, Bind _ _ y ctx2) ->
    Bind name ty (x, y) (zipContext ctx1 ctx2)

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
          Just Refl -> case sameHaskellType (typeProxy @t) ty' of
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
          Just Refl -> case sameHaskellType (typeProxy @t) ty' of
            Just Refl -> Bind name' ty' value ctx
            Nothing   -> error "unreachable due to (NotPresent name env) constraint in Bind constructor"

bindInEnv :: (MonadFail m)
          => String
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (forall name. (KnownSymbol name, NotPresent name env) => EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t
bindInEnv nameStr ty env k = case someSymbolVal nameStr of
  SomeSymbol name -> case lookupEnv' name env of
    Absent' proof -> recallIsAbsent proof (k (bindNameEnv name ty proof env))
    _ -> fail (symbolVal name <> " is defined twice")

bindInEnv' :: (MonadFail m, KnownSymbol name)
          => Proxy name
          -> TypeProxy ty
          -> EnvironmentProxy env
          -> (NotPresent name env => EnvironmentProxy ( '(name, ty) ': env) -> m t)
          -> m t
bindInEnv' name ty env k = case lookupEnv' name env of
  Absent' proof -> recallIsAbsent proof (k (bindNameEnv name ty proof env))
  _ -> fail (symbolVal name <> " is defined twice")
