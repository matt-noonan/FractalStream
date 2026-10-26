{-# language AllowAmbiguousTypes #-}

module Language.Code
  ( module Language.Value
  , Code
  , CodeF(..)
  , SomeCode(..)
  , StartOrEnd(..)
  , transformValues
  , transformValuesM
  , set
  , let_
  , letInEnv
  , usedVarsInCode
  , usedVarsInValue
  , checkDefiniteAssignment
  , pprintCode
  , Unit
  , UnitET
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Draw
import Data.Indexed.Functor
import qualified Data.Set as Set
import Control.Monad.Identity (runIdentity)

data SomeCode where
  SomeCode :: forall env. Code env -> SomeCode

---------------------------------------------------------------------------------
-- Code
---------------------------------------------------------------------------------

type Code = CodeF (FIX CodeF)

instance Show (Code env) where show _ = "<code>"

-- | The Code type is used recursively at different Type parameters,
-- and also at different Environments (in a Let binding). That means
-- we need to use both the environment *and* the type as indices
-- in order to make an indexed functor.
data CodeF (code :: Environment -> Exp Type)
           (env :: Environment) where

  -- | Define a new variable scoped to the given Code.
  Let :: forall name ty env code
       . (KnownSymbol name, KnownEnvironment env)
      => NameIsPresent name ty ( '(name, ty) ': env)
      -> Proxy (name :: Symbol)
      -> Value '(env, ty)
      -> Eval (code ( '(name, ty) ': env ))
      -> CodeF code env

  -- | Update the value of a variable
  Set :: forall name ty env code
        . (KnownSymbol name, KnownEnvironment env)
       => NameIsPresent name ty env
       -> Proxy name
       -> Value '(env, ty)
       -> CodeF code env

  -- | A block of statements with VoidT type, followed by a
  -- statement with any type. The type of the block is the
  -- type of the final statement.
  Block :: forall env code
         . KnownEnvironment env
        => [Eval (code env)]
        -> CodeF code env

  -- | Do nothing
  NoOp :: forall env code
        . KnownEnvironment env
       => CodeF code env

  -- | Do-while loop
  DoWhile :: forall env code
           . KnownEnvironment env
          => Value '(env, 'BooleanT)
          -> Eval (code env)
          -> CodeF code env

  -- | If/else statement
  IfThenElse :: forall env code
       . KnownEnvironment env
      => Value '(env, 'BooleanT)
      -> Eval (code env)
      -> Eval (code env)
      -> CodeF code env

  -- | Draw commands
  DrawCommand :: forall env code
               . KnownEnvironment env
              => Draw env
              -> CodeF code env

  -- | List commands
  Lookup :: forall name ty env code item
          . (KnownSymbol name, KnownSymbol item)
         => NameIsPresent name ('ListT ty) env
         -> Proxy name
         -> TypeProxy ('ListT ty)
         -> Proxy item
         -> NameIsAbsent item env
         -> EnvironmentProxy ('(item, ty) ': env)
         -> EnvironmentProxy env
         -> Value '( '(item, ty) ': env, 'BooleanT)
         -- ^ predicate to test for
         -> Eval (code ( '(item, ty) ': env))
         -- ^ action to run on the first item matching the predicate
         -> Maybe (Eval (code env))
         -- ^ optional fallback action to run if there was no match
         -> CodeF code env

  ForEach :: forall name ty env code item
           . (KnownSymbol name, KnownSymbol item)
          => NameIsPresent name ('ListT ty) env
          -> Proxy name
          -> TypeProxy ('ListT ty)
          -> Proxy item
          -> NameIsAbsent item env
          -> EnvironmentProxy env
          -> EnvironmentProxy ('(item, ty) ': env)
          -> Eval (code ( '( item, ty) ': env))
          -> CodeF code env

data StartOrEnd = Start | End

---------------------------------------------------------------------------------
-- Indexed functor instance for Code
---------------------------------------------------------------------------------

instance IFunctor CodeF where

  type IndexProxy CodeF = EnvironmentProxy

  toIndex = \case
    Let {} -> envProxy Proxy
    Set {} -> envProxy Proxy
    Block {} -> envProxy Proxy
    NoOp -> envProxy Proxy
    DoWhile {} -> envProxy Proxy
    IfThenElse {} -> envProxy Proxy
    DrawCommand {} -> envProxy Proxy
    Lookup _ _ _ _ _ _ env _ _ _ -> env
    ForEach _ _ _ _ _ env _ _ -> env

  imap :: forall a b env
        . (forall env'. EnvironmentProxy env' -> Eval (a env') -> Eval (b env'))
       -> CodeF a env
       -> CodeF b env
  imap f code =
    let env = toIndex code
    in case code of
      Let pf (n :: Proxy name) v e -> recallIsAbsent (removeName @name pf) $
        Let pf n v (f (BindingProxy n (typeOfValue v) env) e)
      Set pf n v -> Set pf n v
      Block body -> Block (map (f env) body)
      NoOp -> NoOp
      DoWhile cond body -> DoWhile cond (f env body)
      IfThenElse cond yes no -> IfThenElse cond (f env yes) (f env no)
      DrawCommand d -> DrawCommand d
      Lookup pf n t i pf' env'' env' v body alt -> Lookup pf n t i pf' env'' env' v (f env'' body) (fmap (f env') alt)
      ForEach pf n t i pf' env' env'' body -> ForEach pf n t i pf' env' env'' (f env'' body)

---------------------------------------------------------------------------------
-- Apply an operation to the @Value@s in a @Code@
---------------------------------------------------------------------------------

transformValuesM :: forall env m
                  . Monad m
                 => (forall et. Value et -> m (Value et))
                 -> Code env
                 -> m (Code env)
transformValuesM f = indexedFoldM @(FIX CodeF) (transformValuesM' f)

transformValuesM' :: forall env code m
                   . Monad m
                  => (forall et. Value et -> m (Value et))
                  -> CodeF code env
                  -> m (CodeF code env)
transformValuesM' f = \case
  Let pf n v e -> Let pf n <$> f v <*> pure e
  Set pf n v -> Set pf n <$> f v
  DoWhile cond body -> DoWhile <$> f cond <*> pure body
  IfThenElse cond yes no -> IfThenElse <$> f cond <*> pure yes <*> pure no
  Block cmds -> pure $ Block cmds
  NoOp -> pure NoOp
  DrawCommand d -> DrawCommand <$> transformDrawValuesM f d
  Lookup pf n t i pf' env' env v body alt ->
    Lookup pf n t i pf' env' env <$> f v <*> pure body <*> pure alt
  ForEach pf n t i pf' env env' body -> pure (ForEach pf n t i pf' env env' body)

transformValues :: forall env
                 . (forall et. Value et -> Value et)
                -> Code env
                -> Code env
transformValues f = runIdentity . transformValuesM (pure . f)

---------------------------------------------------------------------------------
-- Indexed traversable instance
---------------------------------------------------------------------------------

instance ITraversable CodeF where
  isequence = \case
    Let pf n v c -> Let pf n v <$> c
    Set pf n v -> pure (Set pf n v)
    Block block -> Block  <$> sequenceA block
    NoOp -> pure NoOp
    DoWhile c body -> DoWhile c <$> body
    IfThenElse v yes no -> IfThenElse v <$> yes <*> no
    DrawCommand d -> pure (DrawCommand d)
    Lookup pf n t i pf' env' env v body alt ->
      Lookup pf n t i pf' env' env v <$> body <*> sequenceA alt
    ForEach pf n t i pf' env env' body -> ForEach pf n t i pf' env env' <$> body

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
--     Let bindingEvidence (Proxy @"foo") value code
--
let_ :: forall name env ty
      . (NotPresent name env, KnownSymbol name, KnownEnvironment env)
     => Value '(env, ty)
     -> Code ('(name, ty) ': env)
     -> Code env
let_ = Let bindingEvidence (Proxy @name)

letInEnv :: forall name env ty
          . (KnownSymbol name, KnownType ty)
         => (KnownEnvironment env => Value '(env, ty))
         -> (EnvironmentProxy ( '(name, ty) ': env ), Code ( '(name, ty) ': env))
         -> (EnvironmentProxy env, Code env)
letInEnv x (BindingProxy _ _ e, c) = withEnvironment e (e, let_ x c)

-- | Set the value of a variable.
-- This is the same as using the 'Set' constructor directly,
-- except that it saves you the syntactic noise of using a
-- 'Proxy', by using type applications instead.
--
--     set @"foo" value
--
-- vs
--
--     Set bindingEvidence (Proxy @"foo") value
--
set :: forall name env ty
     . ( Required name env ~ ty, NotPresent name (env `Without` name)
       , KnownSymbol name, KnownEnvironment env)
    => Value '(env, ty)
    -> Code env
set = Set bindingEvidence (Proxy @name)

-- | Find all of the variables that this code depends on
gatherUsedVarsInCode :: CodeF Unit env
                     -> State (Set String) ()
gatherUsedVarsInCode = \case
  Let _ name v _ -> do
    usedVarsInValue v
    modify' (Set.delete (symbolVal name))
  Lookup _ name _ item _ _ _ v _ _ -> do
    usedVarsInValue v
    modify' (Set.delete (symbolVal item) .
             Set.insert (symbolVal name))
  ForEach _ name _ item _ _ _ _ -> do
    modify' (Set.delete (symbolVal item) .
             Set.insert (symbolVal name))
  other -> void (transformValuesM' (\v -> usedVarsInValue v >> pure v) other)

usedVarsInValue :: Value et -> State (Set String) ()
usedVarsInValue = indexedFoldM @UnitET phi
  where
    phi :: forall et'. ValueF UnitET et' -> State (Set String) ()
    phi = \case
      Var name _ _ -> modify' (Set.insert (symbolVal name))
      LocalLet name _ _ _ _ _ -> modify' (Set.delete (symbolVal name))
      Remove name _ _ _ _ -> modify' (Set.delete (symbolVal name))
      Find name _ _ _ _ _ -> modify' (Set.delete (symbolVal name))
      Transform name _ _ _ _ _ -> modify' (Set.delete (symbolVal name))
      _ -> pure ()

usedVarsInCode :: Code env -> State (Set String) ()
usedVarsInCode = indexedFoldM @Unit gatherUsedVarsInCode

-- | Check that every one of the given @targets@ is definitely 'Set'
-- by the end of this code, and never read before its own first
-- assignment anywhere this code might read it -- the same discipline
-- Java applies to a @final@ local. Written for
-- 'Actor.Ensemble.buildComplexViewer's own handling of a @prepare
-- name1 : Type1, ...:@ block's declared output variables, but not
-- specific to them: just a generic property of a set of names and a
-- piece of code.
--
-- Deliberately conservative about control flow, in exchange for one
-- simple rule that's always sound (never accepts code that could
-- genuinely read a not-yet-assigned target) rather than a precise but
-- intricate one:
--
--   * An @if@\/@else@ only credits an assignment present in *both*
--     branches (if only one branch assigns a target, a run that takes
--     the other branch never would have).
--   * A loop body (@while@\/@until@\/@repeat@, which all compile down
--     to the same 'DoWhile') never credits anything it assigns, even
--     a @repeat@-style loop that's guaranteed at least one iteration --
--     treating every loop the same way avoids a separate "is this
--     guaranteed to run" case while staying sound either way. Its own
--     reads are still checked, just never given credit afterward.
--   * 'Lookup'\/'ForEach'\/'DrawCommand' are treated the same way: any
--     read anywhere inside is checked (reusing 'usedVarsInCode' on the
--     whole node, which already excludes each one's own bound
--     iteration variable), but nothing inside is ever credited.
checkDefiniteAssignment :: Set String -> Code env -> Either String ()
checkDefiniteAssignment targets code0 = do
  assigned <- go Set.empty code0
  case Set.toList (targets Set.\\ assigned) of
    []    -> Right ()
    (n:_) -> Left ("`" ++ n ++ "` is never assigned.")
  where
    go :: forall env'. Set String -> Code env' -> Either String (Set String)
    go assigned = \case
      Let _ _ val rest -> do
        checkValue assigned val
        go assigned rest
      Set _ name val -> do
        checkValue assigned val
        let n = symbolVal name
        pure (if n `Set.member` targets then Set.insert n assigned else assigned)
      Block stmts -> foldM go assigned stmts
      NoOp -> pure assigned
      IfThenElse cond yes no -> do
        checkValue assigned cond
        assignedYes <- go assigned yes
        assignedNo  <- go assigned no
        pure (assigned `Set.union` (assignedYes `Set.intersection` assignedNo))
      other -> do
        let used = execState (usedVarsInCode other) Set.empty
        case Set.toList ((targets Set.\\ assigned) `Set.intersection` used) of
          []    -> pure assigned
          (n:_) -> unassignedRead n

    checkValue :: Set String -> Value et -> Either String ()
    checkValue assigned val =
      let used = execState (usedVarsInValue val) Set.empty
      in case Set.toList ((targets Set.\\ assigned) `Set.intersection` used) of
        []    -> Right ()
        (n:_) -> unassignedRead n

    unassignedRead :: String -> Either String a
    unassignedRead n = Left ("`" ++ n ++ "` is read before `prepare` assigns it.")

data String_ :: Environment -> Exp Type
type instance Eval (String_ env) = String

pprintCode :: Code env -> String
pprintCode = unlines . go
  where
    name n = "`" ++ symbolVal n ++ "`"
    go, indented :: forall env'. Code env' -> [String]
    indented = map ("  " ++) . go
    go = \case
      Let _ n v body -> ("let " ++ name n ++ " = " ++ pprint v ++ " in:") : indented body
      Set _ n v -> ["set " ++ name n ++ " := " ++ pprint v]
      Block body -> "block:" : concatMap (map ("  " ++) . go) body
      NoOp -> ["no-op"]
      DoWhile v body -> ("do-while " ++ pprint v ++ ":") : indented body
      IfThenElse c y n ->
        let yy = indented y
            nn = indented n
        in ["if " ++ pprint c ++ " then:"] ++ yy ++ ["else:"] ++ nn
      DrawCommand cmd -> (:[]) $ case cmd of
        DrawPoint _ v -> "draw point at " ++ pprint v
        DrawCircle _ f r v -> "draw " ++ (if f then "filled " else "") ++
                              "circle at " ++ pprint v ++ " with radius " ++ pprint r
        DrawLine _ p q -> "draw line from " ++ pprint p ++ " to " ++ pprint q
        DrawRect _ f p q -> "draw " ++ (if f then "filled " else "") ++
                            "rectangle from " ++ pprint p ++ " to " ++ pprint q
        SetStroke _ c -> "use " ++ pprint c ++ " for line"
        SetFill _ c -> "use " ++ pprint c ++ " for line"
        Clear _ -> "erase"
        Write _ t p -> "write " ++ pprint t ++ " at " ++ pprint p
      Lookup{} -> ["{TODO: Lookup}"]
      ForEach{} -> ["{TODO: ForEach}"]

data Unit :: Environment -> Exp Type
type instance Eval (Unit _) = ()

data UnitET :: (Environment, FSType) -> Exp Type
type instance Eval (UnitET _) = ()
