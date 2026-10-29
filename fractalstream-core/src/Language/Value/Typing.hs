-- | The typing pass for value expressions: decorates every node of a
-- surface 'Expr' with its own 'FSType', given the types of the variables
-- in scope.
--
-- This is bidirectional. 'synth' computes the smallest type a node can
-- have (e.g. @1 + 2@ is an integer, @x / 2@ is at least real); 'check'
-- accepts any synthesized type that can be implicitly converted to the
-- expected one (integer → real → complex, and numbers → text), and
-- otherwise pushes the expected type down into the structural nodes
-- (pairs, lists, if/then/else, list operations) that can adopt it. No
-- typed AST is built here, and there is no backtracking beyond that one
-- fallback; "Language.Value.Elaborate" then builds the typed AST from the
-- decorated tree, inserting the conversions at node boundaries.
--
-- The rules mirror the older closure-based typechecker (which these
-- replaced), so that elaboration produces the same typed ASTs it did, down
-- to the placement of conversions -- except for comparisons, which now use
-- integer comparison for integers, and `escapes`/`vanishes`/@=@, which now
-- take real absolute values of real numbers rather than first converting
-- them to complex numbers.
module Language.Value.Typing
  ( TypeEnv(..)
  , typeEnvOf
  , strictTypeEnv
  , lenientTypeEnv
  , structuralJoin
  , synth
  , check
  , coercible
  , joinType
  , anType
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Type
import Language.Environment
import Language.Typecheck
import Language.Parser.SourceRange
import Language.Value.Syntax
import Language.Value.Builtins
import Language.Value.Differentiate

-- | The types of the variables in scope.
data TypeEnv = TypeEnv
  { tyVars :: Map String FSType
  , tyLenient :: Bool
    -- ^ In lenient mode, used during type inference, a list-valued
    -- subexpression that would need to be widened to a larger item type is
    -- just assumed to have that type, and branches are joined structurally.
    -- These are what the inferred variables involved will need to be raised
    -- to; elaboration checks the final result in the usual, strict mode.
  }

typeEnvOf :: EnvironmentProxy env -> TypeEnv
typeEnvOf = strictTypeEnv . Map.map (\(SomeType t) -> typeToFS t) . envToMap

strictTypeEnv, lenientTypeEnv :: Map String FSType -> TypeEnv
strictTypeEnv vs = TypeEnv vs False
lenientTypeEnv vs = TypeEnv vs True

anType :: FSType -> String
anType t = withType t (an . SomeType)

------------------------------------------------------
-- Implicit conversions
------------------------------------------------------

numericRank :: FSType -> Maybe Int
numericRank = \case
  IntegerT -> Just 0
  RealT    -> Just 1
  ComplexT -> Just 2
  _        -> Nothing

isNumeric :: FSType -> Bool
isNumeric = isJust . numericRank

-- | Can a value of the first type be implicitly converted to the second?
coercible :: FSType -> FSType -> Bool
coercible from to
  | from == to = True
  | Just i <- numericRank from, Just j <- numericRank to = i <= j
  | to == TextT = isNumeric from
  | otherwise = False

-- | The smallest type both arguments can be converted to, if there is one.
-- Only the numeric tower is joined; text conversions are only ever used
-- when checking against an expected text type.
joinType :: FSType -> FSType -> Maybe FSType
joinType a b
  | a == b = Just a
  | Just i <- numericRank a, Just j <- numericRank b = Just (if i >= j then a else b)
  | otherwise = Nothing

------------------------------------------------------
-- Checking
------------------------------------------------------

check :: TypeEnv -> FSType -> Expr -> TC TExpr
check env ty e = case pushdown env ty e of
  -- An expected text type is pushed down first, so that e.g.
  -- @if c then 1 else "x"@ converts each branch to text.
  Just pushed | ty == TextT -> pushed `catchError` \err -> synthOr (throwError err)
  Just pushed -> synthOr pushed
  Nothing -> do
    t <- synth env e
    if coercible (exprType t) ty then pure t else throwError (mismatch t ty)
  where
    -- Use the synthesized type if it converts to the expected one,
    -- otherwise fall back to the given alternative.
    synthOr fallback = do
      r <- (Right <$> synth env e) `catchError` (pure . Left)
      case r of
        Right t | coercible (exprType t) ty -> pure t
        _ -> fallback

mismatch :: TExpr -> FSType -> TCError
mismatch t ty =
  Surprise (synRange t) (describe (synNode t)) (anType (exprType t)) (Expected (anType ty))

-- | Push an expected type into a node that can adopt it, for the nodes
-- where that can succeed even though synthesis did not produce a type
-- convertible to the expected one.
pushdown :: TypeEnv -> FSType -> Expr -> Maybe (TC TExpr)
pushdown env ty (Syn sr node) = fmap (fmap (Syn (sr, ty))) $ case (node, ty) of
  (EITE c yes no, _) -> Just $
    EITE <$> check env BooleanT c <*> check env ty yes <*> check env ty no
  (EPair x y, Pair t1 t2) -> Just $
    EPair <$> check env t1 x <*> check env t2 y
  (EList xs, ListT t) -> Just $
    EList <$> traverse (check env t) xs
  (EJoin xs, ListT _) -> Just $
    EJoin <$> traverse (check env ty) xs
  (EAppend lst x, ListT t) -> Just $
    EAppend <$> check env ty lst <*> check env t x
  (EPrepend lst x, ListT t) -> Just $
    EPrepend <$> check env ty lst <*> check env t x
  (ERemove lst n p, ListT t) -> Just $ do
    env' <- bind sr n t env
    ERemove <$> check env ty lst <*> pure n <*> check env' BooleanT p
  (EFind lst n p d, _) -> Just $ do
    env' <- bind sr n ty env
    EFind <$> check env (ListT ty) lst <*> pure n <*> check env' BooleanT p <*> check env ty d
  (ETransform lst n f, ListT t2) -> Just $ do
    lst' <- synth env lst
    t1 <- listItemType lst'
    env' <- bind sr n t1 env
    ETransform lst' n <$> check env' t2 f
  (ELet n v body, _) -> Just $ do
    v' <- synth env v
    env' <- bind sr n (exprType v') env
    ELet n v' <$> check env' ty body
  (EIndex cyc lst ix, _) -> Just $
    EIndex cyc <$> check env (ListT ty) lst <*> check env IntegerT ix
  _ -> Nothing

------------------------------------------------------
-- Synthesis
------------------------------------------------------

synth :: TypeEnv -> Expr -> TC TExpr
synth env (Syn sr node) = case node of

  ELit d c@(SomeHaskellType t _) -> done (typeToFS t) (ELit d c)

  EVar n -> case Map.lookup n (tyVars env) of
    Just t  -> done t (EVar n)
    Nothing -> throwError (MissingName sr n)

  ECast x t -> do
    x' <- check env t x
    done t (ECast x' t)

  EITE c yes no -> do
    c' <- check env BooleanT c
    yes' <- synth env yes
    no' <- synth env no
    case joinIn env (exprType yes') (exprType no') of
      Just t  -> done t (EITE c' yes' no')
      Nothing -> throwError (mismatch no' (exprType yes'))

  EPair x y -> do
    x' <- synth env x
    y' <- synth env y
    done (Pair (exprType x') (exprType y')) (EPair x' y')

  EList [] -> advise sr "I can't tell what type of items this empty list should hold."
  EList xs -> do
    xs' <- traverse (synth env) xs
    t <- joinAll env xs'
    done (ListT t) (EList xs')

  EBin op x y -> synthBin env sr op x y

  EUn op x -> synthUn env sr op x

  EApp f x
    | Map.member f commonFunctions -> do
        x' <- synth env x
        case joinType RealT (exprType x') of
          Just t | t /= IntegerT -> done t (EApp f x')
          _ -> throwError (Surprise sr ("the argument to " ++ f) (anType (exprType x'))
                             (Expected "a real or complex number"))
    | Map.member f realFunctions -> do
        x' <- check env RealT x
        done RealT (EApp f x')
    | Map.member f roundingFunctions -> do
        x' <- check env RealT x
        done IntegerT (EApp f x')
    | Just (ComplexFun resultTy _) <- Map.lookup f complexFunctions -> do
        x' <- check env ComplexT x
        done (typeToFS resultTy) (EApp f x')
    | Map.member f wheels -> do
        x' <- check env RealT x
        done ColorT (EApp f x')
    | otherwise -> throwError (Internal (MissingName sr f))

  EColor3 op x y z -> do
    x' <- check env RealT x
    (y', z') <- case op of
      OpRGB -> (,) <$> check env RealT y <*> check env RealT z
      _   -> (,) <$> check env ColorT y <*> check env ColorT z
    done ColorT (EColor3 op x' y' z')

  EDiff x f -> do
    x' <- synth env x
    f' <- synth env f
    case filter (not . isNumeric . exprType) [x', f'] of
      bad : _ -> throwError (mismatch bad RealT)
      [] -> pure ()
    var <- case synNode x of
      EVar n | exprType x' `elem` [RealT, ComplexT] -> pure n
      _ -> throwError (DiffInputMustBeVariable (synRange x))
    -- The derivative is at least real, and at least as big as the types
    -- of the variable and the function.
    let t = fromMaybe ComplexT (joinType RealT =<< joinType (exprType x') (exprType f'))
    df <- differentiate var f
    synth env (Syn sr (EWiden t df))

  EText xs -> do
    xs' <- traverse (check env TextT) xs
    done TextT (EText xs')

  EJoin xs -> do
    xs' <- traverse (synth env) xs
    t <- joinAll env =<< traverse listItem xs'
    let ty = ListT t
    xs'' <- sequence [ refit env ty x x'
                     | (x, x') <- zip xs xs' ]
    done ty (EJoin xs'')

  EAppend lst x -> synthCons EAppend lst x
  EPrepend lst x -> synthCons EPrepend lst x

  ERemove lst n p -> do
    lst' <- synth env lst
    t <- listItemType lst'
    env' <- bind sr n t env
    p' <- check env' BooleanT p
    done (ListT t) (ERemove lst' n p')

  EFind lst n p d -> do
    lst' <- synth env lst
    d' <- synth env d
    tl <- listItemType lst'
    t <- maybe (throwError (mismatch d' tl)) pure (joinIn env tl (exprType d'))
    lst'' <- refit env (ListT t) lst lst'
    env' <- bind sr n t env
    p' <- check env' BooleanT p
    done t (EFind lst'' n p' d')

  ETransform lst n f -> do
    lst' <- synth env lst
    t1 <- listItemType lst'
    env' <- bind sr n t1 env
    f' <- synth env' f
    done (ListT (exprType f')) (ETransform lst' n f')

  ERange x y -> do
    x' <- check env IntegerT x
    y' <- check env IntegerT y
    done (ListT IntegerT) (ERange x' y')

  ELength lst -> do
    lst' <- case lst of
      -- the length of an empty list literal is fine, whatever it holds
      Syn lsr (EList []) -> pure (Syn (lsr, ListT BooleanT) (EList []))
      _ -> synth env lst
    case exprType lst' of
      ListT t | t `elem` [BooleanT, IntegerT, RealT, ComplexT, ColorT] ->
        done IntegerT (ELength lst')
      _ -> advise sr "The argument to `length` should be a list."

  EIndex cyc lst ix -> do
    lst' <- synth env lst
    t <- listItemType lst'
    ix' <- check env IntegerT ix
    done t (EIndex cyc lst' ix')

  ELet n v body -> do
    v' <- synth env v
    env' <- bind sr n (exprType v') env
    body' <- synth env' body
    done (exprType body') (ELet n v' body')

  ECall f _ -> throwError (Internal (Advice sr ("unexpanded call to " ++ f)))
  EDerivCall f _ _ -> throwError (Internal (Advice sr ("unexpanded call to " ++ f ++ "'")))

  EWiden t x -> do
    x' <- synth env x
    case joinType t (exprType x') of
      Just t' -> done t' (EWiden t x')
      Nothing -> throwError (mismatch x' t)

  EAdvice msg -> advise sr msg

  where
    done :: FSType -> ExprF TExpr -> TC TExpr
    done t n = pure (Syn (sr, t) n)

    synthCons mk lst x = do
      lst' <- synth env lst
      x' <- synth env x
      tl <- listItemType lst'
      t <- maybe (throwError (mismatch x' tl)) pure (joinIn env tl (exprType x'))
      lst'' <- refit env (ListT t) lst lst'
      done (ListT t) (mk lst'' x')

synthBin :: TypeEnv -> SourceRange -> BinOp -> Expr -> Expr -> TC TExpr
synthBin env sr op x y = case op of
  OpAdd -> arith Nothing
  OpSub -> arith Nothing
  OpMul -> arith Nothing
  OpPow -> arith Nothing
  OpDiv -> arith (Just RealT)

  OpIDiv -> do
    x' <- check env IntegerT x
    y' <- check env IntegerT y
    done IntegerT x' y'

  OpMod -> do
    (x', y', t) <- numericPair
    unless (t `elem` [IntegerT, RealT]) $ throwError
      (Surprise sr "the result of a modulo operation" (anType t)
        (Expected "an integer or real number"))
    done t x' y'

  OpOr  -> logical
  OpAnd -> logical

  OpExactEq -> do
    (x', y', _) <- numericPair `catchTypeError` \_ ->
      advise sr "Arguments to ≡ must be real or complex numbers."
    done BooleanT x' y'

  OpApproxEq  -> approx id
  OpApproxNEq -> approx (Syn (sr, BooleanT) . EUn OpNot)

  OpLt -> comparison
  OpGt -> comparison
  OpLe -> comparison
  OpGe -> comparison

  where
    done t x' y' = pure (Syn (sr, t) (EBin op x' y'))

    numericPair = do
      x' <- synth env x
      y' <- synth env y
      case joinType (exprType x') (exprType y') of
        Just t | isNumeric t -> pure (x', y', t)
        _ -> throwError $ case filter (not . isNumeric . exprType) [x', y'] of
          bad : _ -> mismatchNumeric bad
          [] -> mismatch y' (exprType x')

    mismatchNumeric t = Surprise (synRange t) (describe (synNode t))
                          (anType (exprType t)) (Expected "a number")

    arith atLeast = do
      (x', y', t0) <- numericPair
      let t = fromMaybe t0 (atLeast >>= joinType t0)
      done t x' y'

    logical = do
      x' <- check env BooleanT x
      y' <- check env BooleanT y
      done BooleanT x' y'

    comparison = do
      (x', y', t) <- numericPair `catchTypeError` \_ -> notReal
      unless (t `elem` [IntegerT, RealT]) notReal
      done BooleanT x' y'
    notReal :: TC a
    notReal = advise sr "Arguments to a comparison must be integers or real numbers."

    -- Integers and truth values are compared exactly; other numbers are
    -- compared up to the vanishing radius, as @(x - y) vanishes@.
    approx wrap = do
      x' <- synth env x
      y' <- synth env y
      case (exprType x', exprType y') of
        (IntegerT, IntegerT) -> done BooleanT x' y'
        (BooleanT, BooleanT) -> done BooleanT x' y'
        (tx, ty) | Just t <- joinType tx ty, isNumeric t -> do
          unless (Map.member internalVanishingRadius (tyVars env)) $
            throwError (MissingName sr internalVanishingRadius)
          let delta = Syn (sr, t) (EBin OpSub x' y')
          pure (wrap (Syn (sr, BooleanT) (EUn OpVanishes delta)))
        _ -> advise sr ("Arguments to " ++ (if op == OpApproxEq then "=" else "≠")
                        ++ " must be of some comparable type.")

synthUn :: TypeEnv -> SourceRange -> UnOp -> Expr -> TC TExpr
synthUn env sr op x = case op of
  OpNeg -> do
    x' <- synth env x
    unless (isNumeric (exprType x')) $ throwError
      (Surprise sr "the result of negation" (anType (exprType x')) (Expected "of some numeric type"))
    done (exprType x') x'

  OpNot -> check env BooleanT x >>= done BooleanT

  OpAbs -> do
    x' <- synth env x
    case exprType x' of
      IntegerT -> done IntegerT x'
      RealT    -> done RealT x'
      ComplexT -> done RealT x'
      _ -> advise sr "The argument to |·| is not a real or complex number"

  OpEscapes  -> radius internalEscapeRadius "escapes"
  OpVanishes -> radius internalVanishingRadius "vanishes"

  OpDark   -> check env ColorT x >>= done ColorT
  OpLight  -> check env ColorT x >>= done ColorT
  OpInvert -> check env ColorT x >>= done ColorT

  where
    done t x' = pure (Syn (sr, t) (EUn op x'))

    radius name kw = do
      let notNumber :: TC a
          notNumber = advise sr ("The argument to `" ++ kw ++ "` should be a real or complex number.")
      x' <- (synth env x >>= \x' -> if isNumeric (exprType x') then pure x' else notNumber)
              `catchTypeError` \_ -> notNumber
      case Map.lookup name (tyVars env) of
        Just RealT -> done BooleanT x'
        Just t -> throwError (Surprise sr name (anType t) (Expected (anType RealT)))
        Nothing -> throwError (MissingName sr name)

------------------------------------------------------
-- Utilities
------------------------------------------------------

bind :: SourceRange -> String -> FSType -> TypeEnv -> TC TypeEnv
bind sr n t env
  | Map.member n (tyVars env) = throwError (AlreadyDefined sr n)
  | otherwise = pure env { tyVars = Map.insert n t (tyVars env) }

listItem :: TExpr -> TC TExpr
listItem t = case exprType t of
  ListT it -> pure (Syn (synRange t, it) (synNode t))
  _ -> throwError (Surprise (synRange t) (describe (synNode t))
                    (anType (exprType t)) (Expected "some kind of list"))

listItemType :: TExpr -> TC FSType
listItemType = fmap exprType . listItem

-- | Join the types of a non-empty list of typed nodes.
joinAll :: TypeEnv -> [TExpr] -> TC FSType
joinAll env = \case
  [] -> throwError (Internal (Advice NoSourceRange "joinAll: no items"))
  (x : xs) -> foldM step (exprType x) xs
  where
    step t x = maybe (throwError (mismatch x t)) pure (joinIn env t (exprType x))

joinIn :: TypeEnv -> FSType -> FSType -> Maybe FSType
joinIn env = if tyLenient env then structuralJoin else joinType

-- | Like 'joinType', but also joining the item types of lists and the
-- components of pairs.
structuralJoin :: FSType -> FSType -> Maybe FSType
structuralJoin a b = case (a, b) of
  (ListT x, ListT y) -> ListT <$> structuralJoin x y
  (Pair x1 x2, Pair y1 y2) -> Pair <$> structuralJoin x1 y1 <*> structuralJoin x2 y2
  _ -> joinType a b

-- | Make an already-synthesized node fit a (larger) type, by checking the
-- original expression again at that type. In lenient mode, a node that
-- can't be made to fit is assumed to have the type anyway.
refit :: TypeEnv -> FSType -> Expr -> TExpr -> TC TExpr
refit env ty x x'
  | exprType x' == ty = pure x'
  | otherwise = check env ty x `catchError` \err ->
      if tyLenient env then pure (Syn (synRange x', ty) (synNode x')) else throwError err

-- | How to refer to a node in an error message.
describe :: ExprF r -> String
describe = \case
  ELit d _ -> d
  EVar n -> n
  ECast{} -> "this conversion"
  EITE{} -> "this if/then/else expression"
  EPair{} -> "this ordered pair"
  EList{} -> "this list"
  EBin op _ _ -> case op of
    OpOr -> "the result of a disjunction"
    OpAnd -> "the result of a conjunction"
    OpMod -> "the result of a modulo operation"
    _ | op `elem` [OpExactEq, OpApproxEq, OpApproxNEq] -> "the result of an equality check"
      | op `elem` [OpLt, OpGt, OpLe, OpGe] -> "the result of a comparison"
      | otherwise -> "the result of an arithmetic operation"
  EUn op _ -> case op of
    OpNeg -> "the result of negation"
    OpNot -> "the result of a logical negation"
    OpAbs -> "the result of |·|"
    OpEscapes -> "the result of `escapes`"
    OpVanishes -> "the result of `vanishes`"
    _ -> "this color"
  EApp f _ -> "the result of " ++ f
  EColor3{} -> "this color"
  EDiff{} -> "the result of a derivative"
  EText{} -> "this text"
  EJoin{} -> "the result of a join operation"
  EAppend{} -> "the result of an append operation"
  EPrepend{} -> "the result of a prepend operation"
  ERemove{} -> "the result of a remove operation"
  EFind{} -> "the result of a find operation"
  ETransform{} -> "the result of a transform operation"
  ERange{} -> "the result of a range operation"
  ELength{} -> "the result of a length operation"
  EIndex{} -> "this list item"
  ELet{} -> "the result of this expression"
  ECall f _ -> "the result of " ++ f
  EDerivCall f order _ -> "the result of " ++ f ++ replicate order '\''
  EWiden{} -> "this value"
  EAdvice{} -> "this expression"
