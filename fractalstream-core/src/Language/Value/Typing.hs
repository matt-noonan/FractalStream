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
  , tupleParts
  , tableIndexParts
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
  (EIndex _ (Syn _ (EVar n)) _, _)
    | Just TableT{} <- Map.lookup n (tyVars env) -> Nothing  -- (see synth)
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
      -- (types that don't join as they are may still fit, as `(0, 0)`
      -- fits a pair of a complex number and an integer)
      Nothing ->
        (do no'' <- check env (exprType yes') no
            done (exprType yes') (EITE c' yes' no''))
        `catchError` \_ ->
        (do yes'' <- check env (exprType no') yes
            done (exprType no') (EITE c' yes'' no'))
        `catchError` \_ -> throwError (mismatch no' (exprType yes'))

  EPair x y -> do
    x' <- synth env x
    y' <- synth env y
    done (Pair (exprType x') (exprType y')) (EPair x' y')

  -- (from a tuple assignment, with its tuple of variables' source range)
  EProj isSecond x -> do
    x' <- synth env x
    case exprType x' of
      Pair t1 t2 -> done (if isSecond then t2 else t1) (EProj isSecond x')
      t -> throwError (Surprise sr "the value for this tuple of variables" (anType t)
                         (Expected "a pair, with a part for each"))

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
      OpHSV -> (,) <$> check env RealT y <*> check env RealT z
      OpHSL -> (,) <$> check env RealT y <*> check env RealT z
      _   -> (,) <$> check env ColorT y <*> check env ColorT z
    done ColorT (EColor3 op x' y' z')

  EAutocolor mode g p -> do
    -- The group number has to be written as a number, so that which
    -- groups a project uses is fixed by its script.
    g' <- case integerLiteral g of
      Just _ -> check env IntegerT g
      Nothing -> advise (synRange g) ("The group number of `" ++ autocolorModeName mode ++ "` has \
                                      \to be a plain number, like `" ++ autocolorModeName mode ++ "(0, z)`: \
                                      \it can't be computed.")
    p' <- check env ComplexT p
    done ColorT (EAutocolor mode g' p')

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
      ListT _ -> done IntegerT (ELength lst')
      _ -> advise sr "The argument to `length` should be a list."

  EIndex cyc lst ix -> do
    lst' <- synth env lst
    case exprType lst' of
      TableT d it
        | cyc -> advise sr "A table can't be indexed cyclically (with `@@`): use `@`."
        | otherwise -> do
            (mp, rest) <- tableIndexParts sr (fromIntegral d) ix
            mp' <- traverse (check env ComplexT) mp
            rest' <- mapM (traverse (check env IntegerT)) rest
            case (length (filter isNothing rest'), mp') of
              (0, Nothing) -> done it (ETableIndex lst' (catMaybes rest'))
              (0, Just p') -> done it (ETablePoint lst' p' (catMaybes rest'))
              (1, _) -> done (ListT it) (ETableSlice lst' mp' rest')
              _ -> advise sr "A slice of a table with more than one `*` can only be copied onto \
                             \another one, as in `T @ (*, *, k) <- U @ (*, *, m)`; with one `*`, \
                             \it's a list."
      _ -> do
        t <- listItemType lst'
        ix' <- check env IntegerT ix
        done t (EIndex cyc lst' ix')

  ETableIndex{} -> throwError (Internal (Advice sr "unexpected table index"))
  ETablePoint{} -> throwError (Internal (Advice sr "unexpected table point"))
  ETableSlice{} -> throwError (Internal (Advice sr "unexpected table slice"))

  EPointIndex _ -> advise sr "`point z` only goes in a table's index, where it stands for the \
                             \first two indices: `T @ point z`, or `T @ (point z, k)`."
  ESliceAll -> advise sr "`*` stands for every index along one dimension of a table, \
                         \in the table's index: `T @ (j, *)`."

  ETableSize tbl -> do
    tbl' <- synth env tbl
    case exprType tbl' of
      TableT d _ -> done (tableSizeType (fromIntegral d)) (ETableSize tbl')
      _ -> advise sr "The argument to `table_size` should be a table."

  ETablePointOr tbl ix v -> case pointOf ix of
    Just p -> synth env (Syn sr (EITE (Syn sr (ETableHas tbl p)) (Syn sr (EIndex False tbl ix)) v))
    Nothing -> throwError (Internal (Advice sr "unexpected table index in `else`"))
    where pointOf = \case
            Syn _ (EPointIndex p) -> Just p
            Syn _ (EPair (Syn _ (EPointIndex p)) _) -> Just p
            _ -> Nothing

  ETableHas tbl p -> do
    tbl' <- synth env tbl
    case exprType tbl' of
      TableT d _ | d >= 2 -> do
        p' <- check env ComplexT p
        done BooleanT (ETableHas tbl' p')
      TableT _ _ -> advise sr "A 1-dimensional table has no points."
      _ -> advise sr "`z in T` asks whether a point is on a table: `T` should be a table."

  ETableCell tbl p -> do
    tbl' <- synth env tbl
    case exprType tbl' of
      TableT d _ | d >= 2 -> do
        p' <- check env ComplexT p
        done (tableSizeType 2) (ETableCell tbl' p')
      TableT _ _ -> advise sr "A 1-dimensional table has no points."
      _ -> advise sr "The first argument to `table_cell` should be a table."

  ETableCellPoint tbl ixs -> do
    tbl' <- synth env tbl
    case (exprType tbl', ixs) of
      (TableT d _, [ix]) | d >= 2 -> case tupleParts 2 ix of
        Just parts -> do
          parts' <- mapM (check env IntegerT) parts
          done ComplexT (ETableCellPoint tbl' parts')
        Nothing -> advise sr "A cell's place in the plane is given by its column and row: `table_point(T, (j, k))`."
      (TableT d _, _) | d >= 2 -> throwError (Internal (Advice sr "unexpected table_point"))
      (TableT _ _, _) -> advise sr "A 1-dimensional table has no points."
      _ -> advise sr "The first argument to `table_point` should be a table."

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

-- | The value of an integer expression made of integer constants alone
-- (like @-8@, or @1 - 3@), if it is one.
constantInteger :: Expr -> Maybe Integer
constantInteger e = case synNode e of
  _ | Just n <- integerLiteral e -> Just n
  EUn OpNeg x -> negate <$> constantInteger x
  EBin op x y -> do
    a <- constantInteger x
    b <- constantInteger y
    case op of
      OpAdd -> Just (a + b)
      OpSub -> Just (a - b)
      OpMul -> Just (a * b)
      OpIDiv | b /= 0 -> Just (a `div` b)
      _ -> Nothing
  _ -> Nothing

synthBin :: TypeEnv -> SourceRange -> BinOp -> Expr -> Expr -> TC TExpr
synthBin env sr op x y = case op of
  OpAdd -> arith Nothing
  OpSub -> arith Nothing
  OpMul -> arith Nothing
  -- (a power of integers is an integer, unless its exponent is a
  -- negative constant: then it's a fraction, so it's real)
  OpPow -> do
    (x', y', t) <- numericPair
    done (if t == IntegerT && maybe False (< 0) (constantInteger y) then RealT else t) x' y'
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
        -- Tuples of numbers are compared by the distance between them
        -- (as in the plane, for a pair of reals), with each value held in
        -- a fresh local so that it's computed once.
        (tx, ty) | a : b : _ <- freshLocals env
                 , Just diffs <- partDifferences sr tx ty (Syn sr (EVar a)) (Syn sr (EVar b)) -> do
          unless (Map.member internalVanishingRadius (tyVars env)) $
            throwError (MissingName sr internalVanishingRadius)
          let r = Syn sr (EVar internalVanishingRadius)
              near = Syn sr (EBin OpLt (sumOfSquares sr diffs) (Syn sr (EBin OpMul r r)))
          wrap <$> synth env (Syn sr (ELet a x (Syn sr (ELet b y near))))
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
  -- every `stops` in a loop's condition was rewritten away
  -- ("Language.Code.Stops"), so this one is somewhere else
  OpStops -> advise sr "`stops` can only be used in a loop's condition, as in `until z stops`."
  -- Similarly, every `previous` in a loop body was
  -- rewritten away.
  OpPrevious -> advise sr "`previous` can only be used inside a loop."

  OpDark   -> check env ColorT x >>= done ColorT
  OpLight  -> check env ColorT x >>= done ColorT
  OpInvert -> check env ColorT x >>= done ColorT

  where
    done t x' = pure (Syn (sr, t) (EUn op x'))

    radius name kw = do
      let notNumber :: TC a
          notNumber = advise sr ("The argument to `" ++ kw ++ "` should be a real or complex number, \
                                 \or a tuple of numbers.")
      x' <- synth env x `catchTypeError` \_ -> notNumber
      case Map.lookup name (tyVars env) of
        Just RealT -> pure ()
        Just t -> throwError (Surprise sr name (anType t) (Expected (anType RealT)))
        Nothing -> throwError (MissingName sr name)
      if isNumeric (exprType x') then done BooleanT x' else
        -- A tuple of numbers is measured by its length (as in the plane,
        -- for a pair of reals), with the tuple held in a fresh local.
        case freshLocals env of
          n : _ | Just parts <- numericParts sr (exprType x') (Syn sr (EVar n)) -> do
            let r = Syn sr (EVar name)
                r2 = Syn sr (EBin OpMul r r)
                len2 = sumOfSquares sr parts
                cmp | op == OpEscapes = EBin OpLt r2 len2
                    | otherwise = EBin OpLt len2 r2
            synth env (Syn sr (ELet n x (Syn sr cmp)))
          _ -> notNumber

------------------------------------------------------
-- Utilities
------------------------------------------------------

-- | Names for locals that rewrites introduce, not yet in use. (No script
-- can write a name starting with @#@.)
freshLocals :: TypeEnv -> [String]
freshLocals env = [ n | k <- [0 :: Int ..], let n = "#tuple " ++ show k
                      , Map.notMember n (tyVars env) ]

-- | The numbers making up a tuple of numbers (nested pairs of integers,
-- reals, and complex numbers) of the given type, each as an expression
-- taking the given value apart; 'Nothing' for any other type.
numericParts :: SourceRange -> FSType -> Expr -> Maybe [Expr]
numericParts sr t e = case t of
  Pair a b -> (++) <$> numericParts sr a (Syn sr (EProj False e))
                   <*> numericParts sr b (Syn sr (EProj True e))
  _ | isNumeric t -> Just [e]
    | otherwise -> Nothing

-- | The part-by-part differences of two tuples of numbers of the same
-- shape.
partDifferences :: SourceRange -> FSType -> FSType -> Expr -> Expr -> Maybe [Expr]
partDifferences sr s t x y = case (s, t) of
  (Pair a b, Pair c d) ->
    (++) <$> partDifferences sr a c (proj False x) (proj False y)
         <*> partDifferences sr b d (proj True x) (proj True y)
  _ | isNumeric s, isNumeric t -> Just [Syn sr (EBin OpSub x y)]
    | otherwise -> Nothing
  where proj isSecond e = Syn sr (EProj isSecond e)

-- | The sum of the squares of some numbers' absolute values.
sumOfSquares :: SourceRange -> [Expr] -> Expr
sumOfSquares sr = foldr1 (\a b -> Syn sr (EBin OpAdd a b)) . map square
  where square e = let m = Syn sr (EUn OpAbs e) in Syn sr (EBin OpMul m m)

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
    OpPow -> "this power"
    _ | op `elem` [OpExactEq, OpApproxEq, OpApproxNEq] -> "the result of an equality check"
      | op `elem` [OpLt, OpGt, OpLe, OpGe] -> "the result of a comparison"
      | otherwise -> "the result of an arithmetic operation"
  EUn op _ -> case op of
    OpNeg -> "the result of negation"
    OpNot -> "the result of a logical negation"
    OpAbs -> "the result of |·|"
    OpEscapes -> "the result of `escapes`"
    OpVanishes -> "the result of `vanishes`"
    OpStops -> "the result of `stops`"
    OpPrevious -> "the result of `previous`"
    _ -> "this color"
  EApp f _ -> "the result of " ++ f
  EColor3{} -> "this color"
  EAutocolor{} -> "this color"
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
  ETableIndex{} -> "this table entry"
  ETablePoint{} -> "this table entry"
  ETableSlice{} -> "this slice of a table"
  EPointIndex{} -> "this point"
  ESliceAll -> "this `*`"
  ETableSize{} -> "this table's size"
  ETablePointOr{} -> "this table entry"
  ETableHas{} -> "whether this point is on the table"
  ETableCell{} -> "this table cell"
  ETableCellPoint{} -> "this table cell's point"
  ELet{} -> "the result of this expression"
  ECall f _ -> "the result of " ++ f
  EDerivCall f order _ -> "the result of " ++ f ++ replicate order '\''
  EWiden{} -> "this value"
  EProj{} -> "this part of a pair"
  EAdvice{} -> "this expression"

-- | The parts of a tuple written out with @n@ components (@(a, b, c)@ is
-- @(a, (b, c))@), or the expression itself when @n@ is 1.
tupleParts :: Int -> Expr -> Maybe [Expr]
tupleParts n e
  | n <= 1 = Just [e]
  | otherwise = case e of
      Syn _ (EPair a b) -> (a :) <$> tupleParts (n - 1) b
      _ -> Nothing

-- | A table's index, split into its parts: a point standing for the
-- first two indices, if there is one, and then the rest of them, each an
-- integer or a @*@ ('Nothing'). There must be one per dimension (the
-- point counting for two).
tableIndexParts :: SourceRange -> Int -> Expr -> TC (Maybe Expr, [Maybe Expr])
tableIndexParts sr d ix = do
  let firstPart = case ix of
        Syn _ (EPair a _) -> a
        _ -> ix
      point = case firstPart of
        Syn _ (EPointIndex p) -> Just p
        _ -> Nothing
      count = if isJust point then d - 1 else d
  when (isJust point && d < 2) $
    advise sr "A 1-dimensional table has no points: index it with an integer, `T @ k`."
  parts <- case tupleParts count ix of
    Just parts | count >= 1 -> pure parts
    _ -> advise sr (howToIndex d)
  rest <- forM (if isJust point then drop 1 parts else parts) $ \case
    Syn _ ESliceAll -> pure Nothing
    Syn psr (EPointIndex _) -> advise psr "`point z` stands for a table's first two indices, so it has to come first: `T @ (point z, k)`."
    e -> pure (Just e)
  pure (point, rest)

-- | How to index a table with some number of dimensions.
howToIndex :: Int -> String
howToIndex d
  | d == 1 = "A 1-dimensional table is indexed by one integer: `T @ k`."
  | d == 2 = "A 2-dimensional table is indexed by two integers, written out as a tuple, \
             \`T @ (j, k)`, or by a point, `T @ point z`."
  | otherwise = "A " ++ show d ++ "-dimensional table is indexed by " ++ show d
                ++ " integers, written out as a tuple, `T @ (" ++ intercalate ", " (take d names)
                ++ ")`, or by a point and " ++ show (d - 2) ++ " more, `T @ (point z, "
                ++ intercalate ", " (take (d - 2) (drop 2 names)) ++ ")`."
  where names = map pure "jkmnpqrs" ++ [ "i" ++ show i | i <- [9 :: Int ..] ]

-- | The type of a table's size: an integer for each dimension, as a tuple
-- (@(a, (b, c))@ for three), or just one integer.
tableSizeType :: Int -> FSType
tableSizeType n
  | n <= 1 = IntegerT
  | otherwise = Pair IntegerT (tableSizeType (n - 1))
