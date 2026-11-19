-- | Symbolic differentiation of surface expressions: @diff(x, e)@ is
-- replaced (during typing) by the derivative of @e@ with respect to the
-- variable @x@, which is then typed and elaborated like any other
-- expression.
--
-- Variables other than @x@ are treated as constants, except for those
-- bound by local bindings (from inlined function calls, for example),
-- which are handled by the chain rule:
--
-- > d/dx (let p = v in b) = let p = v in (∂b/∂x + ∂b/∂p · dv/dx)
--
-- A subexpression that doesn't mention @x@ at all has derivative zero,
-- even if it is something (like @|y|@) that couldn't be differentiated in
-- general.
module Language.Value.Differentiate
  ( differentiate
  , differentiateWith
  , DiffEnv(..)
  , valueDeps
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Type
import Language.Typecheck
import Language.Value.Syntax
import Language.Value.Builtins (roundingFunctions)
import Language.Parser.SourceRange

-- | What differentiating needs to know besides the expression: the
-- variable, whether it's real (so that @|u|@, @re@, @im@, @conj@ and
-- @arg@ have derivatives: with respect to a complex variable they aren't
-- holomorphic), and the variables whose values depend on it, each with
-- the variable holding its derivative (see "Language.Code.Derivatives";
-- a variable that depends on it but has none, like an integer worked out
-- from it, is in the set without one).
data DiffEnv = DiffEnv
  { deVar     :: String
  , deReal    :: Bool
  , deFollow  :: Map String String
  , deDepends :: Set String
  }

-- | The derivative of an expression with respect to the named variable,
-- with every other variable a constant: what @partial@ means.
differentiate :: String -> Expr -> TC Expr
differentiate x = differentiateWith (DiffEnv x False Map.empty Set.empty)

-- | The derivative of an expression with respect to a variable, following
-- the variables that depend on it to their derivatives.
differentiateWith :: DiffEnv -> Expr -> TC Expr
differentiateWith env e@(Syn sr node)
  | not (dependsOn e) = pure (zero sr)
  | otherwise = case node of

      EVar n | n == x -> pure (one sr)
             | Just dn <- Map.lookup n (deFollow env) -> pure (Syn sr (EVar dn))
             | otherwise -> advise sr ("`diff` can't follow `" ++ n ++ "` back to `" ++ x
                                       ++ "`: it's worked out from `" ++ x ++ "`, but isn't a real \
                                          \or complex number.")

      ECast a t -> (\da -> Syn sr (ECast da t)) <$> d a
      EWiden t a -> Syn sr . EWiden t <$> d a
      EITE c a b -> (\da db -> Syn sr (EITE c da db)) <$> d a <*> d b

      EBin op a b -> case op of
        OpAdd -> add sr <$> d a <*> d b
        OpSub -> sub sr <$> d a <*> d b
        -- (a b)' = b a' + a b'
        OpMul -> (\da db -> add sr (mul sr b da) (mul sr a db)) <$> d a <*> d b
        -- (a / b)' = (b a' - a b') / b^2
        OpDiv -> (\da db -> divide sr (sub sr (mul sr b da) (mul sr a db)) (mul sr b b))
                   <$> d a <*> d b
        OpPow
          -- (a^n)' = n a^(n-1) a', for n independent of x
          | not (dependsOn b) -> (\da -> mul sr (mul sr b (pow sr a (minusOne sr b))) da) <$> d a
          -- (a^b)' = a^b (b' log a + b a' / a)
          | otherwise -> (\da db -> mul sr e (add sr (mul sr db (call sr "log" a))
                                                     (divide sr (mul sr b da) a)))
                           <$> d a <*> d b
        OpMod -> jumps "`mod`"
        OpIDiv -> jumps "`//`"
        _ -> notImplemented

      EUn OpNeg a -> neg sr <$> d a

      -- (pairs, as tuple assignments make)
      EPair a b -> (\da db -> Syn sr (EPair da db)) <$> d a <*> d b
      EProj isSecond p -> Syn sr . EProj isSecond <$> d p

      -- with respect to a real variable: |u|' = re(conj(u) u') / |u|
      EUn OpAbs a
        | deReal env -> (\da -> divide sr (call sr "re" (mul sr (call sr "conj" a) da)) e) <$> d a
        | otherwise -> notHolomorphic "|·|"

      EApp f a
        | Just rule <- Map.lookup f derivatives -> rule sr a <$> d a
        | Map.member f roundingFunctions -> jumps ("`" ++ f ++ "`")
        | f `elem` ["re", "Re", "im", "Im", "conj", "Conj", "bar", "Bar", "arg", "Arg"] ->
            if deReal env
              then d a <&> \da -> case f of
                _ | f `elem` ["re", "Re"] -> call sr "re" da
                  | f `elem` ["im", "Im"] -> call sr "im" da
                  | f `elem` ["arg", "Arg"] -> call sr "im" (divide sr da a)
                  | otherwise -> call sr "conj" da
              else notHolomorphic ("`" ++ f ++ "`")

      -- d/dx (let p = v in b) = let p = v in (∂b/∂x + ∂b/∂p · dv/dx)
      ELet p v b
        | p == x -> throwError (Internal (Advice sr "differentiating by a shadowed variable"))
        | otherwise -> do
            dbx <- d b
            dbp <- differentiate p b
            dvx <- d v
            pure (Syn sr (ELet p v (addParts sr dbx (scaleParts sr dbp dvx))))

      -- the derivative of a derivative
      EDiff (Syn _ (EVar y)) f -> d =<< differentiate y f
      EDiff y _ -> throwError (DiffInputMustBeVariable (synRange y))
      EPartial (Syn _ (EVar y)) f -> d =<< differentiate y f
      EPartial y _ -> throwError (DiffInputMustBeVariable (synRange y))

      EIndex{} -> throughData
      ETableIndex{} -> throughData
      ETablePoint{} -> throughData
      _ -> notImplemented
  where
    x = deVar env
    d = differentiateWith env
    dependsOn ex = not (Set.null (valueDeps ex `Set.intersection` Set.insert x (deDepends env)))
    notImplemented = throwError (DiffNotImplemented sr x)
    jumps what = advise sr ("`diff` can't follow `" ++ x ++ "` through " ++ what
                            ++ ", which jumps, so has no derivative there.")
    notHolomorphic what = advise sr (what ++ " isn't holomorphic, so it has no derivative with \
                                     \respect to the complex variable `" ++ x ++ "`.")
    throughData = advise sr ("`diff` can't follow `" ++ x ++ "` through an item of a list or \
                             \table: work its derivative out manually instead.")

-- | The variables an expression's value depends on, in a way a derivative
-- could follow: not through conditions (which pick a branch, and aren't
-- differentiated), comparisons or truth values, nor a local binding the
-- rest doesn't use. (Through lists and tables, every variable counts, so
-- that a derivative that would have to follow one says it can't.)
valueDeps :: Expr -> Set String
valueDeps (Syn _ node) = case node of
  EVar n -> Set.singleton n
  ELet n v b -> let inB = valueDeps b
                in if Set.member n inB then Set.delete n inB <> valueDeps v else inB
  EITE _ a b -> valueDeps a <> valueDeps b
  EBin op a b
    | op `elem` [OpOr, OpAnd, OpExactEq, OpApproxEq, OpApproxNEq, OpLt, OpGt, OpLe, OpGe] -> Set.empty
    | otherwise -> valueDeps a <> valueDeps b
  EUn op a
    | op `elem` [OpNot, OpEscapes, OpVanishes, OpStops] -> Set.empty
    | otherwise -> valueDeps a
  ELambda{} -> boundFree (Syn NoSourceRange node)
  ERemove{} -> boundFree (Syn NoSourceRange node)
  EFind{} -> boundFree (Syn NoSourceRange node)
  ETransform{} -> boundFree (Syn NoSourceRange node)
  _ -> foldMap valueDeps node

-- (the free variables of an expression with binders of its own)
boundFree :: Expr -> Set String
boundFree (Syn _ node) = case node of
  EVar n -> Set.singleton n
  ELambda ps b -> boundFree b `Set.difference` Set.fromList ps
  ERemove lst n p -> boundFree lst <> Set.delete n (boundFree p)
  EFind lst n p d -> boundFree lst <> Set.delete n (boundFree p) <> boundFree d
  ETransform lst n f -> boundFree lst <> Set.delete n (boundFree f)
  ELet n v b -> boundFree v <> Set.delete n (boundFree b)
  _ -> foldMap boundFree node

-- | @a + b@ and @a k@, part by part where @a@ and @b@ are pairs (a
-- function's derivative, where the function gives a pair, as a tuple
-- assignment's does): the language has no arithmetic on pairs.
addParts :: SourceRange -> Expr -> Expr -> Expr
addParts sr a b = case (a, b) of
  (Syn psr (EPair a1 a2), Syn _ (EPair b1 b2)) -> Syn psr (EPair (addParts sr a1 b1) (addParts sr a2 b2))
  -- (both from the same function body: the same binding, once)
  (Syn lsr (ELet n v a'), Syn _ (ELet m _ b')) | n == m -> Syn lsr (ELet n v (addParts sr a' b'))
  (Syn lsr (ELet n v a'), _) | not (isInt 0 b) -> Syn lsr (ELet n v (addParts sr a' b))
  (_, Syn lsr (ELet n v b')) | not (isInt 0 a) -> Syn lsr (ELet n v (addParts sr a b'))
  _ -> add sr a b

scaleParts :: SourceRange -> Expr -> Expr -> Expr
scaleParts sr a k = case a of
  Syn psr (EPair a1 a2) -> Syn psr (EPair (scaleParts sr a1 k) (scaleParts sr a2 k))
  Syn lsr (ELet n v a') -> Syn lsr (ELet n v (scaleParts sr a' k))
  _ -> mul sr a k

-- | The derivatives of the builtin functions: given the argument @a@ and
-- its derivative @da@, the derivative of @f(a)@.
derivatives :: Map String (SourceRange -> Expr -> Expr -> Expr)
derivatives = Map.fromList $
  [ ("exp",  \sr a da -> mul sr da (call sr "exp" a))
  , ("log",  \sr a da -> divide sr da a)
  , ("sqrt", \sr a da -> divide sr da (mul sr (int sr 2) (call sr "sqrt" a)))
  , ("cos",  \sr a da -> mul sr (neg sr da) (call sr "sin" a))
  , ("sin",  \sr a da -> mul sr da (call sr "cos" a))
  , ("tan",  \sr a da -> mul sr da (add sr (one sr) (sq sr (call sr "tan" a))))
  , ("cosh", \sr a da -> mul sr da (call sr "sinh" a))
  , ("sinh", \sr a da -> mul sr da (call sr "cosh" a))
  , ("tanh", \sr a da -> mul sr da (sub sr (one sr) (sq sr (call sr "tanh" a))))
  ] ++ concat
  [ [ (f, rule), (short, rule) ]
  | (f, short, rule) <-
      [ ("arccos",  "acos",  \sr a da -> divide sr (neg sr da) (sqrt1 sr (sub sr (one sr) (sq sr a))))
      , ("arcsin",  "asin",  \sr a da -> divide sr da (sqrt1 sr (sub sr (one sr) (sq sr a))))
      , ("arctan",  "atan",  \sr a da -> divide sr da (add sr (one sr) (sq sr a)))
      , ("arccosh", "acosh", \sr a da -> divide sr da (sqrt1 sr (sub sr (sq sr a) (one sr))))
      , ("arcsinh", "asinh", \sr a da -> divide sr da (sqrt1 sr (add sr (one sr) (sq sr a))))
      , ("arctanh", "atanh", \sr a da -> divide sr da (sub sr (one sr) (sq sr a)))
      ]
  ]
  where
    sq sr a = pow sr a (int sr 2)
    sqrt1 sr = call sr "sqrt"

------------------------------------------------------
-- Building expressions, with light simplification
------------------------------------------------------

int :: SourceRange -> Int64 -> Expr
int sr n = Syn sr (ELit (show n) (SomeHaskellType IntegerType n))

zero, one :: SourceRange -> Expr
zero sr = int sr 0
one sr = int sr 1

isInt :: Int64 -> Expr -> Bool
isInt n (Syn _ (ELit _ (SomeHaskellType ty v))) = case ty of
  IntegerType -> v == n
  RealType    -> v == fromIntegral n
  ComplexType -> v == fromIntegral n
  _ -> False
isInt _ _ = False

add, sub, mul, divide, pow :: SourceRange -> Expr -> Expr -> Expr
add sr a b
  | isInt 0 a = b
  | isInt 0 b = a
  | otherwise = Syn sr (EBin OpAdd a b)
sub sr a b
  | isInt 0 b = a
  | isInt 0 a = neg sr b
  | otherwise = Syn sr (EBin OpSub a b)
mul sr a b
  | isInt 0 a || isInt 0 b = zero sr
  | isInt 1 a = b
  | isInt 1 b = a
  | otherwise = Syn sr (EBin OpMul a b)
divide sr a b
  | isInt 0 a = zero sr
  | isInt 1 b = a
  | otherwise = Syn sr (EBin OpDiv a b)
pow sr a b
  | isInt 1 b = a
  | otherwise = Syn sr (EBin OpPow a b)

neg :: SourceRange -> Expr -> Expr
neg sr a
  | isInt 0 a = a
  | otherwise = Syn sr (EUn OpNeg a)

call :: SourceRange -> String -> Expr -> Expr
call sr f a = Syn sr (EApp f a)

-- | @n - 1@, folding integer literals.
minusOne :: SourceRange -> Expr -> Expr
minusOne sr = \case
  Syn lsr (ELit _ (SomeHaskellType IntegerType n)) -> int lsr (n - 1)
  n -> sub sr n (one sr)
