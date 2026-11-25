-- | Symbolic differentiation of surface expressions: @diff(e, x)@ is
-- replaced (during typing) by the derivative of @e@ with respect to the
-- variable @x@, which is then typed and elaborated like any other
-- expression.
--
-- With respect to a complex variable, the derivative is the Wirtinger
-- derivative ∂ = ½(∂/∂x − i ∂/∂y): the usual complex derivative wherever
-- the expression is holomorphic, and defined for the rest too (@|z|@,
-- @re@, @im@, @conj@, @arg@). The chain rule then needs ∂̄ = ½(∂/∂x +
-- i ∂/∂y) of what's inside, too, so both are worked out together
-- ('differentiatePair'), ∂̄ as "zero" for anything holomorphic, so that
-- the derivative of holomorphic code is just what it always was.
--
-- Derivatives are taken wherever they exist: as an @if@'s condition isn't
-- differentiated (the derivative is that of the branch taken), @round@,
-- @floor@, @ceiling@ and @//@ have derivative 0, @mod@ follows its
-- definition, and @|x|@ has derivative 0 at 0.
--
-- Variables other than @x@ are treated as constants, except for those
-- bound by local bindings (from inlined function calls, for example),
-- which are handled by the chain rule:
--
-- > ∂/∂x (let p = v in b) = let p = v in (∂b/∂x + ∂b/∂p · ∂v + ∂b/∂p̄ · ∂v̄)
--
-- A subexpression that doesn't mention @x@ at all has derivative zero.
module Language.Value.Differentiate
  ( differentiate
  , differentiateWith
  , differentiatePair
  , DiffEnv(..)
  , valueDeps
  , displayName
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bits (xor)
import Data.Char (ord)

import Language.Type
import Language.Typecheck
import Language.Value.Syntax
import Language.Value.Builtins (roundingFunctions)
import Language.Parser.SourceRange

-- | What differentiating needs to know besides the expression: the
-- variable, whether it's real (so that the derivative is the plain
-- derivative, and ∂̄ isn't needed), and the variables whose values depend
-- on it, each with the variable holding its derivative (see
-- "Language.Code.Derivatives"; a variable that depends on it but has
-- none, like a list worked out from it, is in the set without one), and
-- with the one holding its ∂̄, for those whose ∂̄ isn't zero.
data DiffEnv = DiffEnv
  { deVar       :: String
  , deReal      :: Bool
  , deFollow    :: Map String String
  , deDepends   :: Set String
  , deFollowBar :: Map String String
  }

-- | The derivative of an expression with respect to the named variable,
-- with every other variable a constant: what @partial@ means.
differentiate :: String -> Expr -> TC Expr
differentiate x = differentiateWith (DiffEnv x False Map.empty Set.empty Map.empty)

-- | The derivative of an expression with respect to a variable, following
-- the variables that depend on it to their derivatives.
differentiateWith :: DiffEnv -> Expr -> TC Expr
differentiateWith env e = fst <$> differentiatePair env e

-- | A name as a script would know it: an internal name made for a
-- function's parameter (@[internal] f.p #3@, @[internal] f'.p #3@) is the
-- parameter's.
displayName :: String -> String
displayName n
  | "[internal] " `isPrefixOf` n =
      let rest = drop (length ("[internal] " :: String)) n
          base = takeWhile (/= ' ') rest
      in case break (== '.') (reverse base) of
           (p, _ : _) | not (null p) -> reverse p
           _ -> n
  | otherwise = n

-- | ∂ and ∂̄ of an expression with respect to the variable (∂̄ 'Nothing'
-- where it's zero: always, for a real variable, whose derivative is just
-- the first).
differentiatePair :: DiffEnv -> Expr -> TC (Expr, Maybe Expr)
differentiatePair env e@(Syn sr node)
  | not (dependsOn e) = pure (zero sr, Nothing)
  | otherwise = case node of

      EVar n | n == x -> pure (one sr, Nothing)
             | Just dn <- Map.lookup n (deFollow env) ->
                 pure (Syn sr (EVar dn), Syn sr . EVar <$> Map.lookup n (deFollowBar env))
             | otherwise -> advise sr ("`diff` can't follow `" ++ displayName n ++ "` back to `"
                                       ++ displayName x ++ "`: it's worked out from `" ++ displayName x
                                       ++ "`, but isn't a number.")

      -- (with respect to a complex variable, a real conversion's
      -- derivative is complex)
      ECast a t
        | not (deReal env), t `elem` [RealT, IntegerT] -> both (Syn sr . EWiden ComplexT) <$> d a
        | otherwise -> both (\da -> Syn sr (ECast da t)) <$> d a
      EWiden t a -> both (Syn sr . EWiden t) <$> d a
      EITE c a b -> lift2 (\da db -> Syn sr (EITE c da db)) <$> d a <*> d b

      EBin op a b -> case op of
        OpAdd -> lift2 (add sr) <$> d a <*> d b
        OpSub -> lift2 (sub sr) <$> d a <*> d b
        -- (a b)' = b a' + a b'
        OpMul -> lift2 (\da db -> add sr (mul sr b da) (mul sr a db)) <$> d a <*> d b
        -- (a / b)' = (a' - (a / b) b') / b, with b worked out once: never
        -- b^2, which could overflow (or underflow) where b doesn't, and
        -- which, for an integer b such as 2^k, would be an integer product
        -- that wraps (b is only ever divided by here, so it's computed as
        -- a real number). For a b that doesn't depend on x, just a' / b.
        OpDiv -> lift2 (\da db ->
          if isInt 0 db then divide sr da b
          else let n = divisorName e
                   v = Syn sr (EVar n)
               in Syn sr (ELet n (Syn (synRange b) (EWiden RealT b))
                                 (divide sr (sub sr da (mul sr (divide sr a v) db)) v)))
                 <$> d a <*> d b
        OpPow
          -- (a^n)' = n a^(n-1) a', for n independent of x
          | not (dependsOn b) -> both (\da -> mul sr (mul sr b (pow sr a (minusOne sr b))) da) <$> d a
          -- (a^b)' = a^b (b' log a + b a' / a)
          | otherwise -> lift2 (\da db -> mul sr e (add sr (mul sr db (call sr "log" a))
                                                           (divide sr (mul sr b da) a)))
                           <$> d a <*> d b
        -- (a mod b = a - |b| floor(a / |b|), Euclidean: just a', for a b
        -- that doesn't depend on x)
        OpMod
          | not (dependsOn b) -> d a
          | otherwise -> let ab = Syn sr (EUn OpAbs b)
                         in d (sub sr a (mul sr ab (call sr "floor" (divide sr a ab))))
        -- (an integer, which only jumps)
        OpIDiv -> pure (zero sr, Nothing)
        _ -> notImplemented

      EUn OpNeg a -> both (neg sr) <$> d a

      -- (pairs, as tuple assignments make)
      EPair a b -> lift2 (\da db -> Syn sr (EPair da db)) <$> d a <*> d b
      EProj isSecond p -> both (Syn sr . EProj isSecond) <$> d p

      -- |u| = (u ū)^(1/2), and 0 where u is (where it has no derivative)
      EUn OpAbs a
        | deReal env -> d a <&> \(da, _) ->
            (atZero a (divide sr (call sr "re" (mul sr (call sr "conj" a) da)) e), Nothing)
        | otherwise -> d a <&> \da ->
            let twice = mul sr (real sr 2) e
            in both (atZero a) (wirtinger (divide sr (call sr "conj" a) twice) (divide sr a twice) da)

      EApp f a
        | Just rule <- Map.lookup f derivatives -> both (rule sr a) <$> d a
        -- (piecewise constant)
        | Map.member f roundingFunctions -> pure (zero sr, Nothing)
        | f `elem` ["re", "Re", "im", "Im", "conj", "Conj", "bar", "Bar", "arg", "Arg"] ->
            if deReal env
              then d a <&> \(da, _) -> (,Nothing) $ case f of
                _ | f `elem` ["re", "Re"] -> call sr "re" da
                  | f `elem` ["im", "Im"] -> call sr "im" da
                  | f `elem` ["arg", "Arg"] -> call sr "im" (divide sr da a)
                  | otherwise -> call sr "conj" da
              else d a <&> \da -> case f of
                -- re u = (u + ū)/2, im u = (u - ū)/(2i), arg u = im(log u)
                _ | f `elem` ["re", "Re"] -> wirtinger (real sr 0.5) (real sr 0.5) da
                  | f `elem` ["im", "Im"] -> wirtinger (complex sr (0 :+ (-0.5))) (complex sr (0 :+ 0.5)) da
                  | f `elem` ["arg", "Arg"] -> wirtinger (divide sr (complex sr (0 :+ (-0.5))) a)
                                                         (divide sr (complex sr (0 :+ 0.5)) (call sr "conj" a)) da
                  | otherwise -> wirtinger (zero sr) (one sr) da

      -- ∂/∂x (let p = v in b) = let p = v in (∂b/∂x + ∂b/∂p ∂v + ∂b/∂p̄ ∂v̄),
      -- and ∂̄ likewise (with respect to a real x, ∂v̄ is the conjugate of
      -- v's derivative)
      ELet p v b
        | p == x -> throwError (Internal (Advice sr "differentiating by a shadowed variable"))
        | otherwise -> do
            (dbx, dbbx) <- d b
            (dbp, dbbp) <- differentiatePair (DiffEnv p False Map.empty Set.empty Map.empty) b
            (dvx, dbvx) <- d v
            let bound parts = Syn sr (ELet p v parts)
                conjOf = call sr "conj"
                -- ∂v̄ and ∂̄v̄
                dvbar = if deReal env then Just (conjOf dvx) else conjOf <$> dbvx
                dbvbar = conjOf dvx
                total = addParts sr dbx (addParts sr (scaleParts sr dbp dvx) (scalePartsM dbbp dvbar))
                totalBar = sumM [ dbbx, scalePartsM' dbp dbvx, (\k -> scaleParts sr k dbvbar) <$> dbbp ]
            pure (bound total, if deReal env then Nothing else bound <$> totalBar)

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
    d = differentiatePair env
    dependsOn ex = not (Set.null (valueDeps ex `Set.intersection` Set.insert x (deDepends env)))
    notImplemented = throwError (DiffNotImplemented sr (displayName x))
    throughData = advise sr ("`diff` can't follow `" ++ displayName x ++ "` through an item of a list or \
                             \table: work its derivative out manually instead.")

    both f (a, mb) = (f a, f <$> mb)
    -- (a rule linear in each derivative, for ∂ and for ∂̄; ∂̄ is zero when
    -- both of the parts' are)
    lift2 f (da, mda) (db, mdb) = (f da db, case (mda, mdb) of
      (Nothing, Nothing) -> Nothing
      _ -> Just (f (fromMaybe (zero sr) mda) (fromMaybe (zero sr) mdb)))

    -- A function F(u, ū) of u, given ∂F/∂u and ∂F/∂ū at u, and ∂u and ∂̄u:
    -- ∂F = F_u ∂u + F_ū ∂ū, ∂̄F = F_u ∂̄u + F_ū ∂̄ū, where ∂ū is the conjugate
    -- of ∂̄u and ∂̄ū that of ∂u
    wirtinger fu fubar (du, mdbu) =
      ( add sr (mul sr fu du) (maybe (zero sr) (mul sr fubar . call sr "conj") mdbu)
      , sumM [ mul sr fu <$> mdbu, Just (mul sr fubar (call sr "conj" du)) ] )

    -- (0 where u is 0)
    atZero u r = Syn sr (EITE (Syn sr (EBin OpExactEq (Syn sr (EUn OpAbs u)) (zero sr))) (zero sr) r)

    sumM ms = case catMaybes ms of
      [] -> Nothing
      t : ts -> Just (foldl' (addParts sr) t ts)
    scalePartsM ma mk = case (ma, mk) of
      (Just a', Just k) -> scaleParts sr a' k
      _ -> zero sr
    scalePartsM' a' mk = scaleParts sr a' <$> mk

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

real :: SourceRange -> Double -> Expr
real sr v = Syn sr (ELit (show v) (SomeHaskellType RealType v))

complex :: SourceRange -> Complex Double -> Expr
complex sr v = Syn sr (ELit (show v) (SomeHaskellType ComplexType v))

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

-- | The name a quotient's derivative binds its divisor to: made from the
-- quotient itself (its shape, not where it is), so that it's different
-- from the name of any quotient around it or inside it, as a derivative
-- of a derivative needs (see the rule for 'ELet', which can't
-- differentiate by a name an inner binding takes again).
divisorName :: Expr -> String
divisorName q = "[internal] divisor #" ++ show (fnv1a (show (fmap (const ()) q)))
  where
    fnv1a :: String -> Word64
    fnv1a = foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) 14695981039346656037

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
