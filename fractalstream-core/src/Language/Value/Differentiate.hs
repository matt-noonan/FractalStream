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
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Type
import Language.Typecheck
import Language.Value.Syntax
import Language.Parser.SourceRange

-- | The derivative of an expression with respect to the named variable.
differentiate :: String -> Expr -> TC Expr
differentiate x e@(Syn sr node)
  | not (mentions x e) = pure (zero sr)
  | otherwise = case node of

      EVar n | n == x -> pure (one sr)

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
          | not (mentions x b) -> (\da -> mul sr (mul sr b (pow sr a (minusOne sr b))) da) <$> d a
          -- (a^b)' = a^b (b' log a + b a' / a)
          | otherwise -> (\da db -> mul sr e (add sr (mul sr db (call sr "log" a))
                                                     (divide sr (mul sr b da) a)))
                           <$> d a <*> d b
        _ -> notImplemented

      EUn OpNeg a -> neg sr <$> d a

      EApp f a | Just rule <- Map.lookup f derivatives -> rule sr a <$> d a

      -- d/dx (let p = v in b) = let p = v in (∂b/∂x + ∂b/∂p · dv/dx)
      ELet p v b
        | p == x -> throwError (Internal (Advice sr "differentiating by a shadowed variable"))
        | otherwise -> do
            dbx <- d b
            dbp <- differentiate p b
            dvx <- d v
            pure (Syn sr (ELet p v (add sr dbx (mul sr dbp dvx))))

      -- the derivative of a derivative
      EDiff (Syn _ (EVar y)) f -> d =<< differentiate y f
      EDiff y _ -> throwError (DiffInputMustBeVariable (synRange y))

      _ -> notImplemented
  where
    d = differentiate x
    notImplemented = throwError (DiffNotImplemented sr x)

-- | Does the expression refer to the named variable?
mentions :: String -> Expr -> Bool
mentions x (Syn _ node) = case node of
  EVar n -> n == x
  ERemove lst n p -> mentions x lst || (n /= x && mentions x p)
  EFind lst n p def -> mentions x lst || (n /= x && mentions x p) || mentions x def
  ETransform lst n f -> mentions x lst || (n /= x && mentions x f)
  ELet n v b -> mentions x v || (n /= x && mentions x b)
  _ -> any (mentions x) node

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
