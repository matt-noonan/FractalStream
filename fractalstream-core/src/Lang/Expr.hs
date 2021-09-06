

module Lang.Expr
  ( Symbol
  , Type(..)
  , Expr(..)
  , ExprF(..)
  , BuiltinFunc(..)
  , builtinFuncType
  , var
  , apply
  , fromDouble
  , AExpr(..)
  , getAnnotation
  , getExpr
  , forget
  , upgrade
  , annotate
  , annotateWith
  , annotateM
  , annotateWithM
  , foldExpr
  , foldExprM
  , foldAExpr
  , foldAExprM
  ) where

import           Lang.Numbers
import           Utilities

type Symbol = String

data Type
  = Bool_T
  | Real_T
  | Complex_T
  | Pair_T Type Type
  | Func_T Type Type
  | Row_T Int Type
  | Col_T Int Type
  | Poly_T Symbol
  | TypeOf Symbol
  | Underconstrained Symbol
  deriving (Eq, Ord, Show)

builtinFuncType :: BuiltinFunc -> Type
builtinFuncType = \case
    FunExp     -> r_to_r
    FunLog     -> r_to_r
    FunPow _ _ -> r_to_r
    FunCos     -> r_to_r
    FunSin     -> r_to_r
    FunTan     -> r_to_r
  where r_to_r = Func_T Real_T Real_T

data BuiltinFunc
  = FunExp
  | FunLog
  | FunPow Int Int
  | FunCos
  | FunSin
  | FunTan
  deriving (Eq, Ord)

instance Show BuiltinFunc where
  show = \case
    FunExp     -> "#exp"
    FunLog     -> "#log"
    FunPow x y -> "#pow{" ++ show x ++ "/" ++ show y ++ "}"
    FunCos     -> "#cos"
    FunSin     -> "#sin"
    FunTan     -> "#tan"


newtype Expr = Fix (ExprF Expr) deriving (Eq, Ord)

unfix :: Expr -> ExprF Expr
unfix (Fix e) = e

data ExprF a
  = Let Symbol a a
  | Var Symbol
  | Apply a a
  | Lambda Symbol a
  | Const R
  | I
  | Embed a
  | Add [a]
  | Sub a a
  | Mul [a]
  | Div a a
  | Pow a a
  | Neg a
  | Conj a
  | RealPart a
  | ImagPart a
  | Norm a
  | Norm2 a
  | Diff Symbol a
  | Case [(a, a)] a
  | Transpose a
  | Row Int [a]
  | Col Int [a]
  | Pair a a
  | Builtin BuiltinFunc
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Num Expr where
  (Fix (Add xs)) + (Fix (Add ys)) = Fix $ Add (sort $ xs ++ ys)
  (Fix (Add xs)) + y = Fix $ Add (sort $ xs ++ [y])
  x + (Fix (Add ys)) = Fix $ Add $ sort (x : ys)
  x + y = Fix $ Add $ sort [x,y]
  x - y = Fix $ x `Sub` y
  (Fix (Mul xs)) * (Fix (Mul ys)) = Fix $ Mul (sort $ xs ++ ys)
  (Fix (Mul xs)) * y = Fix $ Mul (sort $ xs ++ [y])
  x * (Fix (Mul ys)) = Fix $ Mul $ sort (x : ys)
  x * y = Fix $ Mul [x,y]
  abs = Fix . Norm
  signum x = Fix $ x `Div` abs x
  fromInteger = Fix . Const . fromInteger
  negate = Fix . Neg

instance Fractional Expr where
  recip x = Fix $ (Fix $ Conj x) `Div` (Fix $ Norm2 x)
  fromRational = Fix . Const . fromRational

instance Floating Expr where
  pi    = Fix $ Const pi
  exp   = apply (var "exp")
  log   = apply (var "log")
  sin   = apply (var "sin")
  cos   = apply (var "cos")
  tan   = apply (var "tan")
  asin  = apply (var "asin")
  acos  = apply (var "acos")
  atan  = apply (var "atan")
  sinh  = apply (var "sinh")
  cosh  = apply (var "cosh")
  tanh  = apply (var "tanh")
  asinh = apply (var "asinh")
  acosh = apply (var "acosh")
  atanh = apply (var "atanh")
  sqrt  = apply (var "sqrt")
  x ** n = Fix $ x `Pow` n

var :: Symbol -> Expr
var = Fix . Var

apply :: Expr -> Expr -> Expr
apply f = Fix . Apply f

fromDouble :: Double -> Expr
fromDouble = Fix . Const . R

-- | Expression AST with an annotation of type t at each AST node.
--   shachaf on #haskell suggests looking at cofree comonads to
--   de-boilerplate this type and the associated fold functions.
data AExpr t = AExpr t (ExprF (AExpr t))
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- | Extract the top-level annotation on an annotated expression.
getAnnotation :: AExpr t -> t
getAnnotation (AExpr x _) = x

-- | Expose the top-level ExprF constructor.
getExpr :: AExpr t -> ExprF (AExpr t)
getExpr (AExpr _ x) = x

-- | Annotate each subexpression with the result of
--   applying the given function.
annotateWith :: (Expr -> t) -> Expr -> AExpr t
annotateWith = annotate . (. Fix . fmap forget)

-- | Annotate each subexpression with the result of applying
--   the given monadic function.  Monadic results are gathered
--   in preorder over the expression's AST.
annotateWithM :: Monad m => (Expr -> m t) -> Expr -> m (AExpr t)
annotateWithM = annotateM . (. Fix . fmap forget)

-- | Forget annotations on an expression.
forget :: AExpr t -> Expr
forget = foldAExpr (const Fix)

-- | Lift a function on expressions to a function on annotated expressions.
upgrade :: (Expr -> t) -> (AExpr t -> t)
upgrade = (.forget)

-- | Add annotations to an expression based on the current
--   AST node and the annotations on its children.
annotate :: (ExprF (AExpr t) -> t) -> Expr -> AExpr t
annotate f = foldExpr (\e -> AExpr (f e) e)

annotateM :: Monad m => (ExprF (AExpr t) -> m t) -> Expr -> m (AExpr t)
annotateM f = foldExprM $ \e -> do
    ann <- f e
    return $ AExpr ann e

-- | Fold over an expression, implemented as a catamorphism a la recursion-schemes.
foldExpr :: (ExprF a -> a) -> Expr -> a
foldExpr f = f . fmap (foldExpr f) . unfix

-- | Fold over an expression, inside the Kleisi category for a monad m.
foldExprM :: Monad m => (ExprF a -> m a) -> Expr -> m a
foldExprM f = (f =<<) . sequence . fmap (foldExprM f) . unfix

-- | Fold over an annotated expression.
foldAExpr :: (t -> ExprF a -> a) -> AExpr t -> a
foldAExpr f (AExpr x e) = f x $ fmap (foldAExpr f) e

-- | Fold over an annotated expression, inside the Kleisi category for a monad m.
foldAExprM :: Monad m => (t -> ExprF a -> m a) -> AExpr t -> m a
foldAExprM f (AExpr x e) = f x =<< sequence (fmap (foldAExprM f) e)

