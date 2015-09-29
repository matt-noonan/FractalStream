{- |
Module      : Lang.Expr
Description : Expression ASTs and manipulation.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.Expr where

import Lang.Numbers

import Data.List
import qualified Data.Map as Map

data Function a where
    Sqrt :: Function R
    Exp  :: Function R
    Log  :: Function R
    Cos  :: Function R
    Sin  :: Function R
    Tan  :: Function R
    CExp :: Function C
    CLog :: Function C
    --CUser :: String -> Function C
    --User  :: String -> Function R
    --CFunc :: String -> Expr C -> Function C
    --Func  :: String -> Expr R -> Function R
deriving instance Show a => Show (Function a)
deriving instance Ord a => Ord (Function a)
deriving instance Eq a => Eq (Function a)

doCCall :: Function C -> (C -> C)
doCCall CExp = exp
doCCall CLog = log

doCall :: Function R -> (R -> R)
doCall Sqrt = sqrt
doCall Exp = exp
doCall Log = log
doCall Cos = cos
doCall Sin = sin
doCall Tan = tan

doCall2 :: Function (R,R) -> (R -> R -> R)
doCall2 = error "TODO"

data Expression
  = ComplexExpr (Expr C)
  | RealExpr (Expr R)

data Expr a where
    CConst :: C -> Expr C
    CEmbed  :: Expr R -> Expr C
    CVar :: String -> Expr C
    CAdd :: Expr C -> Expr C -> Expr C
    CSub :: Expr C -> Expr C -> Expr C
    CNeg :: Expr C -> Expr C
    CConj :: Expr C -> Expr C
    CMul :: Expr C -> Expr C -> Expr C
    CDiv :: Expr C -> Expr C -> Expr C
    CCall :: Function C -> Expr C -> Expr C
    CRealPart :: Expr C -> Expr R
    CImagPart :: Expr C -> Expr R
    CPow :: Expr C -> Expr C -> Expr C
    Const :: R -> Expr R
    Var :: String -> Expr R
    Norm :: Expr C -> Expr R
    Norm2 :: Expr C -> Expr R
    Add :: [Expr R] -> Expr R
    Sub :: Expr R -> Expr R -> Expr R
    Neg :: Expr R -> Expr R
    Abs :: Expr R -> Expr R
    Mul :: [Expr R] -> Expr R
    Div :: Expr R -> Expr R -> Expr R
    Pow :: Expr R -> Expr R -> Expr R
    Call :: Function R -> Expr R -> Expr R
    Call2 :: Function (R,R) -> Expr R -> Expr R -> Expr R
deriving instance Show a => Show (Expr a)
deriving instance Ord a => Ord (Expr a)
deriving instance Eq a => Eq (Expr a)

instance Num (Expr C) where
  (+) = CAdd
  (-) = CSub
  (*) = CMul
  abs = CEmbed . Norm
  signum z = (CConj z) `CDiv` (CEmbed $ Norm z)
  fromInteger = CConst . fromInteger
  negate = CNeg

instance Fractional (Expr C) where
  recip z = (CConj z) `CDiv` (CEmbed $ Norm2 z)
  fromRational = CConst . fromRational

instance Floating (Expr C) where
  pi = CEmbed $ Const pi
  exp _  = error "unimplemented"
  log _  = error "unimplemented"
  sin _  = error "unimplemented"
  cos _  = error "unimplemented"
  tan _  = error "unimplemented"
  asin _ = error "unimplemented"
  acos _ = error "unimplemented"
  atan _ = error "unimplemented"
  sinh _  = error "unimplemented"
  cosh _  = error "unimplemented"
  tanh _  = error "unimplemented"
  asinh _ = error "unimplemented"
  acosh _ = error "unimplemented"
  atanh _ = error "unimplemented"
  sqrt = error "unimplemented"
  (**) = CPow

instance Num (Expr R) where
  x + y = Add [x,y]
  (-) = Sub
  x * y = Mul [x,y]
  abs = Abs
  signum x = x `Div` (Abs x)
  fromInteger = Const . fromInteger
  negate = Neg

instance Fractional (Expr R) where
  recip x = 1 `Div` x
  fromRational = Const . fromRational

instance Floating (Expr R) where
  pi = Const pi
  exp = Call Exp
  log = Call Log
  sin = Call Sin
  cos = Call Cos
  tan = Call Tan
  asin _ = error "unimplemented"
  acos _ = error "unimplemented"
  atan _ = error "unimplemented"
  sinh _  = error "unimplemented"
  cosh _  = error "unimplemented"
  tanh _  = error "unimplemented"
  asinh _ = error "unimplemented"
  acosh _ = error "unimplemented"
  atanh _ = error "unimplemented"
  sqrt = Call Sqrt
  (**) = Pow

{---------}
{---------  Conversion from complex expressions to pairs of real expressions ----------}
{---------}

toRExpr :: Expr C -> (Expr R, Expr R)

toRExpr (CConst z) = (Const (R x), Const (R y)) where (x,y) = coords z

toRExpr (CVar v) = (Var $ v ++ ".re", Var $ v ++ ".im")

toRExpr (CEmbed r) = (r, Const $ R 0)

toRExpr (CAdd z z') = (Add [x, x'], Add [y, y'])
    where (x , y ) = toRExpr z
          (x', y') = toRExpr z'

toRExpr (CSub z z') = (Sub x x', Sub y y')
    where (x , y ) = toRExpr z
          (x', y') = toRExpr z'

toRExpr (CMul z z') = (Sub (Mul [x, x']) (Mul [y, y']), Add [Mul [x, y'], Mul [y, x']])
    where (x , y ) = toRExpr z
          (x', y') = toRExpr z'

toRExpr (CNeg z) = (Neg x, Neg y) where (x,y) = toRExpr z

toRExpr (CConj z) = (x, Neg y) where (x,y) = toRExpr z

toRExpr (CDiv z z') = (Div u k, Div v k)
    where (u, v) = toRExpr $ CMul z (CConj z')
          (x, y) = toRExpr z'
          k = Add [Mul [x, x], Mul [y, y]]

toRExpr (CCall CExp z) = (Mul [Call Exp logr, Call Cos theta],
                          Mul [Call Exp logr, Call Sin theta])
    where (logr, theta) = toRExpr z

toRExpr (CPow z n) = case n of
  CConst n' -> if n' == fromIntegral (realIntOf n')
               then toRExpr $ mkPower CMul (realIntOf n') z
               else complexExponential
  _ -> complexExponential
  where complexExponential = toRExpr $ CCall CExp (CMul n (CCall CLog z))

mkPower times n x = (foldr times 1 . map fst . filter snd) (zip (power2s x) (bits n))
  where power2s p = p : power2s (p `times` p)
        bits 0 = []
        bits k = odd k : bits (k `div` 2)

{---------}
{---------  Elimination of all complex-valued subexpressions -------------}
{---------}

toPureRExpr :: Expr R -> Expr R

toPureRExpr (CRealPart z) = toPureRExpr x where (x,_) = toRExpr z
toPureRExpr (CImagPart z) = toPureRExpr y where (_,y) = toRExpr z
toPureRExpr (Norm z)  = toPureRExpr $ Call Sqrt (Norm2 z)
toPureRExpr (Norm2 z) = toPureRExpr $ Add [Mul [x, x], Mul [y, y]] where (x, y) = toRExpr z

toPureRExpr (Add xs) = Add $ map toPureRExpr xs
toPureRExpr (Sub x y) = Sub (toPureRExpr x) (toPureRExpr y)
toPureRExpr (Mul xs) = Mul $ map toPureRExpr xs
toPureRExpr (Div x y) = Div (toPureRExpr x) (toPureRExpr y)

toPureRExpr (Abs x) = Abs (toPureRExpr x)
toPureRExpr (Neg x) = Neg (toPureRExpr x)

toPureRExpr (Call f x) = Call f (toPureRExpr x)
toPureRExpr (Call2 f x y) = Call2 f (toPureRExpr x) (toPureRExpr y)

toPureRExpr expr = expr

{---------}
{---------  Some basic expression simplification -------------}
{---------}

simplifyRExpr :: Expr R -> Expr R

simplifyRExpr (Sub x y)
    | y' == Const (R 0)  = x'
    | x' == y'           = Const (R 0)
    | otherwise          = case y' of
                            Neg y'' -> simplifyRExpr $ Add [x', y'']
                            _       -> Sub x' y'
  where x' = simplifyRExpr x
        y' = simplifyRExpr y

simplifyRExpr (Div x y)
    | y' == Const (R 1)  = x'
    | x' == Const (R 0)  = x'
    | x' == y'           = Const (R 1)
    | otherwise          = Div x' y'
  where x' = simplifyRExpr x
        y' = simplifyRExpr y

simplifyRExpr (Abs x) = case x' of
    Const (R u) -> Const $ R $ abs u
    _           -> Abs x'
  where x' = simplifyRExpr x

simplifyRExpr (Neg x) = case x' of
    Const (R u) -> Const $ R $ negate u
    _           -> Neg x'
  where x' = simplifyRExpr x

simplifyRExpr (Call Cos x) = case x' of
    Neg t  -> Call Cos t
    _      -> Call Cos x'
  where x' = simplifyRExpr x

simplifyRExpr (Call Sin x) = case x' of
    Neg t  -> Call Sin t
    _      -> Call Sin x'
  where x' = simplifyRExpr x

simplifyRExpr (Call Exp x) = case x' of
    Const (R 0) -> Const (R 1)
    _           -> Call Exp x'
  where x' = simplifyRExpr x

simplifyRExpr (Call f x) = Call f $ simplifyRExpr x  -- TODO: special cases for individual functions
simplifyRExpr (Call2 f x y) = Call2 f (simplifyRExpr x) (simplifyRExpr y)

simplifyRExpr (Add xs)
    | xs == []      = 0
    | consts == 0   = case vars of
            [x] -> x
            _   -> add vars
    | vars == []    = Const $ R consts
    | otherwise     = add $ (Const $ R consts) : vars
  where xs' = map simplifyRExpr xs
        isConst x = case x of
            Const _ -> True
            _       -> False
        constVal :: Expr R -> Double
        constVal (Const (R x)) = x
        constVal _ = 0
        (nums, vars) = partition isConst xs'
        consts = sum $ 0 : map constVal nums
        isNeg x = case x of { Neg _ -> True; _ -> False }
        add terms
            | terms == [] = 0
            | ns == []    = Add $ sort ps
            | ps == []    = Neg $ Add $ sort ns
            | otherwise   = simplifyRExpr $ Sub (Add ps) (Add ns)
          where (ns', ps)  = partition isNeg terms
                ns = map stripNeg ns'
                stripNeg x = case x of { Neg x' -> x'; _ -> x }

simplifyRExpr (Mul xs)
    | xs == []                    = 1
    | consts == 1                 = case vars of
                                        [x] -> sign $ x
                                        _   -> sign $ mul vars
    | vars == [] || consts == 0   = Const $ R consts
    | otherwise                   = sign $ mul $ (Const $ R consts) : vars
  where xs'' = map simplifyRExpr xs
        sign = cycle [id, Neg] !! sum (map countSign xs'')
        countSign (Neg _) = 1
        countSign _       = 0
        xs' = map stripSign xs''
        stripSign (Neg x) = x
        stripSign x       = x
        isConst x = case x of
            Const _ -> True
            _       -> False
        constVal :: Expr R -> Double
        constVal (Const (R x)) = x
        constVal _ = 1
        (nums, vars) = partition isConst xs'
        consts = product $ 1 : map constVal nums
        mul xs = case xs of
          [] -> 1
          _  -> Mul $ sort xs

simplifyRExpr expr = expr

data Bindings = Bindings
  { varC :: Map.Map String (Expr C)
  , varR :: Map.Map String (Expr R)
  }

-- | Evaluate an expression in a given context.
--   The context is represented by a map from variable names to expressions.
--   Currently, recursive bindings are disallowed by removing a binding from
--   the context when recursively calling evalExpr on the binding's value.
evalExpr :: Bindings -> Expr a -> Expr a

evalExpr ctx e = case e of
  CConst _   -> e
  CEmbed x   -> CEmbed $ ev x
  CVar s    -> evalExpr (ctx { varC = (varC ctx) `without` s }) $ Map.findWithDefault e s (varC ctx)
  CAdd z z'  -> CAdd (evC z) (evC z')
  CSub z z'  -> CSub (evC z) (evC z')
  CNeg z     -> CNeg (evC z)
  CConj z    -> CConj (evC z)
  CMul z z'  -> CMul (evC z) (evC z')
  CDiv z z'  -> CDiv (evC z) (evC z')
  CCall f z  -> CCall f (evC z)
  CRealPart z -> CRealPart (evC z)
  CImagPart z -> CImagPart (evC z)
  CPow z n    -> CPow (evC z) (evC n)
  Const _     -> e
  Var s     -> evalExpr (ctx { varR = (varR ctx) `without` s }) $ Map.findWithDefault e s (varR ctx)
  Norm z      -> Norm (evC z)
  Norm2 z     -> Norm2 (evC z)
  Add xs      -> Add $ map ev xs
  Sub x y     -> Sub (ev x) (ev y)
  Neg x       -> Neg (ev x)
  Abs x       -> Abs (ev x)
  Mul xs      -> Mul $ map ev xs
  Div x y     -> Div (ev x) (ev y)
  Pow x y     -> Pow (ev x) (ev y)
  Call f x    -> Call f (ev x)
  Call2 f x y -> Call2 f (ev x) (ev y)
 where evC = (evalExpr ctx) :: Expr C -> Expr C  -- explicitly typed because of some GADT
       ev  = (evalExpr ctx) :: Expr R -> Expr R  -- behavior that I don't really understand..
       without = (flip Map.delete) :: Map.Map String (Expr a) -> String -> Map.Map String (Expr a)

-- | Perform constant propagation on the given expression.
constProp :: Expr a -> Expr a

constProp k@(CConst _) = k

constProp (CEmbed x) = let x' = constProp x in case x' of
  Const (R r) -> CConst $ complex r 0
  _  -> CEmbed x'

constProp k@(CVar _) = k

constProp (CAdd x y) = case (x',y') of
    (CConst 0, _) -> y'
    (_, CConst 0) -> x'
    (CConst z, CConst z') -> CConst $ z + z'
    _ -> CAdd x' y'
  where (x',y') = (constProp x, constProp y)

constProp (CSub x y) = case (x',y') of
    (CConst 0, _) -> constProp $ CNeg y'
    (_, CConst 0) -> x'
    (CConst z, CConst z') -> CConst $ z - z'
    _ -> CSub x' y'
  where (x',y') = (constProp x, constProp y)

constProp (CNeg x) = case x' of
    CConst z -> CConst (-z)
    _ -> CNeg x'
  where x' = constProp x

constProp (CConj z) = case z' of
    CConst w -> CConst $ conj w
    _ -> CConj z'
  where z' = constProp z

constProp (CMul z w) = case (z',w') of
    (CConst 0, _) -> CConst 0
    (_, CConst 0) -> CConst 0
    (CConst 1, p) -> p
    (p, CConst 1) -> p
    (CConst x, CConst y) -> CConst $ x * y
    _ -> CMul z' w'
  where (z',w') = (constProp z, constProp w)

constProp (CDiv z w) = case (z',w') of
    (CConst 0, _) -> CConst 0
    (p, CConst 1) -> p
    (CConst x, CConst y) -> CConst $ x / y
    _ -> CDiv z' w'
  where (z',w') = (constProp z, constProp w)

constProp (CCall f z) = case z' of
    CConst w -> CConst $ doCCall f w
    _ -> CCall f z'
  where z' = constProp z

constProp (CRealPart z) = case z' of
    CConst w -> (Const . R . realPart) w
    _ -> CRealPart z'
  where z' = constProp z

constProp (CImagPart z) = case z' of
    CConst w -> (Const . R . imagPart) w
    _ -> CImagPart z'
  where z' = constProp z

constProp (CPow z n) = case (z',n') of
    (CConst x, CConst y) -> CConst $ x ** y
    _ -> CPow z' n'
  where (z',n') = (constProp z, constProp n)

constProp k@(Const _) = k

constProp k@(Var _) = k

constProp (Add xs) = case (k, ts) of
    (_, []) -> Const k
    (0, _)  -> Add ts
    _       -> Add (Const k : ts)
  where isConst (Const _) = True
        isConst _ = False
        xs' = map constProp xs
        ts = filter (not . isConst) xs'
        constVal :: Expr R -> R
        constVal (Const x) = x
        constVal _ = 0
        k = sum $ map constVal xs'

constProp (Sub x y) = case (x',y') of
    (Const 0, _) -> constProp $ Neg y'
    (_, Const 0) -> x'
    (Const z, Const z') -> Const $ z - z'
    _ -> Sub x' y'
  where (x',y') = (constProp x, constProp y)

constProp (Neg x) = case x' of
    Const z -> Const (-z)
    _ -> Neg x'
  where x' = constProp x

constProp (Abs x) = case x' of
    Const z -> Const $ abs z
    _ -> Abs x'
  where x' = constProp x

constProp (Mul xs) = case (k, ts) of
    (_, []) -> Const k
    (0, _)  -> Const 0
    (1, _)  -> Mul ts
    _       -> Mul (Const k : ts)
  where isConst (Const _) = True
        isConst _ = False
        xs' = map constProp xs
        ts = filter (not . isConst) xs'
        constVal :: Expr R -> R
        constVal (Const x) = x
        constVal _ = 1
        k = product $ map constVal xs'

constProp (Div z w) = case (z',w') of
    (Const 0, _) -> Const 0
    (p, Const 1) -> p
    (Const x, Const y) -> Const $ x / y
    _ -> Div z' w'
  where (z',w') = (constProp z, constProp w)

constProp (Pow z n) = case (z',n') of
    (Const x, Const y) -> Const $ x ** y
    _ -> Pow z' n'
  where (z',n') = (constProp z, constProp n)

constProp (Call f z) = case z' of
    Const w -> Const $ doCall f w
    _ -> Call f z'
  where z' = constProp z

constProp (Call2 f x y) = case (x',y') of
    (Const u, Const v) -> Const $ doCall2 f u v
    _ -> Call2 f x' y'
  where (x', y') = (constProp x, constProp y)
