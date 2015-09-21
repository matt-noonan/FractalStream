{- |
Module      : Lang.Expr
Description : Expression ASTs and manipulation.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.Expr where

import Lang.Numbers

import Data.List

data Function a where
    Sqrt :: Function R
    Exp  :: Function R
    Log  :: Function R
    Cos  :: Function R
    Sin  :: Function R
    CExp :: Function C
    --CUser :: String -> Function C
    --User  :: String -> Function R
    --CFunc :: String -> Expr C -> Function C
    --Func  :: String -> Expr R -> Function R
deriving instance Show a => Show (Function a)
deriving instance Ord a => Ord (Function a)
deriving instance Eq a => Eq (Function a)

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

{---------}
{---------  Conversion from complex expressions to pairs of real expressions ----------}
{---------}

toRExpr :: Expr C -> (Expr R, Expr R)

toRExpr (CConst z) = (Const (R x), Const (R y)) where (x,y) = coords z

toRExpr (CVar v) = (Var $ v ++ "@x", Var $ v ++ "@y")

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
            | ns == []    = Add $ sort ps
            | ps == []    = Neg $ Add $ sort ns
            | otherwise   = simplifyRExpr $ Sub (Add ps) (Add ns)
          where (ns', ps)  = partition isNeg terms
                ns = map stripNeg ns'
                stripNeg x = case x of { Neg x' -> x'; _ -> x }

simplifyRExpr (Mul xs)
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
        mul = Mul . sort

simplifyRExpr expr = expr
