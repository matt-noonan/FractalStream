{-# LANGUAGE LambdaCase #-}

module Lang.Expr.Transform
  ( normalize
  , normalizeA
  , embedR
  , constProp
  , complexToReal
  , precompile
  ) where

import Lang.Numbers
import Lang.Expr
import Lang.Expr.Typing
import Utilities

import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.State

-- | When an expression involves operations on a mixed set of
--   real and complex numbers, embed the reals as r |-> r + 0i.
--   The result of this function is a typed expression such
--   that the operands to each operation match the result type.
embedR :: TypedExpr -> TypedExpr
embedR = foldAExpr phi
  where embed = AExpr Complex_T . Embed
        embed' e@(AExpr Real_T _) = embed e
        embed' e = e
        phi Complex_T expr = AExpr Complex_T $ case expr of
          Add xs                      -> Add (sort $ map embed' xs)
          Mul xs                      -> Mul (sort $ map embed' xs)
          Sub x y@(AExpr Real_T _)    -> Sub x (embed y)
          Sub x@(AExpr Real_T _) y    -> Sub (embed x) y
          Div x y@(AExpr Real_T _)    -> Div x (embed y)
          Div x@(AExpr Real_T _) y    -> Div (embed x) y
          Pow x y@(AExpr Real_T _)    -> Pow x (embed y)
          Pow x@(AExpr Real_T _) y    -> Pow (embed x) y
          otherwise                   -> expr
        phi t expr = AExpr t expr

normalize :: Expr -> Expr
normalize = forget . normalizeA . annotateWith (const ())

normalizeA :: Ord t => AExpr t -> AExpr t
normalizeA = foldAExpr phi
  where phi t = AExpr t . \case
          Add xs -> Add $ sort $ concatMap unpackAdd xs
          Mul xs -> Mul $ sort $ concatMap unpackMul xs
          expr@_ -> expr
        unpackAdd (AExpr _ (Add xs)) = xs
        unpackAdd e = [e]
        unpackMul (AExpr _ (Mul xs)) = xs
        unpackMul e = [e]

constProp :: TypedExpr -> TypedExpr
constProp = normalizeA . foldAExpr phi
  where phi Real_T expr = AExpr Real_T $ case expr of

          Add xs -> case mergeAddConsts xs of
                      [] -> Const 0
                      [AExpr _ x] -> x
                      terms@_ -> Add $ sort terms

          Sub (AExpr _ (Const 0)) x -> Neg x
          Sub (AExpr _ x) (AExpr _ (Const 0)) -> x
          Sub (AExpr _ (Const x)) (AExpr _ (Const y)) -> Const $ x - y

          Mul xs -> if not (null $ filter isZero xs)
                      then Const 0
                      else case mergeMulConsts xs of
                        [] -> Const 1
                        [AExpr _ x] -> x
                        terms@_ -> Mul $ sort terms

          Div (AExpr _ (Const 0)) _ -> Const 0
          Div (AExpr _ x) (AExpr _ (Const 1)) -> x
          Div (AExpr _ (Const x)) (AExpr _ (Const y)) -> Const $ x / y

          Pow (AExpr _ (Const 0)) _ -> Const 0
          Pow _ (AExpr _ (Const 0)) -> Const 1
          Pow (AExpr _ x) (AExpr _ (Const 1)) -> x
          Pow (AExpr _ (Const x)) (AExpr _ (Const y)) -> Const $ x ** y

          Neg (AExpr _ (Const x)) -> Const $ negate x

          otherwise -> expr
        phi Complex_T _ = error "cannot use constProp on complex expressions!"
        phi t e = AExpr t e

        isZero = maybe False (== 0) . constValue

        constValue (AExpr _ (Const x)) = Just x
        constValue _ = Nothing

        negatedValue (AExpr _ (Neg x)) = Just x
        negatedValue _ = Nothing

        mergeMulConsts :: [TypedExpr] -> [TypedExpr]
        mergeMulConsts xs = if c == 1 then exprs else (AExpr Real_T (Const c) : exprs)
          where exprs = filter (isNothing . constValue) xs
                c = product $ mapMaybe constValue xs

        mergeAddConsts :: [TypedExpr] -> [TypedExpr]
        mergeAddConsts xs = if c == 0 then diff else (AExpr Real_T (Const c) : diff)
          where exprs = filter (isNothing . constValue) xs
                c = sum $ mapMaybe constValue xs
                pos = toMultiset $ filter (isNothing . negatedValue) exprs
                neg = toMultiset $ mapMaybe negatedValue exprs
                (pos', neg') = Map.partition (> 0) $ Map.filter (/= 0) $ Map.unionWith (-) pos neg
                diff = fromMultiset pos' ++ map (AExpr Real_T . Neg) (fromMultiset neg')

-- | Convert all complex-valued expressions to pairs of real-valued
--   expressions. The result has to be normalized again, because
--   addition and multiplication can lead to newly nested sums.
complexToReal :: TypedExpr -> TypedExpr
complexToReal = normalizeA . foldAExpr phi
  where ra = AExpr Real_T
        unpair (AExpr _ (Pair x y)) = (x,y)
        unzipP = unzip . map (\(AExpr _ (Pair x y)) -> (x,y))
        (==>) = Func_T
        rfun = Real_T ==> Real_T
        phi Complex_T expr = AExpr (Pair_T Real_T Real_T) $ case expr of
          Let s x a  -> letInCExpr s x a
          Var s      -> Pair (ra $ Var $ s ++ ".re") (ra $ Var $ s ++ ".im")
          I          -> Pair (ra $ Const 0) (ra $ Const 1)
          Apply f x  -> error "TODO"
          Lambda s e -> error "Internal error: impossible type, function == scalar"
          Const k    -> Pair (ra $ Const k) (ra $ Const 0)
          Add xs     -> let (re, im) = unzipP xs in
                          Pair (ra $ Add $ sort re) (ra $ Add $ sort im)
          Embed x    -> Pair x (ra $ Const 0)
          Sub x y    -> let (xr,xi) = unpair x
                            (yr,yi) = unpair y in
                          Pair (ra $ Sub xr yr) (ra $ Sub xi yi)
          Mul xs     -> foldl cplxMul (Pair (ra $ Const 1) (ra $ Const 0)) (map getExpr xs)
          Div z w    -> let (x,y) = unpair z
                            (u,v) = unpair w
                            n2 e = ra $ Div (ra e) (ra $ norm2 u v) in
                          Pair (n2 $ Add [ra $ Mul [x,u], ra $ Mul [y,v]])
                               (n2 $ Add [ra $ Mul [y,u], ra $ Mul [x,v]])
          Pow x n    -> error "TODO"
          Neg x      -> let (xr,xi) = unpair x in Pair (ra $ Neg xr) (ra $ Neg xi)
          Conj x     -> let (xr,xi) = unpair x in Pair xr (ra $ Neg xi)
          Diff s x   -> error "TODO"
          Case cs x  -> error "TODO"
          otherwise  -> expr

        phi Real_T expr = AExpr Real_T $ case expr of
          Let s x a  -> letInRExpr s x a
          RealPart x -> let (AExpr Real_T xr, _) = unpair x in xr
          ImagPart x -> let (_, AExpr Real_T xi) = unpair x in xi
          Norm  z@(AExpr Complex_T _) -> let (x,y) = unpair z in (Apply rsqrt (ra $ norm2 x y))
          Norm2 z@(AExpr Complex_T _) -> let (x,y) = unpair z in norm2 x y
          otherwise  -> expr

        phi (Func_T Complex_T Real_T) (Lambda s e) = AExpr (Real_T ==> rfun)
          (Lambda (s ++ ".re") $ AExpr rfun $ Lambda (s ++ ".im") e)

        phi (Func_T Real_T Complex_T) (Lambda s z) =
          let (x,y) = unpair z in AExpr (Pair_T rfun rfun)
            (Pair (AExpr rfun (Lambda s x)) (AExpr rfun (Lambda s y)))

        phi (Func_T Complex_T Complex_T) (Lambda s z) =
          let (x,y) = unpair z
              lambda2 = Lambda (s ++ ".re") . AExpr rfun . Lambda (s ++ ".im")
              rfun2 = Real_T ==> rfun in
            AExpr (Pair_T rfun2 rfun2) (Pair (AExpr rfun2 $ lambda2 x) (AExpr rfun2 $ lambda2 y))

        phi t e = AExpr t e

        rsqrt = AExpr (Func_T Real_T Real_T) (Var "sqrt")
        norm2 x y = Add [ra $ Mul [x, x], ra $ Mul [y, y]]

        cplxMul (Pair x y) (Pair u v) = Pair px py where
          px = ra $ Sub (ra $ Mul [x,u]) (ra $ Mul [y,v])
          py = ra $ Add [ra $ Mul [x,v], ra $ Mul [y,u]]

        letInCExpr s (AExpr _ (Pair x y))
                     (AExpr (Pair_T ut vt) (Pair u v)) = Pair (lets ut u) (lets vt v)
          where lets t = AExpr t . Let (s ++ ".re") x
                       . AExpr t . Let (s ++ ".im") y
        letInCExpr s x (AExpr _ (Pair u v)) = Pair (let' u) (let' v)
          where let' w@(AExpr t _) = AExpr t $ Let s x w
        letInCExpr s x e = Let s x e

        letInRExpr s (AExpr _ (Pair x y)) e@(AExpr t _) = Let (s ++ ".re") x
                                                        $ AExpr t $ Let (s ++ ".im") y e
        letInRExpr s x e = Let s x e

pow2mul :: TypedExpr -> TypedExpr
pow2mul = normalizeA . foldAExpr phi
  where phi ty expr = case expr of
          Pow x (AExpr _ (Const (R n))) -> if fromIntegral (floor n) == n && n > 1
                                              then mkPower ty x (floor n)
                                              else AExpr ty expr
          otherwise -> AExpr ty expr

mkPower :: Type -> TypedExpr -> Int -> TypedExpr
mkPower ty x n = foldl (.) id lets (AExpr ty $ Mul pows)
  where pv_name k = "_^(2^" ++ show k ++ ")"
        pv 0 = x
        pv k = AExpr ty (Var $ pv_name k)
        mkLet k = AExpr ty . Let (pv_name k) (AExpr ty $ Mul [pv $ k - 1, pv $ k - 1])
        pows = (map fst . filter snd) $ zip (map pv [0..]) (bits n)
        lets = map mkLet [1 .. length (bits n) - 1]
        bits 0 = []
        bits k = odd k : bits (k `div` 2)

simplifyLet :: TypedExpr -> TypedExpr
simplifyLet = normalizeA . foldAExpr phi
  where phi ty expr = case expr of
          Let s x e -> case countUsesOf s (forget e) of
                        0 -> e
                        1 -> substitute s x e
                        _ -> AExpr ty expr
          otherwise -> AExpr ty expr

countUsesOf :: Symbol -> Expr -> Int
countUsesOf s e = snd $ runState (foldExprM phi e) 0
  where phi :: ExprF () -> State Int ()
        phi = \case
                Var s'    -> if s == s' then modify (+1) else return ()
                otherwise -> return ()

substitute :: Symbol -> AExpr a -> AExpr a -> AExpr a
substitute s x = foldAExpr phi
  where phi ty expr = case expr of
          Var s'    -> if s == s' then x else AExpr ty expr
          otherwise -> AExpr ty expr

precompile :: TypedExpr -> TypedExpr
precompile = normalizeA . simplifyLet . pow2mul . constProp . complexToReal . embedR . normalizeA . simplifyLet . pow2mul
