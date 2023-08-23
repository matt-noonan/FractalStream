{-# language OverloadedStrings #-}
module Backend.LLVM.Value
  ( value_
  , buildValue
  , getGetExtern
  ) where

import Backend.LLVM.Operand

import Language.Value
import Data.Indexed.Functor
import Data.Color

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as P
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.AST.Type as AST

import Data.Functor
import Control.Monad.Reader
import Control.Monad.Except
import Fcf (Exp, Eval)

import qualified Data.Map as Map

value_ :: forall env t m
       . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
      => (String -> Operand)
      -> Value '(env, t)
      -> ReaderT (Context OperandPtr env) m (Op t)
value_ = buildValue

data CtxOp :: (* -> *) -> (Environment, FSType) -> Exp *
type instance Eval (CtxOp m et) =
  ReaderT (Context OperandPtr (Env et)) m (Op (Ty et))

buildValue :: forall env t' m
            . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
           => (String -> Operand)
           -> Value '(env, t')
           -> ReaderT (Context OperandPtr env) m (Op t')
buildValue getExtern = indexedFold @(CtxOp m) @(Fix ValueF) @ValueF go'
  where
    go' :: forall et. ValueF (CtxOp m) et -> Eval (CtxOp m et)
    go' x = case toIndex x of { EnvType _ -> go x }

    go :: forall env' t. (KnownEnvironment env') => ValueF (CtxOp m) '(env', t) -> Eval (CtxOp m '(env', t))
    go = \case
      Var _ t pf -> withKnownType t $ do
        ctx <- ask
        derefOperand (getBinding ctx pf)

      Const (Scalar BooleanType b) -> pure (BooleanOp (C.bit (if b then 1 else 0)))
      Const (Scalar IntegerType n) -> pure (IntegerOp (C.int32 (fromIntegral n)))
      Const (Scalar RealType x) -> pure (RealOp (C.double x))
      Const (Scalar ComplexType (x :+ y)) ->
        pure (ComplexOp (C.double x) (C.double y))
      Const (Scalar ColorType c) ->
        let (r,g,b) = colorToRGB c
        in pure (ColorOp (C.int8 (fromIntegral r))
                         (C.int8 (fromIntegral g))
                         (C.int8 (fromIntegral b)))

      Const (Scalar (PairType t1 t2) (x,y)) ->
        PairOp <$> go @env' (Const (Scalar t1 x)) <*> go @env' (Const (Scalar t2 y))

      And x y -> ((,) <$> x <*> y) >>= \case
        (BooleanOp lhs, BooleanOp rhs) -> BooleanOp <$> I.and lhs rhs
      Or x y -> ((,) <$> x <*> y) >>= \case
        (BooleanOp lhs, BooleanOp rhs) -> BooleanOp <$> I.or  lhs rhs
      Not b -> b >>= \case { BooleanOp x -> BooleanOp <$> I.xor x (C.bit 1) }

      R2C r -> r <&> \case { RealOp    x -> ComplexOp x (C.double 0.0) }
      I2R i -> i >>= \case { IntegerOp x -> RealOp <$> sitofp x AST.double }

      ReC z -> z <&> \case { ComplexOp x _ -> RealOp x }
      ImC z -> z <&> \case { ComplexOp _ y -> RealOp y }

      PairV _ x y -> PairOp <$> x <*> y
      ProjV1 _ p  -> p <&> \case { PairOp x _ -> x }
      ProjV2 _ p  -> p <&> \case { PairOp _ y -> y }

      RGB mr mg mb -> ((,,) <$> mr <*> mg <*> mb) >>= \case
        (RealOp r, RealOp g, RealOp b) -> do
          rr <- fmul r (C.double 255.0)
          gg <- fmul g (C.double 255.0)
          bb <- fmul b (C.double 255.0)
          ColorOp <$> fptoui rr AST.i8 <*> fptoui gg AST.i8 <*> fptoui bb AST.i8

      InvertRGB c -> c >>= \case
        ColorOp r g b -> ColorOp <$> sub (C.int8 255) r
                                 <*> sub (C.int8 255) g
                                 <*> sub (C.int8 255) b

      Blend ms mc mc' -> ((,,) <$> ms <*> mc <*> mc') >>= \case
        (RealOp s, ColorOp r g b, ColorOp r' g' b') -> do
          rf  <- uitofp r  AST.double
          gf  <- uitofp g  AST.double
          bf  <- uitofp b  AST.double
          rf' <- uitofp r' AST.double
          gf' <- uitofp g' AST.double
          bf' <- uitofp b' AST.double
          rr <- fmul s rf
          gg <- fmul s gf
          bb <- fmul s bf
          s' <- fsub (C.double 1.0) s
          rr' <- fmul s' rf'
          gg' <- fmul s' gf'
          bb' <- fmul s' bf'
          cr <- fadd rr rr'
          cg <- fadd gg gg'
          cb <- fadd bb bb'
          ColorOp <$> fptoui cr AST.i8 <*> fptoui cg AST.i8 <*> fptoui cb AST.i8

      ITE BooleanType mc myes mno -> ((,,) <$> mc <*> myes <*> mno) >>= \case
        (BooleanOp cond, BooleanOp yes, BooleanOp no) ->
          BooleanOp <$> select cond yes no

      ITE RealType mc myes mno -> ((,,) <$> mc <*> myes <*> mno) >>= \case
        (BooleanOp cond, RealOp yes, RealOp no) ->
          RealOp <$> select cond yes no

      ITE IntegerType mc myes mno -> ((,,) <$> mc <*> myes <*> mno) >>= \case
        (BooleanOp cond, IntegerOp yes, IntegerOp no) ->
          IntegerOp <$> select cond yes no

      ITE ComplexType mc myes mno -> ((,,) <$> mc <*> myes <*> mno) >>= \case
        (BooleanOp cond, ComplexOp yesX yesY, ComplexOp noX noY) ->
          ComplexOp <$> select cond yesX noX <*> select cond yesY noY

      ITE (PairType t1 t2) cond mx my -> ((,) <$> mx <*> my) >>= \case
        (PairOp x1 x2, PairOp y1 y2) ->
          PairOp <$> go @env' (ITE t1 cond (pure x1) (pure y1))
                 <*> go @env' (ITE t2 cond (pure x2) (pure y2))

      ITE ColorType mc myes mno -> ((,,) <$> mc <*> myes <*> mno) >>= \case
        (BooleanOp cond, ColorOp r g b, ColorOp r' g' b') ->
          ColorOp <$> select cond r r' <*> select cond g g' <*> select cond b b'

      Eql BooleanType x y -> ((,) <$> x <*> y) >>= \case
        (BooleanOp lhs, BooleanOp rhs) ->
          BooleanOp <$> icmp P.EQ lhs rhs

      Eql IntegerType x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) ->
           BooleanOp <$> icmp P.EQ lhs rhs

      Eql RealType x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) ->
           BooleanOp <$> fcmp P.OEQ lhs rhs

      Eql ComplexType x y -> ((,) <$> x <*> y) >>= \case
         (ComplexOp lhsX lhsY, ComplexOp rhsX rhsY) -> do
           cX <- fcmp P.OEQ lhsX rhsX
           cY <- fcmp P.OEQ lhsY rhsY
           BooleanOp <$> I.and cX cY

      Eql (PairType t1 t2) x y -> ((,) <$> x <*> y) >>= \case
          (PairOp x1 x2, PairOp y1 y2) -> do
            c1 <- getBooleanOp <$> go @env' (Eql t1 (pure x1) (pure y1))
            c2 <- getBooleanOp <$> go @env' (Eql t2 (pure x2) (pure y2))
            BooleanOp <$> I.and c1 c2

      Eql ColorType x y -> ((,) <$> x <*> y) >>= \case
          (ColorOp r g b, ColorOp r' g' b') -> do
            c1 <- icmp P.EQ r r'
            c2 <- icmp P.EQ g g'
            c3 <- icmp P.EQ b b'
            c12 <- I.and c1 c2
            BooleanOp <$> I.and c12 c3

      NEq BooleanType x y -> ((,) <$> x <*> y) >>= \case
           (BooleanOp lhs, BooleanOp rhs) ->
             BooleanOp <$> icmp P.NE lhs rhs

      NEq IntegerType x y -> ((,) <$> x <*> y) >>= \case
           (IntegerOp lhs, IntegerOp rhs) ->
             BooleanOp <$> icmp P.NE lhs rhs

      NEq RealType x y -> ((,) <$> x <*> y) >>= \case
           (RealOp lhs, RealOp rhs) ->
             BooleanOp <$> fcmp P.ONE lhs rhs

      NEq ComplexType x y -> ((,) <$> x <*> y) >>= \case
           (ComplexOp lhsX lhsY, ComplexOp rhsX rhsY) -> do
             cX <- fcmp P.ONE lhsX rhsX
             cY <- fcmp P.ONE lhsY rhsY
             BooleanOp <$> I.or cX cY

      NEq (PairType t1 t2) x y -> ((,) <$> x <*> y) >>= \case
           (PairOp x1 x2, PairOp y1 y2) -> do
             c1 <- getBooleanOp <$> go @env' (NEq t1 (pure x1) (pure y1))
             c2 <- getBooleanOp <$> go @env' (NEq t2 (pure x2) (pure y2))
             BooleanOp <$> I.or c1 c2

      NEq ColorType x y -> ((,) <$> x <*> y) >>= \case
           (ColorOp r g b, ColorOp r' g' b') -> do
             c1 <- icmp P.NE r r'
             c2 <- icmp P.NE g g'
             c3 <- icmp P.NE b b'
             c12 <- I.or c1 c2
             BooleanOp <$> I.or c12 c3

      LEI x y -> ((,) <$> x <*> y) >>= \case
        (IntegerOp lhs, IntegerOp rhs) -> BooleanOp <$> icmp P.SLE lhs rhs
      LTI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> BooleanOp <$> icmp P.SLT lhs rhs
      GEI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> BooleanOp <$> icmp P.SGE lhs rhs
      GTI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> BooleanOp <$> icmp P.SGT lhs rhs
      LEF x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) -> BooleanOp <$> fcmp P.OLE lhs rhs
      LTF x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) -> BooleanOp <$> fcmp P.OLT lhs rhs
      GEF x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) -> BooleanOp <$> fcmp P.OGE lhs rhs
      GTF x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) -> BooleanOp <$> fcmp P.OGT lhs rhs

      AddI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> IntegerOp <$> add lhs rhs
      SubI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> IntegerOp <$> sub lhs rhs
      MulI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> IntegerOp <$> mul lhs rhs
      DivI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> IntegerOp <$> sdiv lhs rhs
      ModI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> IntegerOp <$> srem lhs rhs
      NegI i -> i >>= \case
         (IntegerOp x) -> IntegerOp <$> sub (C.int32 0) x
      PowI mx my -> ((,) <$> mx <*> my) >>= \case
         (IntegerOp x, IntegerOp n) ->
           IntegerOp <$> call (getExtern "powi") [(x, []), (n, [])]
      AbsI i -> i >>= \case
        IntegerOp x -> IntegerOp <$> call (getExtern "absi") [(x, [])]

      AddF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fadd lhs rhs
      SubF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fsub lhs rhs
      MulF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fmul lhs rhs
      DivF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fdiv lhs rhs
      ModF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> frem lhs rhs
      NegF r -> r >>= \case
          (RealOp x) -> RealOp <$> fsub (C.double 0.0) x

      AddC x y -> ((,) <$> x <*> y) >>= \case
          (ComplexOp lx ly, ComplexOp rx ry) -> ComplexOp <$> fadd lx rx
                                                          <*> fadd ly ry
      SubC x y -> ((,) <$> x <*> y) >>= \case
          (ComplexOp lx ly, ComplexOp rx ry) -> ComplexOp <$> fsub lx rx
                                                          <*> fsub ly ry
      MulC x y -> ((,) <$> x <*> y) >>= \case
          (ComplexOp lx ly, ComplexOp rx ry) -> do
            xx <- fmul lx rx
            yy <- fmul ly ry
            xy <- fmul lx ry
            yx <- fmul ly rx
            ComplexOp <$> fsub xx yy <*> fadd xy yx

      DivC x y -> ((,) <$> x <*> y) >>= \case
          (ComplexOp lx ly, ComplexOp rx ry) -> do
            rx2 <- fmul rx rx
            ry2 <- fmul ry ry
            r2 <- fadd rx2 ry2
            xx <- fmul lx rx
            yy <- fmul ly ry
            xy <- fmul lx ry
            yx <- fmul ly rx
            zx <- fadd xx yy
            zy <- fsub yx xy
            ComplexOp <$> fdiv zx r2 <*> fdiv zy r2

      NegC z -> z >>= \case
        ComplexOp x y -> ComplexOp <$> fsub (C.double 0.0) x
                                   <*> fsub (C.double 0.0) y

      ConjC z -> z >>= \case
        ComplexOp x y -> ComplexOp x <$> fsub (C.double 0.0) y

      AbsF r -> r >>= \case
        (RealOp x)  -> RealOp <$> call (getExtern "fabs") [(x, [])]
      SqrtF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "sqrt") [(x, [])]
      PowF r1 r2 -> ((,) <$> r1 <*> r2) >>= \case
         (RealOp x, RealOp y) ->
           RealOp <$> call (getExtern "pow") [(x, []), (y, [])]
      LogF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "log") [(x, [])]
      ExpF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "exp") [(x, [])]
      CosF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "cos") [(x, [])]
      SinF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "sin") [(x, [])]
      TanF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "tan") [(x, [])]
      SinhF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "sinh") [(x, [])]
      CoshF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "cosh") [(x, [])]
      TanhF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "tanh") [(x, [])]
      ArcsinF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "asin") [(x, [])]
      ArccosF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "acos") [(x, [])]
      ArctanF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "atan") [(x, [])]
      Arctan2F r1 r2 -> ((,) <$> r1 <*> r2) >>= \case
        (RealOp y, RealOp x) ->
          RealOp <$> call (getExtern "atan2") [(y, []), (x, [])]
      ArcsinhF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "asinh") [(x, [])]
      ArccoshF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "acosh") [(x, [])]
      ArctanhF r -> r >>= \case
         (RealOp x) -> RealOp <$> call (getExtern "atanh") [(x, [])]

      AbsC z -> z >>= \case
        ComplexOp x y -> do
          xx <- fmul x x
          yy <- fmul y y
          abs2 <- fadd xx yy
          RealOp <$> call (getExtern "sqrt") [(abs2, [])]

      ArgC z -> z >>= \case
        ComplexOp x y -> RealOp <$> call (getExtern "atan2") [(y, []), (x, [])]

      ExpC z -> z >>= \case
        ComplexOp x y -> do
          ex <- call (getExtern "exp") [(x, [])]
          cy <- call (getExtern "cos") [(y, [])]
          sy <- call (getExtern "sin") [(y, [])]
          ComplexOp <$> fmul ex cy <*> fmul ex sy

      LogC z -> z >>= \case
        ComplexOp x y -> do
          xx <- fmul x x
          yy <- fmul y y
          norm2 <- fadd xx yy
          logNorm2 <- call (getExtern "log") [(norm2, [])]
          ComplexOp <$> fmul (C.double 0.5) logNorm2
                    <*> call (getExtern "atan2") [(y, []), (x, [])]

      CosC z -> z >>= \case
        ComplexOp x y -> do
          cosX  <- call (getExtern "cos")  [(x, [])]
          coshY <- call (getExtern "cosh") [(y, [])]
          sinX  <- call (getExtern "sin")  [(x, [])]
          sinhY <- call (getExtern "sinh") [(y, [])]
          yy <- fmul sinX sinhY
          ComplexOp <$> fmul cosX coshY <*> fsub (C.double 0.0) yy

      SinC z -> z >>= \case
        ComplexOp x y -> do
          cosX  <- call (getExtern "cos")  [(x, [])]
          coshY <- call (getExtern "cosh") [(y, [])]
          sinX  <- call (getExtern "sin")  [(x, [])]
          sinhY <- call (getExtern "sinh") [(y, [])]
          ComplexOp <$> fmul sinX coshY <*> fmul cosX sinhY


      _ -> error "TODO: unhandled Value constructor"


getGetExtern :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => m (String -> Operand)
getGetExtern = do
  es <- mapM (\(name, getter) -> (name,) <$> getter) $
    [ ("absi", extern "llvm.abs.i32" [AST.i32] AST.i32)
    , ("powi", extern "llvm.powi.i32" [AST.i32] AST.i32)
    , ("log", extern "llvm.log.f64" [AST.double] AST.double)
    , ("exp", extern "llvm.exp.f64" [AST.double] AST.double)
    , ("pow", extern "llvm.pow.f64" [AST.double, AST.double] AST.double)
    , ("cos", extern "llvm.cos.f64" [AST.double] AST.double)
    , ("sin", extern "llvm.sin.f64" [AST.double] AST.double)
    , ("tan", extern "tan" [AST.double] AST.double)
    , ("acos", extern "acos" [AST.double] AST.double)
    , ("asin", extern "asin" [AST.double] AST.double)
    , ("atan", extern "atan" [AST.double] AST.double)
    , ("atan2", extern "atan2" [AST.double, AST.double] AST.double)
    , ("sqrt", extern "llvm.sqrt.f64" [AST.double] AST.double)
    , ("fabs", extern "llvm.fabs.f64" [AST.double] AST.double)
    , ("cosh", extern "cosh" [AST.double] AST.double)
    , ("sinh", extern "sinh" [AST.double] AST.double)
    , ("tanh", extern "tanh" [AST.double] AST.double)
    , ("acosh", extern "acosh" [AST.double] AST.double)
    , ("asinh", extern "asinh" [AST.double] AST.double)
    , ("atanh", extern "atanh" [AST.double] AST.double)
    ]
  let m = Map.fromList es
  pure (m Map.!)
