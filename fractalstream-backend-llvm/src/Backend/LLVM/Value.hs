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

import Control.Monad.Reader
import Control.Monad.Except
import Fcf (Pure1)

import qualified Data.Map as Map

value_ :: forall env t m
       . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
      => (String -> Operand)
      -> Value env t
      -> ReaderT (Context OperandPtr env) m (Op t)
value_ getExtern v = ReaderT (\ctx -> buildValue getExtern ctx v)

buildValue :: forall env t' m
            . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
           => (String -> Operand)
           -> Context OperandPtr env
           -> Value env t'
           -> m (Op t')
buildValue getExtern ctx = indexedFoldM @(Pure1 Op) @(Fix (ValueF env)) @(ValueF env) go
  where
    go :: forall t. ValueF env (Pure1 Op) t -> m (Op t)
    go = \case

      Var _ t pf -> withKnownType t (derefOperand (getBinding ctx pf))

      Const (Scalar BooleanProxy b) -> pure (BooleanOp (C.bit (if b then 1 else 0)))
      Const (Scalar IntegerProxy n) -> pure (IntegerOp (C.int32 (fromIntegral n)))
      Const (Scalar RealProxy x) -> pure (RealOp (C.double x))
      Const (Scalar ComplexProxy (x :+ y)) ->
        pure (ComplexOp (C.double x) (C.double y))
      Const (Scalar ColorProxy c) ->
        let (r,g,b) = colorToRGB c
        in pure (ColorOp (C.int8 (fromIntegral r))
                         (C.int8 (fromIntegral g))
                         (C.int8 (fromIntegral b)))

      Const (Scalar (PairProxy t1 t2) (x,y)) ->
        PairOp <$> go (Const (Scalar t1 x)) <*> go (Const (Scalar t2 y))

      And (BooleanOp lhs) (BooleanOp rhs) -> BooleanOp <$> I.and lhs rhs
      Or  (BooleanOp lhs) (BooleanOp rhs) -> BooleanOp <$> I.or  lhs rhs
      Not (BooleanOp x) -> BooleanOp <$> I.xor x (C.bit 1)

      R2C (RealOp x) -> pure (ComplexOp x (C.double 0.0))
      I2R (IntegerOp x) -> RealOp <$> sitofp x AST.double

      ReC (ComplexOp x _) -> pure (RealOp x)
      ImC (ComplexOp _ y) -> pure (RealOp y)

      PairV _ x y           -> pure (PairOp x y)
      ProjV1 _ (PairOp x _) -> pure x
      ProjV2 _ (PairOp _ y) -> pure y

      RGB (RealOp r) (RealOp g) (RealOp b) -> do
        rr <- fmul r (C.double 255.0)
        gg <- fmul g (C.double 255.0)
        bb <- fmul b (C.double 255.0)
        ColorOp <$> fptoui rr AST.i8 <*> fptoui gg AST.i8 <*> fptoui bb AST.i8

      InvertRGB (ColorOp r g b) ->
        ColorOp <$> sub (C.int8 255) r
                <*> sub (C.int8 255) g
                <*> sub (C.int8 255) b

      Blend (RealOp s) (ColorOp r g b) (ColorOp r' g' b') -> do
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

      ITE BooleanProxy (BooleanOp cond) (BooleanOp yes) (BooleanOp no) ->
        BooleanOp <$> select cond yes no

      ITE RealProxy (BooleanOp cond) (RealOp yes) (RealOp no) ->
        RealOp <$> select cond yes no

      ITE IntegerProxy (BooleanOp cond) (IntegerOp yes) (IntegerOp no) ->
        IntegerOp <$> select cond yes no

      ITE ComplexProxy (BooleanOp cond) (ComplexOp yesX yesY) (ComplexOp noX noY) ->
        ComplexOp <$> select cond yesX noX <*> select cond yesY noY

      ITE (PairProxy t1 t2) cond (PairOp x1 x2) (PairOp y1 y2) ->
        PairOp <$> go (ITE t1 cond x1 y1) <*> go (ITE t2 cond x2 y2)

      ITE ColorProxy (BooleanOp cond) (ColorOp r g b) (ColorOp r' g' b') ->
        ColorOp <$> select cond r r' <*> select cond g g' <*> select cond b b'

      Eql BooleanProxy (BooleanOp lhs) (BooleanOp rhs) ->
        BooleanOp <$> icmp P.EQ lhs rhs

      Eql IntegerProxy (IntegerOp lhs) (IntegerOp rhs) ->
        BooleanOp <$> icmp P.EQ lhs rhs

      Eql RealProxy (RealOp lhs) (RealOp rhs) ->
        BooleanOp <$> fcmp P.OEQ lhs rhs

      Eql ComplexProxy (ComplexOp lhsX lhsY) (ComplexOp rhsX rhsY) -> do
        cX <- fcmp P.OEQ lhsX rhsX
        cY <- fcmp P.OEQ lhsY rhsY
        BooleanOp <$> I.and cX cY

      Eql (PairProxy t1 t2) (PairOp x1 x2) (PairOp y1 y2) -> do
        c1 <- getBooleanOp <$> go (Eql t1 x1 y1)
        c2 <- getBooleanOp <$> go (Eql t2 x2 y2)
        BooleanOp <$> I.and c1 c2

      Eql ColorProxy (ColorOp r g b) (ColorOp r' g' b') -> do
        c1 <- icmp P.EQ r r'
        c2 <- icmp P.EQ g g'
        c3 <- icmp P.EQ b b'
        c12 <- I.and c1 c2
        BooleanOp <$> I.and c12 c3

      NEq BooleanProxy (BooleanOp lhs) (BooleanOp rhs) ->
        BooleanOp <$> icmp P.NE lhs rhs

      NEq IntegerProxy (IntegerOp lhs) (IntegerOp rhs) ->
        BooleanOp <$> icmp P.NE lhs rhs

      NEq RealProxy (RealOp lhs) (RealOp rhs) ->
        BooleanOp <$> fcmp P.ONE lhs rhs

      NEq ComplexProxy (ComplexOp lhsX lhsY) (ComplexOp rhsX rhsY) -> do
        cX <- fcmp P.ONE lhsX rhsX
        cY <- fcmp P.ONE lhsY rhsY
        BooleanOp <$> I.and cX cY

      NEq (PairProxy t1 t2) (PairOp x1 x2) (PairOp y1 y2) -> do
        c1 <- getBooleanOp <$> go (NEq t1 x1 y1)
        c2 <- getBooleanOp <$> go (NEq t2 x2 y2)
        BooleanOp <$> I.and c1 c2

      NEq ColorProxy (ColorOp r g b) (ColorOp r' g' b') -> do
        c1 <- icmp P.NE r r'
        c2 <- icmp P.NE g g'
        c3 <- icmp P.NE b b'
        c12 <- I.and c1 c2
        BooleanOp <$> I.and c12 c3

      LEI (IntegerOp lhs) (IntegerOp rhs) -> BooleanOp <$> icmp P.SLE lhs rhs
      LTI (IntegerOp lhs) (IntegerOp rhs) -> BooleanOp <$> icmp P.SLT lhs rhs
      GEI (IntegerOp lhs) (IntegerOp rhs) -> BooleanOp <$> icmp P.SGE lhs rhs
      GTI (IntegerOp lhs) (IntegerOp rhs) -> BooleanOp <$> icmp P.SGT lhs rhs
      LEF (RealOp lhs) (RealOp rhs) -> BooleanOp <$> fcmp P.OLE lhs rhs
      LTF (RealOp lhs) (RealOp rhs) -> BooleanOp <$> fcmp P.OLT lhs rhs
      GEF (RealOp lhs) (RealOp rhs) -> BooleanOp <$> fcmp P.OGE lhs rhs
      GTF (RealOp lhs) (RealOp rhs) -> BooleanOp <$> fcmp P.OGT lhs rhs

      AddI (IntegerOp lhs) (IntegerOp rhs) -> IntegerOp <$> add lhs rhs
      SubI (IntegerOp lhs) (IntegerOp rhs) -> IntegerOp <$> sub lhs rhs
      MulI (IntegerOp lhs) (IntegerOp rhs) -> IntegerOp <$> mul lhs rhs
      DivI (IntegerOp lhs) (IntegerOp rhs) -> IntegerOp <$> sdiv lhs rhs
      ModI (IntegerOp lhs) (IntegerOp rhs) -> IntegerOp <$> srem lhs rhs
      NegI (IntegerOp x) -> IntegerOp <$> sub (C.int32 0) x
      PowI (IntegerOp x) (IntegerOp n) ->
        IntegerOp <$> call (getExtern "powi") [(x, []), (n, [])]
      AbsI (IntegerOp x) -> IntegerOp <$> call (getExtern "absi") [(x, [])]

      AddF (RealOp lhs) (RealOp rhs) -> RealOp <$> fadd lhs rhs
      SubF (RealOp lhs) (RealOp rhs) -> RealOp <$> fsub lhs rhs
      MulF (RealOp lhs) (RealOp rhs) -> RealOp <$> fmul lhs rhs
      DivF (RealOp lhs) (RealOp rhs) -> RealOp <$> fdiv lhs rhs
      ModF (RealOp lhs) (RealOp rhs) -> RealOp <$> frem lhs rhs
      NegF (RealOp x) -> RealOp <$> fsub (C.double 0.0) x

      AddC (ComplexOp lx ly) (ComplexOp rx ry) -> ComplexOp <$> fadd lx rx
                                                            <*> fadd ly ry
      SubC (ComplexOp lx ly) (ComplexOp rx ry) -> ComplexOp <$> fsub lx rx
                                                            <*> fsub ly ry
      MulC (ComplexOp lx ly) (ComplexOp rx ry) -> do
        xx <- fmul lx rx
        yy <- fmul ly ry
        xy <- fmul lx ry
        yx <- fmul ly rx
        ComplexOp <$> fsub xx yy <*> fadd xy yx

      DivC (ComplexOp lx ly) (ComplexOp rx ry) -> do
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

      NegC (ComplexOp x y) -> ComplexOp <$> fsub (C.double 0.0) x
                                        <*> fsub (C.double 0.0) y

      ConjC (ComplexOp x y) -> ComplexOp x <$> fsub (C.double 0.0) y

      AbsF (RealOp x)  -> RealOp <$> call (getExtern "fabs") [(x, [])]
      SqrtF (RealOp x) -> RealOp <$> call (getExtern "sqrt") [(x, [])]
      PowF (RealOp x) (RealOp y) ->
        RealOp <$> call (getExtern "pow") [(x, []), (y, [])]
      LogF (RealOp x) -> RealOp <$> call (getExtern "log") [(x, [])]
      ExpF (RealOp x) -> RealOp <$> call (getExtern "exp") [(x, [])]
      CosF (RealOp x) -> RealOp <$> call (getExtern "cos") [(x, [])]
      SinF (RealOp x) -> RealOp <$> call (getExtern "sin") [(x, [])]
      TanF (RealOp x) -> RealOp <$> call (getExtern "tan") [(x, [])]
      SinhF (RealOp x) -> RealOp <$> call (getExtern "sinh") [(x, [])]
      CoshF (RealOp x) -> RealOp <$> call (getExtern "cosh") [(x, [])]
      TanhF (RealOp x) -> RealOp <$> call (getExtern "tanh") [(x, [])]
      ArcsinF (RealOp x) -> RealOp <$> call (getExtern "asin") [(x, [])]
      ArccosF (RealOp x) -> RealOp <$> call (getExtern "acos") [(x, [])]
      ArctanF (RealOp x) -> RealOp <$> call (getExtern "atan") [(x, [])]
      Arctan2F (RealOp y) (RealOp x) ->
        RealOp <$> call (getExtern "atan2") [(y, []), (x, [])]
      ArcsinhF (RealOp x) -> RealOp <$> call (getExtern "asinh") [(x, [])]
      ArccoshF (RealOp x) -> RealOp <$> call (getExtern "acosh") [(x, [])]
      ArctanhF (RealOp x) -> RealOp <$> call (getExtern "atanh") [(x, [])]

      AbsC (ComplexOp x y) -> do
        xx <- fmul x x
        yy <- fmul y y
        abs2 <- fadd xx yy
        RealOp <$> call (getExtern "sqrt") [(abs2, [])]

      ArgC (ComplexOp x y) ->
        RealOp <$> call (getExtern "atan2") [(y, []), (x, [])]

      ExpC (ComplexOp x y) -> do
        ex <- call (getExtern "exp") [(x, [])]
        cy <- call (getExtern "cos") [(y, [])]
        sy <- call (getExtern "sin") [(y, [])]
        ComplexOp <$> fmul ex cy <*> fmul ex sy

      LogC (ComplexOp x y) -> do
        xx <- fmul x x
        yy <- fmul y y
        norm2 <- fadd xx yy
        logNorm2 <- call (getExtern "log") [(norm2, [])]
        ComplexOp <$> fmul (C.double 0.5) logNorm2
                  <*> call (getExtern "atan2") [(y, []), (x, [])]

      CosC (ComplexOp x y) -> do
        cosX  <- call (getExtern "cos")  [(x, [])]
        coshY <- call (getExtern "cosh") [(y, [])]
        sinX  <- call (getExtern "sin")  [(x, [])]
        sinhY <- call (getExtern "sinh") [(y, [])]
        yy <- fmul sinX sinhY
        ComplexOp <$> fmul cosX coshY <*> fsub (C.double 0.0) yy

      SinC (ComplexOp x y) -> do
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
