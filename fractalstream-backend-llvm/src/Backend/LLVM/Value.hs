module Backend.LLVM.Value
  ( value_
  , buildValue
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

value_ :: forall env t m
       . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
      => Value env t
      -> ReaderT (Context Operand_ env) m (Op t)
value_ v = ReaderT (`buildValue` v)


buildValue :: forall env t' m
            . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
           => Context Operand_ env
           -> Value env t'
           -> m (Op t')
buildValue ctx = indexedFoldM @(Pure1 Op) @(Fix (ValueF env)) @(ValueF env) go
  where
    go :: forall t. ValueF env (Pure1 Op) t -> m (Op t)
    go = \case

      Var _ t pf -> withKnownType t (derefOperand t (getBinding ctx pf))

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

      _ -> error "TODO: unhandled Value constructor"
