{-# language OverloadedStrings, RecursiveDo #-}
module Backend.LLVM.Value
  ( value_
  , buildValue
  , getGetExtern
  ) where

import FractalStream.Prelude

import Backend.LLVM.Operand

import Language.Value
import Data.Indexed.Functor
import Data.Color

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as P
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.AST.Type as AST

import Control.Monad.Fix
import qualified Data.Map as Map

value_ :: forall env t m
       . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
      => (String -> Operand)
      -> ArenaState
      -> Value '(env, t)
      -> ReaderT (Context OperandPtr env) m (Op t)
value_ = buildValue

data CtxOp :: (* -> *) -> (Environment, FSType) -> Exp *
type instance Eval (CtxOp m et) =
  ReaderT (Context OperandPtr (Env et)) m (Op (Ty et))

buildValue :: forall env t' m
            . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
           => (String -> Operand)
           -> ArenaState
           -> Value '(env, t')
           -> ReaderT (Context OperandPtr env) m (Op t')
buildValue getExtern arena = indexedFold go'
  where
    go' :: forall et. ValueF (CtxOp m) et -> Eval (CtxOp m et)
    go' x = case toIndex x of { EnvType _ -> go x }

    go :: forall env' t. (KnownEnvironment env') => ValueF (CtxOp m) '(env', t) -> Eval (CtxOp m '(env', t))
    go = \case
      Var _ t pf -> withKnownType t $ do
        ctx <- ask
        derefOperand (getBinding ctx pf)

      LocalLet name vt pf v _ e -> recallIsAbsent pf $ do
        vptr <- allocaOp vt
        vv <- v
        storeOperand vv vptr
        withReaderT (Bind name vt vptr) e

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

      Const (Scalar t _) ->
        throwError ("The LLVM backend can not compile constants of type " ++ showType t)

      -- ------------------------------------------------------------------ --
      -- List literal: allocate one node per element in the arena, link them.
      -- ------------------------------------------------------------------ --
      List ty mElems -> do
        let stride = listNodeStride ty
        elems <- sequence mElems   -- evaluate all elements first
        case elems of
          [] -> pure (ListOp nullI8Ptr)
          _  -> lift $ buildLitList arena ty stride elems

      -- ------------------------------------------------------------------ --
      -- Join: copy every list from every input (conservative; safe for
      -- future mutations).  Concatenate the copies in order.
      -- ------------------------------------------------------------------ --
      Join ty mLists -> do
        lists <- sequence mLists
        let headPtrs = map getListOp lists
        lift $ buildJoin arena ty headPtrs

      -- ------------------------------------------------------------------ --
      -- Remove: filter by predicate; keep elements where pred is FALSE.
      -- ------------------------------------------------------------------ --
      Remove name ty pf mxs mtest -> recallIsAbsent pf $ do
        ctx <- ask
        xs <- mxs
        let headPtr = getListOp xs
        elemSlot <- allocaOp ty
        lift $ buildFilter arena ty headPtr elemSlot $ \slot -> do
          testOp <- runReaderT mtest (Bind name ty slot ctx)
          detypeOperand BooleanType testOp

      -- ------------------------------------------------------------------ --
      -- Transform: apply a function to every element, producing a new list.
      -- ------------------------------------------------------------------ --
      Transform name ty1 _ty2 pf mxs mf -> recallIsAbsent pf $ do
        ctx <- ask
        xs <- mxs
        let headPtr = getListOp xs
            ty2 = _ty2
        elemSlot <- allocaOp ty1
        lift $ buildMap arena ty1 ty2 headPtr elemSlot $ \slot ->
          runReaderT mf (Bind name ty1 slot ctx)

      -- ------------------------------------------------------------------ --
      -- Find: return the first element matching pred, or the default.
      -- ------------------------------------------------------------------ --
      Find name ty pf mxs mtest mdefault -> recallIsAbsent pf $ do
        ctx <- ask
        xs    <- mxs
        defOp <- mdefault
        let headPtr = getListOp xs
        elemSlot <- allocaOp ty
        lift $ buildFind arena ty headPtr elemSlot defOp $ \slot -> do
          testOp <- runReaderT mtest (Bind name ty slot ctx)
          detypeOperand BooleanType testOp

      -- ------------------------------------------------------------------ --
      -- Length: count nodes.
      -- ------------------------------------------------------------------ --
      Length ty mxs -> do
        xs <- mxs
        let headPtr = getListOp xs
        lift $ buildLength headPtr (toLLVMType ty)

      -- ------------------------------------------------------------------ --
      -- Index: 1-based indexing, optionally cyclic.
      -- ------------------------------------------------------------------ --
      Index ty cyclic mxs mi -> do
        xs <- mxs
        i  <- mi
        let headPtr = getListOp xs
            iOp = case i of IntegerOp v -> v
        lift $ buildIndex arena ty cyclic headPtr iOp

      -- ------------------------------------------------------------------ --
      -- Range: integer range [lo..hi].
      -- ------------------------------------------------------------------ --
      Range mlo mhi -> do
        lo <- mlo
        hi <- mhi
        let loOp = case lo of IntegerOp v -> v
            hiOp = case hi of IntegerOp v -> v
        lift $ buildRange arena loOp hiOp

      ConcatText {} -> throwError "The LLVM backend can not compile text values"

      And x y -> ((,) <$> x <*> y) >>= \case
        (BooleanOp lhs, BooleanOp rhs) -> BooleanOp <$> I.and lhs rhs
      Or x y -> ((,) <$> x <*> y) >>= \case
        (BooleanOp lhs, BooleanOp rhs) -> BooleanOp <$> I.or  lhs rhs
      Not b -> b >>= \case { BooleanOp x -> BooleanOp <$> I.xor x (C.bit 1) }

      R2C r -> r <&> \case { RealOp    x -> ComplexOp x (C.double 0.0) }
      I2R i -> i >>= \case { IntegerOp x -> RealOp <$> sitofp x AST.double }
      C2R2 z -> z <&> \case { ComplexOp x y -> PairOp (RealOp x) (RealOp y) }
      ToText {} -> pure TextOp

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

      ITE t _ _ _ ->
        throwError ("In an if/then/else expression, the LLVM backend cannot handle the type " ++
                   showType t)

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

      Eql t _ _ ->
        throwError ("In an equality comparison, the LLVM backend cannot handle the type " ++
                   showType t)

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

      NEq t _ _ ->
        throwError ("In an inequality comparison, the LLVM backend cannot handle the type " ++
                   showType t)

      LTI x y -> ((,) <$> x <*> y) >>= \case
         (IntegerOp lhs, IntegerOp rhs) -> BooleanOp <$> icmp P.SLT lhs rhs
      LTF x y -> ((,) <$> x <*> y) >>= \case
         (RealOp lhs, RealOp rhs) -> BooleanOp <$> fcmp P.OLT lhs rhs

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
        IntegerOp x -> IntegerOp <$> call (getExtern "absi") [(x, []), (C.bit 0, [])]

      AddF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fadd lhs rhs
      SubF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fsub lhs rhs
      MulF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fmul lhs rhs
      DivF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> RealOp <$> fdiv lhs rhs
      ModF x y -> ((,) <$> x <*> y) >>= \case
          (RealOp lhs, RealOp rhs) -> do
            m <- frem lhs rhs
            cond <- fcmp P.OLT m (C.double 0.0)
            m' <- fadd m rhs
            RealOp <$> select cond m' m
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

      RoundF r -> r >>= \case
        (RealOp x) -> IntegerOp <$>
          (call (getExtern "trunc") [(x, [])] >>= (`fptosi` AST.i32))

      FloorF r -> r >>= \case
        (RealOp x) -> IntegerOp <$>
          (call (getExtern "floor") [(x, [])] >>= (`fptosi` AST.i32))

      CeilingF r -> r >>= \case
        (RealOp x) -> IntegerOp <$>
          (call (getExtern "ceil") [(x, [])] >>= (`fptosi` AST.i32))

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

      PowC z p -> go (ExpC (go (MulC p (go (LogC z)))))

      SqrtC z -> do
        half <- go (R2C (pure (RealOp (C.double 0.5))))
        go (z `PowC` pure half)

      TanC z -> z >>= \case
        ComplexOp x y -> do
          tanX <- call (getExtern "tan") [(x, [])]
          tanhY  <- call (getExtern "tanh")  [(y, [])]
          tanX_tanhY <- fmul tanX tanhY
          tanX2_tanhY <- fmul tanX tanX_tanhY
          tanX_tanhY2 <- fmul tanhY tanX_tanhY
          tanX2_tanhY2 <- fmul tanX tanX_tanhY2
          denominator <- fadd (C.double 1) tanX2_tanhY2
          realNumerator <- fsub tanX tanX_tanhY2
          imagNumerator <- fadd tanhY tanX2_tanhY
          ComplexOp <$> fdiv realNumerator denominator
                    <*> fdiv imagNumerator denominator

      SinhC z -> z >>= \case
        ComplexOp x y -> do
          sinhX <- call (getExtern "sinh") [(x, [])]
          cosY  <- call (getExtern "cos")  [(y, [])]
          coshX <- call (getExtern "cosh") [(x, [])]
          sinY  <- call (getExtern "sin")  [(y, [])]
          ComplexOp <$> fmul sinhX cosY <*> fmul coshX sinY

      CoshC z -> z >>= \case
        ComplexOp x y -> do
          coshX <- call (getExtern "cosh") [(x, [])]
          cosY  <- call (getExtern "cos")  [(y, [])]
          sinhX <- call (getExtern "sinh") [(x, [])]
          sinY  <- call (getExtern "sin")  [(y, [])]
          ComplexOp <$> fmul coshX cosY <*> fmul sinhX sinY

      TanhC z -> z >>= \case
        ComplexOp x y -> do
          tanhX <- call (getExtern "tanh") [(x, [])]
          tanY  <- call (getExtern "tan")  [(y, [])]
          tanhX_tanY <- fmul tanhX tanY
          tanhX2_tanY <- fmul tanhX tanhX_tanY
          tanhX_tanY2 <- fmul tanY tanhX_tanY
          tanhX2_tanY2 <- fmul tanhX tanhX_tanY2
          denominator <- fadd (C.double 1) tanhX2_tanY2
          realNumerator <- fadd tanhX tanhX_tanY2
          imagNumerator <- fsub tanY tanhX2_tanY
          ComplexOp <$> fdiv realNumerator denominator
                    <*> fdiv imagNumerator denominator


------------------------------------------------------------------------
-- List IR helpers
------------------------------------------------------------------------

-- | Allocate one node per element, link them, and return the head pointer.
-- Elements are pre-evaluated.  A literal has a known length, so we do a single
-- up-front bounds check: if the whole list will not fit, set the overflow flag
-- (pixel goes magenta) and return the empty list.  Otherwise every per-node
-- 'arenaAlloc' below is guaranteed to succeed, so no null can be written.
buildLitList :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
             => ArenaState -> TypeProxy t -> Int -> [Op t] -> m (Op ('ListT t))
buildLitList arena ty stride elems = mdo
  headSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store headSlot 0 nullI8Ptr

  -- Will all (length elems) nodes fit from the current bump position?
  bump    <- load (asBumpAlloca arena) 0
  needEnd  <- gep bump [C.int32 (fromIntegral (stride * length elems))]
  overflow <- icmp IP.UGT needEnd (asArenaEnd arena)   -- unsigned pointer compare
  condBr overflow litOverflow litBuild

  litOverflow <- block `named` "lit_overflow"
  store (asOverflowFlag arena) 0 (C.bit 1)
  br litDone

  litBuild <- block `named` "lit_build"
  nodePtrs <- mapM (const (arenaAlloc arena stride)) elems
  let nextPtrs = drop 1 nodePtrs ++ [nullI8Ptr]
  forM_ (zip3 nodePtrs nextPtrs elems) $ \(nodePtr, nextPtr, elemOp) -> do
    nf <- bitcast nodePtr (AST.ptr (AST.ptr AST.i8))
    store nf 0 nextPtr
    storeListElem ty nodePtr elemOp
  case nodePtrs of
    (h:_) -> store headSlot 0 h
    []    -> pure ()
  br litDone

  litDone <- block `named` "lit_done"
  ListOp <$> load headSlot 0

-- | Copy every node from every input-list head ptr into the arena, linking
-- the copies end-to-end.  All input lists are copied (no sharing).
buildJoin :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
          => ArenaState -> TypeProxy t -> [Operand] -> m (Op ('ListT t))
buildJoin arena ty inputHeads = do
  let stride = listNodeStride ty
  headSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store headSlot 0 nullI8Ptr
  prevSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store prevSlot 0 nullI8Ptr
  -- For each input list, copy its nodes and append to the result.
  forM_ inputHeads $ \inputHead -> do
    currSlot <- alloca (AST.ptr AST.i8) Nothing 0
    store currSlot 0 inputHead
    mdo
      br copyHead
      copyHead <- block `named` "join_copy_head"
      curr <- load currSlot 0
      done <- icmp P.EQ curr nullI8Ptr
      condBr done copyExit copyBody
      copyBody <- block `named` "join_copy_body"
      newNode <- arenaAlloc arena stride
      isNull <- icmp P.EQ newNode nullI8Ptr   -- arena overflow: bail with partial list
      condBr isNull copyExit copyAlloc
      copyAlloc <- block `named` "join_copy_alloc"
      -- Copy element from curr to newNode
      elemOp <- loadListElem ty curr
      nf <- bitcast newNode (AST.ptr (AST.ptr AST.i8))
      store nf 0 nullI8Ptr          -- new node's next = null
      storeListElem ty newNode elemOp
      -- Append to result
      appendNode headSlot prevSlot newNode
      -- Advance
      nextPtr <- loadListNext curr
      store currSlot 0 nextPtr
      br copyHead
      copyExit <- block `named` "join_copy_exit"
      pure ()
  ListOp <$> load headSlot 0

-- | Filter: keep elements where the predicate callback returns 0 (false).
-- The callback receives the element slot and returns an i1 Operand.
buildFilter :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
            => ArenaState -> TypeProxy t -> Operand -> PtrOp t
            -> (PtrOp t -> m Operand)   -- ^ predicate; keep when result = 0
            -> m (Op ('ListT t))
buildFilter arena ty inputHead elemSlot predCb = do
  let stride = listNodeStride ty
  headSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store headSlot 0 nullI8Ptr
  prevSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store prevSlot 0 nullI8Ptr
  currSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store currSlot 0 inputHead
  mdo
    br filterHead
    filterHead <- block `named` "filter_head"
    curr <- load currSlot 0
    done <- icmp P.EQ curr nullI8Ptr
    condBr done filterExit filterBody
    filterBody <- block `named` "filter_body"
    elemOp <- loadListElem ty curr
    storeOperand elemOp elemSlot
    shouldRemove <- predCb elemSlot
    condBr shouldRemove filterAdvance filterAdd
    filterAdd <- block `named` "filter_add"
    newNode <- arenaAlloc arena stride
    isNull <- icmp P.EQ newNode nullI8Ptr   -- arena overflow: bail with partial list
    condBr isNull filterExit filterStore
    filterStore <- block `named` "filter_store"
    nf <- bitcast newNode (AST.ptr (AST.ptr AST.i8))
    store nf 0 nullI8Ptr
    storeListElem ty newNode elemOp
    appendNode headSlot prevSlot newNode
    br filterAdvance
    filterAdvance <- block `named` "filter_advance"
    nextPtr <- loadListNext curr
    store currSlot 0 nextPtr
    br filterHead
    filterExit <- block `named` "filter_exit"
    pure ()
  ListOp <$> load headSlot 0

-- | Map: apply a callback to every element, building a new list with the results.
buildMap :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
         => ArenaState -> TypeProxy t1 -> TypeProxy t2 -> Operand -> PtrOp t1
         -> (PtrOp t1 -> m (Op t2))   -- ^ transform callback
         -> m (Op ('ListT t2))
buildMap arena ty1 ty2 inputHead elemSlot transformCb = do
  let stride2 = listNodeStride ty2
  headSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store headSlot 0 nullI8Ptr
  prevSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store prevSlot 0 nullI8Ptr
  currSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store currSlot 0 inputHead
  mdo
    br mapHead
    mapHead <- block `named` "map_head"
    curr <- load currSlot 0
    done <- icmp P.EQ curr nullI8Ptr
    condBr done mapExit mapBody
    mapBody <- block `named` "map_body"
    elemOp <- loadListElem ty1 curr
    storeOperand elemOp elemSlot
    outOp <- transformCb elemSlot
    newNode <- arenaAlloc arena stride2
    isNull <- icmp P.EQ newNode nullI8Ptr   -- arena overflow: bail with partial list
    condBr isNull mapExit mapStore
    mapStore <- block `named` "map_store"
    nf <- bitcast newNode (AST.ptr (AST.ptr AST.i8))
    store nf 0 nullI8Ptr
    storeListElem ty2 newNode outOp
    appendNode headSlot prevSlot newNode
    nextPtr <- loadListNext curr
    store currSlot 0 nextPtr
    br mapHead
    mapExit <- block `named` "map_exit"
    pure ()
  ListOp <$> load headSlot 0

-- | Find: return first element where the predicate callback returns 1 (true),
-- or the default value if no element matches.
buildFind :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
          => ArenaState -> TypeProxy t -> Operand -> PtrOp t -> Op t
          -> (PtrOp t -> m Operand)  -- ^ predicate callback
          -> m (Op t)
buildFind _arena ty inputHead elemSlot defOp predCb = do
  resultSlot <- allocaOp ty
  storeOperand defOp resultSlot
  currSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store currSlot 0 inputHead
  mdo
    br findHead
    findHead <- block `named` "find_head"
    curr <- load currSlot 0
    done <- icmp P.EQ curr nullI8Ptr
    condBr done findExit findBody
    findBody <- block `named` "find_body"
    elemOp <- loadListElem ty curr
    storeOperand elemOp elemSlot
    matched <- predCb elemSlot
    condBr matched findFound findAdvance
    findFound <- block `named` "find_found"
    storeOperand elemOp resultSlot
    br findExit
    findAdvance <- block `named` "find_advance"
    nextPtr <- loadListNext curr
    store currSlot 0 nextPtr
    br findHead
    findExit <- block `named` "find_exit"
    pure ()
  derefOperand resultSlot

-- | Count the number of nodes in a linked list.
buildLength :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
            => Operand     -- ^ head ptr
            -> AST.Type    -- ^ element LLVM type (unused; for sizing only)
            -> m (Op 'IntegerT)
buildLength inputHead _elemTy = do
  countSlot <- alloca AST.i32 Nothing 0
  store countSlot 0 (C.int32 0)
  currSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store currSlot 0 inputHead
  mdo
    br lenHead
    lenHead <- block `named` "length_head"
    curr <- load currSlot 0
    done <- icmp P.EQ curr nullI8Ptr
    condBr done lenExit lenBody
    lenBody <- block `named` "length_body"
    n <- load countSlot 0
    n' <- add n (C.int32 1)
    store countSlot 0 n'
    nextPtr <- loadListNext curr
    store currSlot 0 nextPtr
    br lenHead
    lenExit <- block `named` "length_exit"
    pure ()
  IntegerOp <$> load countSlot 0

-- | 1-based list indexing.  For cyclic = False: positive indices count from
-- the front, negative from the back.  For cyclic = True: wraps around.
-- Out-of-bounds returns a zero-initialised element (undefined behaviour by spec).
buildIndex :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
           => ArenaState -> TypeProxy t -> Bool -> Operand -> Operand
           -> m (Op t)
buildIndex _arena ty cyclic inputHead iOp = do
  -- Allocate all slots up front (keeps allocas in the entry region).
  stepSlot   <- alloca AST.i32 Nothing 0
  currSlot   <- alloca (AST.ptr AST.i8) Nothing 0
  resultSlot <- allocaOp ty
  store currSlot 0 inputHead
  -- Compute 0-based step count.  We always compute the length because we need
  -- it for both cyclic wrapping and negative non-cyclic indices.
  len <- getIntegerOp <$> buildLength inputHead (toLLVMType ty)
  if cyclic
    then do
      -- step = ((i - 1) mod len + len) mod len   (handles any sign)
      i1   <- sub iOp (C.int32 1)
      r    <- srem i1 len
      r'   <- add r len
      step <- srem r' len
      store stepSlot 0 step
    else do
      -- i >= 1: step = i - 1 (0-based from front)
      -- i <= 0 (incl. negative): step = len + i  (0-based from back)
      isNeg   <- icmp P.SLE iOp (C.int32 0)
      posStep <- sub iOp (C.int32 1)
      negStep <- add len iOp
      finalStep <- select isNeg negStep posStep
      store stepSlot 0 finalStep
  -- Walk 'step' steps from the head.
  mdo
    br stepHead
    stepHead <- block `named` "index_step"
    step <- load stepSlot 0
    curr <- load currSlot 0
    atTarget <- icmp P.EQ step (C.int32 0)
    isNull   <- icmp P.EQ curr nullI8Ptr
    atEnd    <- I.or atTarget isNull
    condBr atEnd stepDone stepAdvance
    stepAdvance <- block `named` "index_advance"
    step' <- sub step (C.int32 1)
    store stepSlot 0 step'
    nextPtr <- loadListNext curr
    store currSlot 0 nextPtr
    br stepHead
    stepDone <- block `named` "index_done"
    pure ()
  -- Load element or return zero on out-of-bounds.
  curr     <- load currSlot 0
  isNull   <- icmp P.EQ curr nullI8Ptr
  mdo
    condBr isNull oobBb elemBb
    oobBb <- block `named` "index_oob"
    br mergeBb                           -- resultSlot stays zero-initialised
    elemBb <- block `named` "index_elem"
    curr2 <- load currSlot 0
    e <- loadListElem ty curr2
    storeOperand e resultSlot
    br mergeBb
    mergeBb <- block `named` "index_merge"
    pure ()
  derefOperand resultSlot

-- | Build an integer range list [lo..hi] in the arena.
buildRange :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
           => ArenaState -> Operand -> Operand -> m (Op ('ListT 'IntegerT))
buildRange arena loOp hiOp = do
  let stride = listNodeStride IntegerType
  headSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store headSlot 0 nullI8Ptr
  prevSlot <- alloca (AST.ptr AST.i8) Nothing 0
  store prevSlot 0 nullI8Ptr
  iSlot <- alloca AST.i32 Nothing 0
  store iSlot 0 loOp
  mdo
    br rangeHead
    rangeHead <- block `named` "range_head"
    i <- load iSlot 0
    past <- icmp P.SGT i hiOp
    condBr past rangeExit rangeBody
    rangeBody <- block `named` "range_body"
    newNode <- arenaAlloc arena stride
    isNull <- icmp P.EQ newNode nullI8Ptr   -- arena overflow: bail with partial list
    condBr isNull rangeExit rangeAlloc
    rangeAlloc <- block `named` "range_alloc"
    nf <- bitcast newNode (AST.ptr (AST.ptr AST.i8))
    store nf 0 nullI8Ptr
    storeListElem IntegerType newNode (IntegerOp i)
    appendNode headSlot prevSlot newNode
    i' <- add i (C.int32 1)
    store iSlot 0 i'
    br rangeHead
    rangeExit <- block `named` "range_exit"
    pure ()
  ListOp <$> load headSlot 0

-- | Append a new node to the result list tracked by (headSlot, prevSlot).
-- headSlot holds the head ptr (null = list is empty so far).
-- prevSlot holds the previous node ptr (null = no previous node yet).
appendNode :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
           => Operand   -- ^ headSlot (i8**, alloca)
           -> Operand   -- ^ prevSlot (i8**, alloca)
           -> Operand   -- ^ newNode  (i8*)
           -> m ()
appendNode headSlot prevSlot newNode = do
  prev <- load prevSlot 0
  mdo
    isFirst <- icmp P.EQ prev nullI8Ptr
    condBr isFirst firstBb linkBb
    firstBb <- block `named` "append_first"
    store headSlot 0 newNode
    br mergeBb
    linkBb <- block `named` "append_link"
    prevNextField <- bitcast prev (AST.ptr (AST.ptr AST.i8))
    store prevNextField 0 newNode
    br mergeBb
    mergeBb <- block `named` "append_merge"
    pure ()
  store prevSlot 0 newNode

getGetExtern :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => m (String -> Operand)
getGetExtern = do
  es <- mapM (\(name, getter) -> (name,) <$> getter) $
    [ ("absi", extern "llvm.abs.i32" [AST.i32, AST.i1] AST.i32)
    , ("powi", extern "llvm.powi.i32" [AST.i32, AST.i32] AST.i32)
    , ("trunc", extern "llvm.trunc.f64" [AST.double] AST.double)
    , ("floor", extern "llvm.floor.f64" [AST.double] AST.double)
    , ("ceil", extern "llvm.ceil.f64" [AST.double] AST.double)
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
