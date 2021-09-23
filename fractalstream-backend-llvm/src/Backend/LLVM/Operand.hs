module Backend.LLVM.Operand
  ( toLLVMType
  , Operand_
  , Op_
  , Op(..)
  , getBooleanOp
  , detypeOperand
  , derefOperand
  , storeOperand
  ) where

import Language.Type
import Language.Value

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Float as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.AST.Operand hiding (local)

import Control.Monad.Except
import Fcf (Exp, Eval)

import Debug.Trace

data Operand_ :: Symbol -> Type -> Exp *
type instance Eval (Operand_ name t) = Operand

data Op_ :: (Environment, Type) -> Exp *
type instance Eval (Op_ et) = Op (Ty et)

data Op (t :: Type) where
  VoidOp    :: Op 'VoidT
  BooleanOp :: Operand -> Op 'BooleanT
  IntegerOp :: Operand -> Op 'IntegerT
  RealOp    :: Operand -> Op 'RealT
  ComplexOp :: Operand -> Operand -> Op 'ComplexT
  ColorOp   :: Operand -> Operand -> Operand -> Op 'ColorT
  PairOp    :: forall t1 t2. Op t1 -> Op t2 -> Op ('Pair t1 t2)

getBooleanOp :: Op 'BooleanT -> Operand
getBooleanOp (BooleanOp x) = x

storeOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => Op t -> Operand -> m ()
storeOperand op ptr = case op of
  VoidOp -> pure ()
  BooleanOp v -> store ptr 0 v
  IntegerOp v -> store ptr 0 v
  RealOp v -> store ptr 0 v
  ComplexOp x y -> do
    let zero = AST.Float (AST.Double 0.0)
    traceM ("*** ptr = " ++ show ptr)
    traceM ("*** x   = " ++ show x)
    traceM ("*** y   = " ++ show y)
    z   <- array [zero,zero]
    z'  <- insertValue z  x [0]
    z'' <- insertValue z' y [1]
    traceM ("*** ready to store")
    store ptr 0 z''
  ColorOp r g b -> do
    store ptr 0 r
    store ptr 1 g
    store ptr 2 b
  _ -> error "TODO: Unhandled store type"

detypeOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
              => ScalarProxy t
              -> Op t
              -> m Operand
detypeOperand _t = \case
  VoidOp       -> throwError "cannot de-type void"
  BooleanOp op -> pure op
  IntegerOp op -> pure op
  RealOp op    -> pure op
  ComplexOp x y -> do
    let zero = AST.Float (AST.Double 0.0)
    z0 <- array [zero,zero]
    z1 <- insertValue z0 x [0]
    insertValue z1 y [1]
  ColorOp r g b -> do
    let zero = AST.Int 8 0
    c0 <- array [zero,zero,zero]
    c1 <- insertValue c0 r [0]
    c2 <- insertValue c1 g [1]
    insertValue c2 b [2]
  PairOp _op1 _op2 -> error "TODO: detypeOperand PairOp"


derefOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => ScalarProxy t -> Operand -> m (Op t)
derefOperand t ptr = case t of
  VoidProxy    -> throwError "internal error: cannot dereference void pointer"
  BooleanProxy -> BooleanOp <$> load ptr 0
  IntegerProxy -> IntegerOp <$> load ptr 0
  RealProxy    -> RealOp <$> load ptr 0
  ComplexProxy -> do
    arr <- load ptr 0
    ComplexOp <$> extractValue arr[0]
              <*> extractValue arr[1]
  ColorProxy   -> do
    arr <- load ptr 0
    ColorOp <$> extractValue arr[0]
            <*> extractValue arr[1]
            <*> extractValue arr[2]
  PairProxy t1 t2 -> do
    p <- load ptr 0
    x <- extractValue p[0]
    y <- extractValue p[1]
    PairOp <$> derefOperand t1 x <*> derefOperand t2 y
  _ -> error "TODO: unhandled type in derefOperand"


toLLVMType :: forall t. ScalarProxy t -> AST.Type
toLLVMType = \case
  VoidProxy       -> AST.void
  BooleanProxy    -> AST.i1
  IntegerProxy    -> AST.i32
  RealProxy       -> AST.double
  ComplexProxy    -> AST.ArrayType 2 AST.double
  RationalProxy   -> AST.ArrayType 2 AST.i32
  PairProxy t1 t2 -> AST.StructureType False [ toLLVMType t1
                                             , toLLVMType t2 ]
  ColorProxy      -> AST.ArrayType 3 AST.i8
  ImageProxy      -> AST.i32
