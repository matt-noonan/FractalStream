module Backend.LLVM.Operand
  ( toLLVMType
  , toLLVMPtrType
  , OperandPtr
  , Op_
  , Op(..)
  , PtrOp_
  , PtrOp(..)
  , getBooleanOp
  , typedOperand
  , typedOperandPtr
  , detypeOperand
  , derefOperand
  , storeOperand
  , allocaOp
  -- * Re-exports
  , Operand
  ) where

import Language.Type
import Language.Value

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Typed as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Float as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.AST.Operand hiding (local)

import Control.Monad.Except
import Fcf (Exp, Eval)

data OperandPtr :: Symbol -> FSType -> Exp *
type instance Eval (OperandPtr name t) = PtrOp t

data PtrOp_ :: (Environment, FSType) -> Exp *
type instance Eval (PtrOp_ et) = PtrOp (Ty et)

data Op_ :: (Environment, FSType) -> Exp *
type instance Eval (Op_ et) = Op (Ty et)

data Op (t :: FSType) where
  VoidOp    :: Op 'VoidT
  BooleanOp :: Operand -> Op 'BooleanT
  IntegerOp :: Operand -> Op 'IntegerT
  RealOp    :: Operand -> Op 'RealT
  ComplexOp :: Operand -> Operand -> Op 'ComplexT
  ColorOp   :: Operand -> Operand -> Operand -> Op 'ColorT
  PairOp    :: forall t1 t2. Op t1 -> Op t2 -> Op ('Pair t1 t2)

deriving instance (Show (Op t))

newtype PtrOp t = PtrOp (Op t)
  deriving Show

getBooleanOp :: Op 'BooleanT -> Operand
getBooleanOp (BooleanOp x) = x

storeOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => Op t -> PtrOp t -> m ()
storeOperand op (PtrOp ptrOp) = case (op, ptrOp) of
  (VoidOp, VoidOp) -> pure ()
  (BooleanOp v, BooleanOp ptr) -> store ptr 0 v
  (IntegerOp v, IntegerOp ptr) -> store ptr 0 v
  (RealOp v, RealOp ptr) -> store ptr 0 v
  (ComplexOp x y, ComplexOp ptrX ptrY) -> do
    store ptrX 0 x
    store ptrY 0 y
  (ColorOp r g b, ColorOp ptrR ptrG ptrB) -> do
    store ptrR 0 r
    store ptrG 0 g
    store ptrB 0 b
  _ -> error "TODO: Unhandled store type"

detypeOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
              => TypeProxy t
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
             => PtrOp t -> m (Op t)
derefOperand (PtrOp ptrOp) = case ptrOp of
  VoidOp    -> throwError "internal error: cannot dereference void pointer"
  BooleanOp ptr -> BooleanOp <$> load ptr 0
  IntegerOp ptr -> IntegerOp <$> load ptr 0
  RealOp ptr    -> RealOp <$> load ptr 0
  ComplexOp ptrX ptrY -> do
    ComplexOp <$> load ptrX 0
              <*> load ptrY 0
  ColorOp ptrR ptrG ptrB -> do
    ColorOp <$> load ptrR 0
            <*> load ptrG 0
            <*> load ptrB 0
  PairOp t1 t2 ->
    PairOp <$> derefOperand (PtrOp t1) <*> derefOperand (PtrOp t2)

-- | Get the LLVM function argument type corresponding to
-- a FractalStream type.
toLLVMType :: forall t. TypeProxy t -> AST.Type
toLLVMType = \case
  VoidType       -> AST.void
  BooleanType    -> AST.i1
  IntegerType    -> AST.i32
  RealType       -> AST.double
  ComplexType    -> AST.ptr (AST.ArrayType 2 AST.double)
  RationalType   -> AST.ptr (AST.ArrayType 2 AST.i32)
  PairType t1 t2 -> AST.ptr (AST.StructureType False [ toLLVMType t1
                                                      , toLLVMType t2 ])
  ColorType      -> AST.ptr (AST.ArrayType 3 AST.i8)
  ImageType      -> AST.i32
  ListType {}    -> AST.i32

toLLVMPtrType :: forall t. TypeProxy t -> AST.Type
toLLVMPtrType = \case
  VoidType       -> AST.ptr AST.void
  BooleanType    -> AST.ptr AST.i1
  IntegerType    -> AST.ptr AST.i32
  RealType       -> AST.ptr AST.double
  ComplexType    -> AST.ptr (AST.ArrayType 2 AST.double)
  RationalType   -> AST.ptr (AST.ArrayType 2 AST.i32)
  PairType t1 t2 -> AST.ptr (AST.StructureType False [ toLLVMType t1
                                                      , toLLVMType t2 ])
  ColorType      -> AST.ptr (AST.ArrayType 3 AST.i8)
  ImageType      -> AST.ptr AST.i32
  ListType {}    -> AST.ptr AST.i32

allocaOp :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
         => TypeProxy t
         -> m (PtrOp t)
allocaOp = \case
  VoidType -> pure (PtrOp VoidOp)
  BooleanType -> PtrOp . BooleanOp <$> alloca AST.i1  Nothing 0
  IntegerType -> PtrOp . IntegerOp <$> alloca AST.i32 Nothing 0
  RealType    -> PtrOp . RealOp    <$> alloca AST.double Nothing 0
  ComplexType -> PtrOp <$> (ComplexOp <$> alloca AST.double Nothing 0
                                       <*> alloca AST.double Nothing 0)
  ColorType   -> PtrOp <$> (ColorOp <$> alloca AST.i8 Nothing 0
                                     <*> alloca AST.i8 Nothing 0
                                     <*> alloca AST.i8 Nothing 0)
  PairType t1 t2 -> do
    PtrOp ptr1 <- allocaOp t1
    PtrOp ptr2 <- allocaOp t2
    pure (PtrOp (PairOp ptr1 ptr2))

  _ -> throwError "TODO: unhandled type in allocaOp"

typedOperandPtr :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
                => TypeProxy t
                -> Operand
                -> m (PtrOp t)
typedOperandPtr t op = do
  opTy <- AST.typeOf op >>= either throwError pure
  if opTy /= toLLVMPtrType t
    then throwError "internal error: mismatched type in typedOperandPtr"
    else case t of
           VoidType    -> pure (PtrOp VoidOp)
           BooleanType -> pure (PtrOp (BooleanOp op))
           IntegerType -> pure (PtrOp (IntegerOp op))
           RealType    -> pure (PtrOp (RealOp op))
           ComplexType ->
             PtrOp <$> (ComplexOp <$> gep op[int32 0, int32 0]
                                  <*> gep op[int32 0, int32 1])
           ColorType   ->
             PtrOp <$> (ColorOp <$> gep op[int32 0, int32 0]
                                <*> gep op[int32 0, int32 1]
                                <*> gep op[int32 0, int32 2])
           _ -> error "TODO: typedOperandPtr"

typedOperand :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => TypeProxy t
             -> Operand
             -> m (Op t)
typedOperand t op = do
  opTy <- AST.typeOf op >>= either throwError pure
  if opTy /= toLLVMType t
    then throwError "internal error: mismatched type in typedOperand"
    else case t of
           VoidType    -> pure VoidOp
           BooleanType -> pure (BooleanOp op)
           IntegerType -> pure (IntegerOp op)
           RealType    -> pure (RealOp op)
           ComplexType -> do
             z <- load op 0
             ComplexOp <$> extractValue z[0]
                       <*> extractValue z[1]
           ColorType -> do
             c <- load op 0
             ColorOp <$> extractValue c[0]
                     <*> extractValue c[1]
                     <*> extractValue c[2]
           PairType t1 t2 -> do
             p <- load op 0
             x1 <- extractValue p[0]
             x2 <- extractValue p[1]
             PairOp <$> typedOperand t1 x1
                    <*> typedOperand t2 x2
           _ -> error "TODO: missing case in typedOperand"
