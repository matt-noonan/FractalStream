{-# language RecursiveDo #-}
module Backend.LLVM.Operand
  ( toLLVMType
  , toLLVMPtrType
  , OperandPtr
  , Op_
  , Op(..)
  , PtrOp_
  , PtrOp(..)
  , getBooleanOp
  , getListOp
  , getIntegerOp
  , typedOperand
  , typedOperandPtr
  , detypeOperand
  , derefOperand
  , storeOperand
  , allocaOp
  , nullI8Ptr
  , listNodeStride
  , loadListNext
  , loadListElem
  , storeListElem
  , ArenaState(..)
  , arenaAlloc
  -- * Re-exports
  , Operand
  ) where

import FractalStream.Prelude

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
import qualified LLVM.AST.IntegerPredicate as P
import Control.Monad.Fix

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
  -- | A list is represented as a pointer to the head node (null = empty).
  -- Node layout (contiguous in memory):
  --   bytes 0-7:  next pointer (i8*, null = end of list)
  --   bytes 8+:   element data (type-dependent, see listNodeStride)
  ListOp    :: forall t. Operand -> Op ('ListT t)
  TextOp    :: Op 'TextT

deriving instance (Show (Op t))

newtype PtrOp t = PtrOp (Op t)
  deriving Show

getBooleanOp :: Op 'BooleanT -> Operand
getBooleanOp (BooleanOp x) = x

getListOp :: Op ('ListT t) -> Operand
getListOp (ListOp headPtr) = headPtr

getIntegerOp :: Op 'IntegerT -> Operand
getIntegerOp (IntegerOp x) = x

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
  (ListOp headPtr, ListOp ptrSlot) -> store ptrSlot 0 headPtr
  (TextOp, TextOp) -> pure ()
  _ -> throwError "TODO: Unhandled store type"

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
  PairOp _op1 _op2 -> throwError "TODO: detypeOperand PairOp"
  ListOp headPtr -> pure headPtr
  TextOp -> throwError "TODO: detypeOperand TextOp"

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
  ListOp ptrSlot -> ListOp <$> load ptrSlot 0
  TextOp -> throwError "TODO: derefOperand TextOp"

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
  -- A list is passed as an i8* pointing to the head node (null = empty list).
  ListType {}    -> AST.ptr AST.i8
  TextType       -> AST.i32 -- fixme

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
  -- A list slot holds a single i8* (the head pointer).
  ListType {}    -> AST.ptr (AST.ptr AST.i8)
  TextType       -> AST.ptr AST.i32

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

  ListType _ -> do
    -- Allocate a stack slot that holds an i8* (the head pointer).
    ptrSlot <- alloca (AST.ptr AST.i8) Nothing 0
    store ptrSlot 0 nullI8Ptr
    pure (PtrOp (ListOp ptrSlot))

  TextType -> pure (PtrOp TextOp)

  ty -> throwError ("Unhandled type in LLVM backend: " ++ showType ty)

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
           TextType -> pure (PtrOp TextOp)
           _ -> throwError "TODO: typedOperandPtr"

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

           -- The argument IS the head pointer; wrap it directly.
           ListType _ -> pure (ListOp op)
           TextType -> pure TextOp
           _ -> throwError ("TODO: missing case in typedOperand for type " ++ showType t)

-- | A null i8* constant (used as the empty-list sentinel).
nullI8Ptr :: Operand
nullI8Ptr = ConstantOperand (AST.Null (AST.ptr AST.i8))

-- | Byte stride between consecutive nodes in a serialized list buffer.
-- Layout: 8 bytes (next i8* pointer) + element data, rounded up to 8-byte alignment.
listNodeStride :: TypeProxy t -> Int
listNodeStride t = roundUp8 (8 + elemBytes t)
  where
    roundUp8 n  = ((n + 7) `div` 8) * 8
    elemBytes BooleanType  = 1
    elemBytes IntegerType  = 4
    elemBytes RealType     = 8
    elemBytes ComplexType  = 16  -- two doubles
    elemBytes ColorType    = 3
    elemBytes (ListType _) = 8   -- nested list: a head pointer
    elemBytes _            = 8   -- fallback for unsupported types

-- | Emit LLVM IR to load the 'next' pointer from a list node.
-- The node pointer is an i8*; the next field lives at byte offset 0.
loadListNext :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => Operand   -- ^ i8* pointing to the current node
             -> m Operand -- ^ i8* pointing to the next node (or null)
loadListNext nodePtr = do
  -- Cast the node ptr to i8** so we can load a pointer from it.
  nextPtrPtr <- bitcast nodePtr (AST.ptr (AST.ptr AST.i8))
  load nextPtrPtr 0

-- | Emit LLVM IR to load the element value from a list node.
-- The element lives at byte offset 8 (just after the 8-byte next pointer).
loadListElem :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
             => TypeProxy t
             -> Operand     -- ^ i8* pointing to the current node
             -> m (Op t)
loadListElem t nodePtr = do
  elemI8 <- gep nodePtr [int32 8]
  case t of
    BooleanType -> do
      p <- bitcast elemI8 (AST.ptr AST.i1)
      BooleanOp <$> load p 0
    IntegerType -> do
      p <- bitcast elemI8 (AST.ptr AST.i32)
      IntegerOp <$> load p 0
    RealType -> do
      p <- bitcast elemI8 (AST.ptr AST.double)
      RealOp <$> load p 0
    ComplexType -> do
      p <- bitcast elemI8 (AST.ptr (AST.ArrayType 2 AST.double))
      xPtr <- gep p [int32 0, int32 0]
      yPtr <- gep p [int32 0, int32 1]
      ComplexOp <$> load xPtr 0 <*> load yPtr 0
    ColorType -> do
      p <- bitcast elemI8 (AST.ptr (AST.ArrayType 3 AST.i8))
      rPtr <- gep p [int32 0, int32 0]
      gPtr <- gep p [int32 0, int32 1]
      bPtr <- gep p [int32 0, int32 2]
      ColorOp <$> load rPtr 0 <*> load gPtr 0 <*> load bPtr 0
    ListType _ -> do
      p <- bitcast elemI8 (AST.ptr (AST.ptr AST.i8))
      ListOp <$> load p 0
    _ -> throwError ("loadListElem: unsupported element type " ++ showType t)

-- | Write element data into a list node at the given byte offset (offset 8 past
-- the start of the node).  Mirror image of 'loadListElem'.
storeListElem :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
              => TypeProxy t
              -> Operand   -- ^ i8* base of the node
              -> Op t
              -> m ()
storeListElem t nodePtr op = do
  elemI8 <- gep nodePtr [int32 8]
  case (t, op) of
    (BooleanType, BooleanOp v) -> do
      p <- bitcast elemI8 (AST.ptr AST.i1)
      store p 0 v
    (IntegerType, IntegerOp v) -> do
      p <- bitcast elemI8 (AST.ptr AST.i32)
      store p 0 v
    (RealType, RealOp v) -> do
      p <- bitcast elemI8 (AST.ptr AST.double)
      store p 0 v
    (ComplexType, ComplexOp x y) -> do
      p <- bitcast elemI8 (AST.ptr (AST.ArrayType 2 AST.double))
      xPtr <- gep p [int32 0, int32 0]
      yPtr <- gep p [int32 0, int32 1]
      store xPtr 0 x
      store yPtr 0 y
    (ColorType, ColorOp r g b) -> do
      p <- bitcast elemI8 (AST.ptr (AST.ArrayType 3 AST.i8))
      rPtr <- gep p [int32 0, int32 0]
      gPtr <- gep p [int32 0, int32 1]
      bPtr <- gep p [int32 0, int32 2]
      store rPtr 0 r
      store gPtr 0 g
      store bPtr 0 b
    (ListType _, ListOp headPtr) -> do
      p <- bitcast elemI8 (AST.ptr (AST.ptr AST.i8))
      store p 0 headPtr
    _ -> throwError ("storeListElem: unsupported type " ++ showType t)

-- | Arena state threaded through LLVM IR generation for dynamic list allocation.
-- The arena is a flat byte buffer; a bump pointer is advanced on each allocation
-- and reset to the base at the start of each pixel/subsample computation.
-- asOverflowFlag is an i1* stack slot set to 1 on the first failed allocation;
-- checked after compileCode to render the pixel magenta.
data ArenaState = ArenaState
  { asBumpAlloca   :: Operand  -- ^ i8** stack slot holding the current bump pointer
  , asArenaEnd     :: Operand  -- ^ i8* constant end of the arena (base + capacity)
  , asOverflowFlag :: Operand  -- ^ i1* stack slot; set to 1 on overflow
  }

-- | Emit inline bump-allocation of 'size' bytes (must be a multiple of 8).
-- Returns the allocated i8* on success; returns null on overflow and sets the
-- overflow flag in ArenaState. Callers MUST null-check the result and bail out
-- of list construction on overflow (see the builders in Backend.LLVM.Value).
arenaAlloc :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
           => ArenaState
           -> Int        -- ^ compile-time byte count (multiple of 8)
           -> m Operand
arenaAlloc ArenaState{..} size = mdo
  bump    <- load asBumpAlloca 0
  newBump <- gep bump [int32 (fromIntegral size)]
  overflow <- icmp P.UGT newBump asArenaEnd
  condBr overflow overflowBb okBb

  okBb <- block
  store asBumpAlloca 0 newBump
  br mergeBb

  overflowBb <- block
  store asOverflowFlag 0 (bit 1)  -- signal overflow
  br mergeBb

  mergeBb <- block
  phi [(bump, okBb), (nullI8Ptr, overflowBb)]
