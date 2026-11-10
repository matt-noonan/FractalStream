{-# language RecursiveDo, OverloadedStrings #-}
-- | Tables in compiled code.
--
-- Compiled code sees a table as a pointer to a /descriptor/, which the host
-- allocates and fills in from a 'Table':
--
-- > offset  0: i8*    the elements (see "Language.Table" for their layout)
-- > offset  8: i32    the number of dimensions, n
-- > offset 12: i32    frozen: nonzero if others may be looking at the table
-- > offset 16: double origin x, the center of cell (1, 1)
-- > offset 24: double origin y
-- > offset 32: double a cell's width
-- > offset 40: double a cell's height (rows go down)
-- > offset 48: i32[n] the size along each dimension
--
-- Every descriptor is in a registry, mapping it back to its 'Table'; that
-- keeps the table's buffer alive while compiled code may read it, and lets
-- a tool's write-back find the table a new descriptor stands for. A tool
-- allocates or copies tables by calling back into the host
-- ('withTableCallbacks'): compiled code never frees or makes a buffer.
module Backend.LLVM.Table
  ( -- * Host side
    newDescriptor
  , freeDescriptor
  , lookupDescriptor
  , withTableCallbacks
    -- * Code generation
  , buildTableRead
  , buildTablePoint
  , buildTableSize
  , buildTableContains
  , buildTableCell
  , buildTableCellPoint
  , buildTableWrite
  , buildTableAllocate
  , freezeDescriptor
  , tableNewFnType
  , tableThawFnType
  ) where

import FractalStream.Prelude

import Backend.LLVM.Operand
import Language.Type
import Language.Table hiding (pointIndices)

import qualified Data.Map as Map
import Data.IORef
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (bracket, finally)

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as AST
import Control.Monad.Fix

------------------------------------------------------------------------
-- Host side

-- | Every live descriptor, and the table it describes.
registry :: IORef (Map WordPtr Table)
registry = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE registry #-}

-- | Make a descriptor for a table, frozen or not.
newDescriptor :: Table -> Bool -> IO (Ptr Word8)
newDescriptor t frozen = do
  let dims = tableDims t
      (ox, oy) = tableOrigin t
      (sx, sy) = tableStep t
  d <- mallocBytes (48 + 4 * max 1 (length dims))
  pokeByteOff d 0 (unsafeForeignPtrToPtr (tableBuffer t))
  pokeByteOff d 8 (fromIntegral (length dims) :: Int32)
  pokeByteOff d 12 (if frozen then 1 else 0 :: Int32)
  pokeByteOff d 16 ox
  pokeByteOff d 24 oy
  pokeByteOff d 32 sx
  pokeByteOff d 40 sy
  forM_ (zip [0 ..] dims) $ \(k, n) -> pokeByteOff d (48 + 4 * k) (fromIntegral n :: Int32)
  atomicModifyIORef' registry (\m -> (Map.insert (ptrToWordPtr d) t m, ()))
  pure d

-- | Free a descriptor (its table lives on as long as anything else
-- refers to it).
freeDescriptor :: Ptr Word8 -> IO ()
freeDescriptor d = do
  mt <- atomicModifyIORef' registry (\m -> (Map.delete (ptrToWordPtr d) m, Map.lookup (ptrToWordPtr d) m))
  forM_ mt (touchForeignPtr . tableBuffer)
  free d

-- | The table a live descriptor describes.
lookupDescriptor :: Ptr Word8 -> IO (Maybe Table)
lookupDescriptor d = Map.lookup (ptrToWordPtr d) <$> readIORef registry

type TableNew = Int32 -> Int32 -> Ptr Int32 -> Double -> Double -> Double -> Double -> IO (Ptr Word8)
type TableThaw = Int32 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall "wrapper"
  wrapTableNew :: TableNew -> IO (FunPtr TableNew)
foreign import ccall "wrapper"
  wrapTableThaw :: TableThaw -> IO (FunPtr TableThaw)

-- | Run a tool handler with the host's table callbacks:
--
-- * @new(element size, n, sizes, x1, y1, x2, y2)@: a descriptor for a new
--   table, filled with zeros, covering the rectangle from (x1, y1) (top
--   left) to (x2, y2); null if a size is negative.
-- * @thaw(element size, descriptor)@: a descriptor for a copy of the table.
--
-- The descriptors they make are freed when the action finishes, so the
-- action must do its write-back first.
withTableCallbacks :: (FunPtr TableNew -> FunPtr TableThaw -> IO a) -> IO a
withTableCallbacks action = do
  made <- newIORef []
  let remember d = modifyIORef' made (d :) >> pure d
      new size n dimsP x1 y1 x2 y2 = do
        dims <- map fromIntegral <$> peekArray (fromIntegral n) dimsP
        if any (< 0) dims
          then pure nullPtr
          else do
            t <- allocateTableBytes (fromIntegral size) dims (x1 :+ y1) (x2 :+ y2)
            remember =<< newDescriptor t False
      thaw size d = lookupDescriptor d >>= \case
        Nothing -> pure nullPtr
        Just t -> do
          t' <- copyTableBytes (fromIntegral size) t
          remember =<< newDescriptor t' False
  bracket (wrapTableNew new) freeHaskellFunPtr $ \newFP ->
    bracket (wrapTableThaw thaw) freeHaskellFunPtr $ \thawFP ->
      action newFP thawFP `finally` (readIORef made >>= mapM_ freeDescriptor)

------------------------------------------------------------------------
-- Code generation

type Build m = (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)

-- | The host's @new@ and @thaw@ callbacks (see 'withTableCallbacks').
tableNewFnType, tableThawFnType :: AST.Type
tableNewFnType = AST.ptr (AST.FunctionType (AST.ptr AST.i8)
  [AST.i32, AST.i32, AST.ptr AST.i32, AST.double, AST.double, AST.double, AST.double] False)
tableThawFnType = AST.ptr (AST.FunctionType (AST.ptr AST.i8) [AST.i32, AST.ptr AST.i8] False)

field :: Build m => Operand -> Integer -> AST.Type -> m Operand
field desc offset ty = do
  p <- gep desc [C.int32 offset]
  bitcast p (AST.ptr ty)

loadField :: Build m => Operand -> Integer -> AST.Type -> m Operand
loadField desc offset ty = field desc offset ty >>= \p -> load p 0

elemBytes :: Build m => TypeProxy t -> m Integer
elemBytes ty = maybe (throwError ("A table can't hold " ++ showType ty)) (pure . fromIntegral) (elementSize ty)

-- | Whether some 1-based indices are in range, and where their cell's
-- element is (only meaningful if they are).
tableCell :: Build m => TypeProxy t -> Operand -> [Operand] -> m (Operand, Operand)
tableCell ty desc ixs = do
  size <- elemBytes ty
  let step (inRange, offset, stride) (k, i) = do
        dim <- loadField desc (48 + 4 * k) AST.i32
        atLeast1 <- icmp IP.SGE i (C.int32 1)
        atMost <- icmp IP.SLE i dim
        inRange' <- I.and inRange =<< I.and atLeast1 atMost
        term <- sub i (C.int32 1) >>= (`mul` stride)
        offset' <- add offset term
        stride' <- mul stride dim
        pure (inRange', offset', stride')
  (inRange, offset, _) <- foldM step (C.bit 1, C.int32 0, C.int32 1) (zip [0 ..] ixs)
  elems <- loadField desc 0 (AST.ptr AST.i8)
  byteOffset <- sext offset AST.i64 >>= (`mul` C.int64 size)
  p <- gep elems [byteOffset]
  pure (inRange, p)

-- | Read a cell; out of range sets the arena's index-error flag.
buildTableRead :: Build m => ArenaState -> TypeProxy t -> Operand -> [Operand] -> m (Op t)
buildTableRead arena ty desc ixs = do
  slot <- allocaOp ty
  (inRange, p) <- tableCell ty desc ixs
  mdo
    condBr inRange okB badB
    okB <- block `named` "table_read"
    loadElemAt ty p >>= (`storeOperand` slot)
    br doneB
    badB <- block `named` "table_read_out_of_range"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "table_read_done"
    pure ()
  derefOperand slot

-- | The indices of the cell nearest a point.
pointIndices :: Build m => (String -> Operand) -> Operand -> Operand -> Operand -> m [Operand]
pointIndices getExtern desc x y = do
  ox <- loadField desc 16 AST.double
  oy <- loadField desc 24 AST.double
  sx <- loadField desc 32 AST.double
  sy <- loadField desc 40 AST.double
  let nearest u = do  -- 1 + floor(u + 1/2 + 10^-6), as 'nearestCell'
        v <- fadd u (C.double 0.500001)
        f <- call (getExtern "floor") [(v, [])]
        n <- fptosi f AST.i32
        add n (C.int32 1)
  j <- fsub x ox >>= (`fdiv` sx) >>= nearest
  k <- fsub oy y >>= (`fdiv` sy) >>= nearest
  pure [j, k]

buildTablePoint :: Build m => (String -> Operand) -> ArenaState -> TypeProxy t -> Operand -> Operand -> Operand -> m (Op t)
buildTablePoint getExtern arena ty desc x y =
  pointIndices getExtern desc x y >>= buildTableRead arena ty desc

-- | A table's size, as an integer or nested pairs of them.
buildTableSize :: Build m => TypeProxy sz -> Operand -> m (Op sz)
buildTableSize sz0 desc = go sz0 0
  where
    go :: Build m => TypeProxy s -> Integer -> m (Op s)
    go sz k = case sz of
      IntegerType -> IntegerOp <$> loadField desc (48 + 4 * k) AST.i32
      PairType IntegerType rest -> PairOp <$> go IntegerType k <*> go rest (k + 1)
      _ -> throwError "internal error: a table's size doesn't fit its type"

-- | Whether a point is on a 2-dimensional table: whether the cell nearest
-- it is in range (as 'Language.Table.pointCell'; no index error).
buildTableContains :: Build m => (String -> Operand) -> TypeProxy t -> Operand -> Operand -> Operand -> m (Op 'BooleanT)
buildTableContains getExtern ty desc x y = do
  ixs <- pointIndices getExtern desc x y
  (inRange, _) <- tableCell ty desc ixs
  pure (BooleanOp inRange)

-- | The indices of the cell nearest a point, in range or not, as an
-- integer or nested pairs of them (as 'buildTableSize').
buildTableCell :: Build m => (String -> Operand) -> TypeProxy sz -> Operand -> Operand -> Operand -> m (Op sz)
buildTableCell getExtern sz0 desc x y = pointIndices getExtern desc x y >>= go sz0
  where
    go :: Build m => TypeProxy s -> [Operand] -> m (Op s)
    go sz ixs = case (sz, ixs) of
      (IntegerType, [i]) -> pure (IntegerOp i)
      (PairType IntegerType rest, i : is) -> PairOp (IntegerOp i) <$> go rest is
      _ -> throwError "internal error: a table's cell doesn't fit its type"

-- | The point at the center of a 2-dimensional table's cell, given its
-- column and row (as 'Language.Table.cellPoint').
buildTableCellPoint :: Build m => Operand -> Operand -> Operand -> m (Op 'ComplexT)
buildTableCellPoint desc j k = do
  ox <- loadField desc 16 AST.double
  oy <- loadField desc 24 AST.double
  sx <- loadField desc 32 AST.double
  sy <- loadField desc 40 AST.double
  let steps n = sub n (C.int32 1) >>= (`sitofp` AST.double)
  x <- steps j >>= (`fmul` sx) >>= fadd ox
  y <- steps k >>= (`fmul` sy) >>= fsub oy
  pure (ComplexOp x y)

-- | Mark the table a descriptor describes as shared (see 'tableFrozen').
freezeDescriptor :: Build m => Operand -> m ()
freezeDescriptor desc = do
  p <- field desc 12 AST.i32
  store p 0 (C.int32 1)

-- | Write a cell of the table in a variable's slot (an @i8**@), copying
-- the table first if it's frozen; out of range sets the index-error flag.
buildTableWrite :: Build m => (String -> Operand) -> ArenaState -> Operand -> TypeProxy t -> Operand
                -> Either [Operand] (Operand, Operand) -> Op t -> m ()
buildTableWrite getExtern arena thawF ty slot cell v = do
  size <- elemBytes ty
  desc0 <- load slot 0
  frozen <- loadField desc0 12 AST.i32
  isFrozen <- icmp IP.NE frozen (C.int32 0)
  mdo
    condBr isFrozen thawB writeB
    thawB <- block `named` "table_thaw"
    copy <- call thawF [(C.int32 size, []), (desc0, [])]
    store slot 0 copy
    br writeB
    writeB <- block `named` "table_write"
    pure ()
  desc <- load slot 0
  ixs <- either pure (\(x, y) -> pointIndices getExtern desc x y) cell
  (inRange, p) <- tableCell ty desc ixs
  mdo
    condBr inRange okB badB
    okB <- block `named` "table_store"
    storeElemAt ty p v
    br doneB
    badB <- block `named` "table_write_out_of_range"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "table_write_done"
    pure ()

-- | Give the variable in a slot a new table; a negative size sets the
-- index-error flag (so the tool's changes are discarded).
buildTableAllocate :: Build m => ArenaState -> Operand -> TypeProxy t -> Operand
                   -> [Operand] -> (Operand, Operand) -> (Operand, Operand) -> Maybe (Op t) -> m ()
buildTableAllocate arena newF ty slot sizes (x1, y1) (x2, y2) fill = do
  size <- elemBytes ty
  dims <- alloca AST.i32 (Just (C.int32 (fromIntegral (max 1 (length sizes))))) 0
  forM_ (zip [0 ..] sizes) $ \(k, n) -> do
    p <- gep dims [C.int32 k]
    store p 0 n
  desc <- call newF [ (C.int32 size, []), (C.int32 (fromIntegral (length sizes)), []), (dims, [])
                    , (x1, []), (y1, []), (x2, []), (y2, []) ]
  isNull <- icmp IP.EQ desc nullI8Ptr
  mdo
    condBr isNull badB okB
    okB <- block `named` "table_allocated"
    store slot 0 desc
    forM_ fill (fillCells ty desc sizes)
    br doneB
    badB <- block `named` "table_bad_size"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "table_allocate_done"
    pure ()

-- | Store a value in every cell of a new table, with the given sizes.
fillCells :: Build m => TypeProxy t -> Operand -> [Operand] -> Op t -> m ()
fillCells ty desc sizes v = do
  size <- elemBytes ty
  count <- foldM mul (C.int32 1) sizes
  elems <- loadField desc 0 (AST.ptr AST.i8)
  counter <- alloca AST.i32 Nothing 0
  store counter 0 (C.int32 0)
  mdo
    br testB
    testB <- block `named` "table_fill_test"
    k <- load counter 0
    more <- icmp IP.SLT k count
    condBr more bodyB doneB
    bodyB <- block `named` "table_fill"
    offset <- sext k AST.i64 >>= (`mul` C.int64 size)
    p <- gep elems [offset]
    storeElemAt ty p v
    add k (C.int32 1) >>= store counter 0
    br testB
    doneB <- block `named` "table_fill_done"
    pure ()
