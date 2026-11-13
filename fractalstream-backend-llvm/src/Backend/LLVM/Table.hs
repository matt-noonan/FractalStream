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
  , buildSliceView
  , buildTableWriteSlice
  , buildTableCopySlice
  , buildTableResize
  , freezeDescriptor
  , pointIndices
  , elemBytes
  , tableNewFnType
  , tableThawFnType
  , tableResizeFnType
  , tableCopyFnType
  , TableCallbacks(..)
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
import Foreign.Storable (pokeByteOff, peekElemOff)
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
type TableResize = Int32 -> Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr Word8 -> IO (Ptr Word8)
type TableCopy = Int32 -> Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr Word8
              -> Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr Word8 -> IO Int32

foreign import ccall "wrapper"
  wrapTableNew :: TableNew -> IO (FunPtr TableNew)
foreign import ccall "wrapper"
  wrapTableThaw :: TableThaw -> IO (FunPtr TableThaw)
foreign import ccall "wrapper"
  wrapTableResize :: TableResize -> IO (FunPtr TableResize)
foreign import ccall "wrapper"
  wrapTableCopy :: TableCopy -> IO (FunPtr TableCopy)

-- | The host's table callbacks, for a tool handler (see
-- 'withTableCallbacks').
data TableCallbacks = TableCallbacks
  { tcNew    :: FunPtr TableNew
  , tcThaw   :: FunPtr TableThaw
  , tcResize :: FunPtr TableResize
  , tcCopy   :: FunPtr TableCopy
  }

-- | Run a tool handler with the host's table callbacks:
--
-- * @new(element size, n, sizes, x1, y1, x2, y2)@: a descriptor for a new
--   table, filled with zeros, covering the rectangle from (x1, y1) (top
--   left) to (x2, y2); null if a size is negative.
-- * @thaw(element size, descriptor)@: a descriptor for a copy of the table.
-- * @resize(element size, descriptor, n, sizes, fill)@: a descriptor for a
--   copy of the table with new sizes, in the same place (see
--   'Language.Table.resizeTableBytes'), its new cells filled with the
--   element at @fill@ (or zeros, if it's null); null if a size is
--   negative.
-- * @copy(element size, to, n, indices, stars, from, m, indices, stars)@:
--   copy a slice of the table @from@ onto one of the table @to@ (which
--   must be the tool's own: thawed, or new), each given by its indices and
--   which of them are @*@s (a nonzero byte); 1 if it went well, 0 if an
--   index was out of range or the slices' shapes are different.
--
-- The descriptors they make are freed when the action finishes, so the
-- action must do its write-back first.
withTableCallbacks :: (TableCallbacks -> IO a) -> IO a
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
      resize size d n dimsP fillP = lookupDescriptor d >>= \case
        Nothing -> pure nullPtr
        Just t -> do
          dims <- map fromIntegral <$> peekArray (fromIntegral n) dimsP
          resizeTableBytes (fromIntegral size) t dims (if fillP == nullPtr then Nothing else Just fillP) >>= \case
            Nothing -> pure nullPtr
            Just t' -> remember =<< newDescriptor t' False
      copy size dstD n dstIxP dstStarP srcD m srcIxP srcStarP = do
        mdst <- lookupDescriptor dstD
        msrc <- lookupDescriptor srcD
        dstIxs <- sliceOf n dstIxP dstStarP
        srcIxs <- sliceOf m srcIxP srcStarP
        case (mdst, msrc) of
          (Just dst, Just src) -> do
            ok <- copySliceBytes (fromIntegral size) dst dstIxs src srcIxs
            pure (if ok then 1 else 0)
          _ -> pure 0
      sliceOf n ixP starP = forM [0 .. fromIntegral n - 1] $ \k -> do
        star <- peekElemOff starP k
        ix <- peekElemOff ixP k
        pure (if star /= 0 then Every else Fixed (fromIntegral ix))
  bracket (wrapTableNew new) freeHaskellFunPtr $ \newFP ->
    bracket (wrapTableThaw thaw) freeHaskellFunPtr $ \thawFP ->
    bracket (wrapTableResize resize) freeHaskellFunPtr $ \resizeFP ->
    bracket (wrapTableCopy copy) freeHaskellFunPtr $ \copyFP ->
      action (TableCallbacks newFP thawFP resizeFP copyFP)
        `finally` (readIORef made >>= mapM_ freeDescriptor)

------------------------------------------------------------------------
-- Code generation

type Build m = (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)

-- | The host's @new@ and @thaw@ callbacks (see 'withTableCallbacks').
tableNewFnType, tableThawFnType :: AST.Type
tableNewFnType = AST.ptr (AST.FunctionType (AST.ptr AST.i8)
  [AST.i32, AST.i32, AST.ptr AST.i32, AST.double, AST.double, AST.double, AST.double] False)
tableThawFnType = AST.ptr (AST.FunctionType (AST.ptr AST.i8) [AST.i32, AST.ptr AST.i8] False)

-- | The host's @resize@ and @copy@ callbacks (see 'withTableCallbacks').
tableResizeFnType, tableCopyFnType :: AST.Type
tableResizeFnType = AST.ptr (AST.FunctionType (AST.ptr AST.i8)
  [AST.i32, AST.ptr AST.i8, AST.i32, AST.ptr AST.i32, AST.ptr AST.i8] False)
tableCopyFnType = AST.ptr (AST.FunctionType AST.i32
  [ AST.i32, AST.ptr AST.i8, AST.i32, AST.ptr AST.i32, AST.ptr AST.i8
  , AST.ptr AST.i8, AST.i32, AST.ptr AST.i32, AST.ptr AST.i8 ] False)

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

-- | Read the cell nearest a point, at some indices in the dimensions
-- after the first two.
buildTablePoint :: Build m => (String -> Operand) -> ArenaState -> TypeProxy t -> Operand -> Operand -> Operand
                -> [Operand] -> m (Op t)
buildTablePoint getExtern arena ty desc x y rest = do
  ixs <- pointIndices getExtern desc x y
  buildTableRead arena ty desc (ixs ++ rest)

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
                -> Either [Operand] ((Operand, Operand), [Operand]) -> Op t -> m ()
buildTableWrite getExtern arena thawF ty slot cell v = do
  desc <- thawSlot thawF ty slot
  ixs <- either pure (\((x, y), rest) -> (++ rest) <$> pointIndices getExtern desc x y) cell
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

-- | The table in a variable's slot (an @i8**@), copied first if it's
-- frozen, so that the tool can write it in place.
thawSlot :: Build m => Operand -> TypeProxy t -> Operand -> m Operand
thawSlot thawF ty slot = do
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
  load slot 0

-- | Where a slice of a table is (as 'Language.Table.sliceView'), given
-- its indices, one per dimension, each an integer or a @*@ ('Nothing'):
-- whether its integers are in range, its first cell (counting cells from
-- 0), and for each @*@ the number of cells it spans and how far apart
-- they are.
buildSliceView :: Build m => Operand -> [Maybe Operand] -> m (Operand, Operand, [(Operand, Operand)])
buildSliceView desc ixs = do
  let step (inRange, base, stride, spans) (k, mi) = do
        dim <- loadField desc (48 + 4 * k) AST.i32
        stride' <- mul stride dim
        case mi of
          Nothing -> pure (inRange, base, stride', spans ++ [(dim, stride)])
          Just i -> do
            atLeast1 <- icmp IP.SGE i (C.int32 1)
            atMost <- icmp IP.SLE i dim
            inRange' <- I.and inRange =<< I.and atLeast1 atMost
            term <- sub i (C.int32 1) >>= (`mul` stride)
            base' <- add base term
            pure (inRange', base', stride', spans)
  (inRange, base, _, spans) <- foldM step (C.bit 1, C.int32 0, C.int32 1, []) (zip [0 ..] ixs)
  pure (inRange, base, spans)

-- | A pointer to the element of the cell with the given number (counting
-- from 0).
cellPtr :: Build m => TypeProxy t -> Operand -> Operand -> m Operand
cellPtr ty desc cell = do
  size <- elemBytes ty
  elems <- loadField desc 0 (AST.ptr AST.i8)
  byteOffset <- sext cell AST.i64 >>= (`mul` C.int64 size)
  gep elems [byteOffset]

-- | Write a slice with one @*@ of the table in a variable's slot, from a
-- list with one element for each of its cells; out of range, or a list of
-- another length, sets the index-error flag.
buildTableWriteSlice :: Build m => ArenaState -> Operand -> TypeProxy t -> Operand -> [Maybe Operand] -> Operand -> m ()
buildTableWriteSlice arena thawF ty slot ixs list = do
  desc <- thawSlot thawF ty slot
  (inRange, base, spans) <- buildSliceView desc ixs
  (count, stride) <- case spans of
    [span1] -> pure span1
    _ -> throwError "internal error: a list is written to a slice with one `*`"
  len <- listLength list
  sameLength <- icmp IP.EQ len count
  ok <- I.and inRange sameLength
  iSlot <- alloca AST.i32 Nothing 0
  store iSlot 0 (C.int32 0)
  mdo
    condBr ok headB badB
    headB <- block `named` "slice_write_head"
    i <- load iSlot 0
    more <- icmp IP.SLT i count
    condBr more bodyB doneB
    bodyB <- block `named` "slice_write"
    v <- listSlot ty list i >>= loadListElem ty
    cell <- mul i stride >>= add base
    p <- cellPtr ty desc cell
    storeElemAt ty p v
    add i (C.int32 1) >>= store iSlot 0
    br headB
    badB <- block `named` "slice_write_bad"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "slice_write_done"
    pure ()

-- | Copy a slice of a table (a descriptor) onto a slice of the table in a
-- variable's slot, each given by its indices (each an integer or a @*@),
-- by the host's @copy@; a failure sets the index-error flag.
buildTableCopySlice :: Build m => ArenaState -> Operand -> Operand -> TypeProxy t -> Operand -> [Maybe Operand]
                    -> Operand -> [Maybe Operand] -> m ()
buildTableCopySlice arena thawF copyF ty slot ixs srcDesc srcIxs = do
  size <- elemBytes ty
  desc <- thawSlot thawF ty slot
  (dstIxP, dstStarP) <- sliceArrays ixs
  (srcIxP, srcStarP) <- sliceArrays srcIxs
  ok <- call copyF [ (C.int32 size, []), (desc, []), (C.int32 (fromIntegral (length ixs)), []), (dstIxP, []), (dstStarP, [])
                   , (srcDesc, []), (C.int32 (fromIntegral (length srcIxs)), []), (srcIxP, []), (srcStarP, []) ]
  failed <- icmp IP.EQ ok (C.int32 0)
  mdo
    condBr failed badB doneB
    badB <- block `named` "slice_copy_bad"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "slice_copy_done"
    pure ()
  where
    sliceArrays mixs = do
      let n = C.int32 (fromIntegral (max 1 (length mixs)))
      ixP <- alloca AST.i32 (Just n) 0
      starP <- alloca AST.i8 (Just n) 0
      forM_ (zip [0 ..] mixs) $ \(k, mi) -> do
        p <- gep ixP [C.int32 k]
        q <- gep starP [C.int32 k]
        store p 0 (fromMaybe (C.int32 0) mi)
        store q 0 (C.int8 (if isJust mi then 0 else 1))
      pure (ixP, starP)

-- | Give the variable in a slot its table with new sizes, by the host's
-- @resize@; a negative size sets the index-error flag.
buildTableResize :: Build m => ArenaState -> Operand -> TypeProxy t -> Operand -> [Operand] -> Maybe (Op t) -> m ()
buildTableResize arena resizeF ty slot sizes fill = do
  size <- elemBytes ty
  dims <- alloca AST.i32 (Just (C.int32 (fromIntegral (max 1 (length sizes))))) 0
  forM_ (zip [0 ..] sizes) $ \(k, n) -> do
    p <- gep dims [C.int32 k]
    store p 0 n
  fillP <- case fill of
    Nothing -> pure nullI8Ptr
    Just v -> do
      buf <- alloca AST.i8 (Just (C.int32 size)) 8
      storeElemAt ty buf v
      pure buf
  desc0 <- load slot 0
  desc <- call resizeF [ (C.int32 size, []), (desc0, []), (C.int32 (fromIntegral (length sizes)), [])
                       , (dims, []), (fillP, []) ]
  isNull <- icmp IP.EQ desc nullI8Ptr
  mdo
    condBr isNull badB okB
    okB <- block `named` "table_resized"
    store slot 0 desc
    br doneB
    badB <- block `named` "table_bad_resize"
    store (asIndexError arena) 0 (C.bit 1)
    br doneB
    doneB <- block `named` "table_resize_done"
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
