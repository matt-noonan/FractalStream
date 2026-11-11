-- | Tables: reading and writing their elements (see "Language.Table.Buffer"
-- for their storage). Elements are laid out the way compiled code reads
-- them: a truth value in 1 byte, an integer in 4 (as compiled code holds
-- it), a real in 8, a complex number in 16 (real, then imaginary part), a
-- color in 3 (red, green, blue). The first index varies fastest.
module Language.Table
  ( Table(..)
  , emptyTable
  , tableCellCount
  , isTableElementType
  , elementSize
  , cellOffset
  , readCell
  , pointCell
  , pointIndices
  , cellPoint
  , writeCell
  , fillTable
  , newTable
  , allocateTable
  , allocateTableBytes
  , copyTableBytes
  , nearestCell
  , freezeTable
  , thawForWrite
    -- * Slices
  , SliceIndex(..)
  , SliceView(..)
  , sliceIndices
  , sliceView
  , sliceCells
  , readSlice
  , writeSlice
  , copySliceBytes
    -- * Resizing
  , resizeTable
  , resizeTableBytes
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Table.Buffer
import Data.Color (colorToRGB, rgbToColor)

import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Utils (fillBytes, copyBytes)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.IORef (newIORef, writeIORef, readIORef)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)

-- | The types a table can hold.
isTableElementType :: FSType -> Bool
isTableElementType = (`elem` [BooleanT, IntegerT, RealT, ComplexT, ColorT])

-- | How many bytes one element takes, for the types a table can hold.
elementSize :: TypeProxy t -> Maybe Int
elementSize = \case
  BooleanType -> Just 1
  IntegerType -> Just 4
  RealType    -> Just 8
  ComplexType -> Just 16
  ColorType   -> Just 3
  _           -> Nothing

-- | Where the cell at some 1-based indices is, counting cells from 0, or
-- 'Nothing' if an index is out of range (or there are the wrong number).
cellOffset :: Table -> [Int] -> Maybe Int
cellOffset t ixs
  | length ixs /= length dims = Nothing
  | and (zipWith (\k n -> k >= 1 && k <= n) ixs dims) =
      Just (foldr (\(k, n) acc -> (k - 1) + n * acc) 0 (zip ixs dims))
  | otherwise = Nothing
  where dims = tableDims t

-- | The indices (column and row) of the cell nearest a point, if the
-- point is on the table: within its first two dimensions, which are the
-- ones laid out in the plane.
pointCell :: Table -> Complex Double -> Maybe [Int]
pointCell t z = do
  let ixs = pointIndices t z
  guard (and (zipWith (\k n -> k >= 1 && k <= n) ixs (tableDims t)) && length (tableDims t) >= 2)
  pure ixs

-- | The column and row of a 2-dimensional table's cell that a point is
-- in, whether or not that's in range.
pointIndices :: Table -> Complex Double -> [Int]
pointIndices t (x :+ y) =
  let (x0, y0) = tableOrigin t
      (dx, dy) = tableStep t
  in [ nearestCell ((x - x0) / dx), nearestCell ((y0 - y) / dy) ]

-- | The point at the center of a 2-dimensional table's cell, by its
-- column and row (in range or not).
cellPoint :: Table -> Int -> Int -> Complex Double
cellPoint t j k =
  let (x0, y0) = tableOrigin t
      (dx, dy) = tableStep t
  in (x0 + fromIntegral (j - 1) * dx) :+ (y0 - fromIntegral (k - 1) * dy)

-- | The 1-based index of the cell nearest a position measured in cells
-- from the center of cell 1: the cell the position is in. A position on
-- the boundary between two cells, give or take a millionth of a cell, is
-- in the second: viewers sample each pixel at its top left corner, which
-- for a table laid over the window is exactly on a boundary, and rounding
-- shouldn't push it into the cell before. Compiled code does the same
-- ("Backend.LLVM.Table").
nearestCell :: Double -> Int
nearestCell u = 1 + floor (u + 0.5 + 0.000001)

-- | The element at some 1-based indices, if they're in range.
readCell :: TypeProxy t -> Table -> [Int] -> Maybe (HaskellType t)
readCell ty t ixs = do
  size <- elementSize ty
  cell <- cellOffset t ixs
  pure $ unsafePerformIO $ withForeignPtr (tableBuffer t) $ \p ->
    peekElement ty (p `plusPtr` (cell * size))

peekElement :: TypeProxy t -> Ptr Word8 -> IO (HaskellType t)
peekElement ty p = case ty of
  BooleanType -> (/= (0 :: Word8)) <$> peekByteOff p 0
  IntegerType -> fromIntegral <$> (peekByteOff p 0 :: IO Int32)
  RealType    -> peekByteOff p 0
  ComplexType -> (:+) <$> peekByteOff p 0 <*> peekByteOff p 8
  ColorType   -> (\r g b -> rgbToColor (r, g, b)) <$> peekByteOff p 0 <*> peekByteOff p 1 <*> peekByteOff p 2
  _ -> error "peekElement: not a table element type"

pokeElement :: TypeProxy t -> Ptr Word8 -> HaskellType t -> IO ()
pokeElement ty p v = case ty of
  BooleanType -> pokeByteOff p 0 (if v then 1 else 0 :: Word8)
  IntegerType -> pokeByteOff p 0 (fromIntegral v :: Int32)
  RealType    -> pokeByteOff p 0 v
  ComplexType -> let x :+ y = v in pokeByteOff p 0 x >> pokeByteOff p 8 y
  ColorType   -> let (r, g, b) = colorToRGB v
                 in pokeByteOff p 0 r >> pokeByteOff p 1 g >> pokeByteOff p 2 b
  _ -> error "pokeElement: not a table element type"

-- | Write the element at some 1-based indices, in place: only for a table
-- no script can see yet (or a tool's own copy). False if the indices are
-- out of range.
writeCell :: TypeProxy t -> Table -> [Int] -> HaskellType t -> IO Bool
writeCell ty t ixs v = case (,) <$> elementSize ty <*> cellOffset t ixs of
  Nothing -> pure False
  Just (size, cell) -> withForeignPtr (tableBuffer t) $ \p -> do
    pokeElement ty (p `plusPtr` (cell * size)) v
    pure True

-- | A new table of the given size, origin, and cell step, filled with
-- zeros (false, 0, black).
newTable :: TypeProxy t -> [Int] -> (Double, Double) -> (Double, Double) -> IO (Maybe Table)
newTable ty dims origin step = case elementSize ty of
  Nothing -> pure Nothing
  Just size -> Just <$> newTableBytes size dims origin step

-- | 'newTable', for elements of the given size in bytes.
newTableBytes :: Int -> [Int] -> (Double, Double) -> (Double, Double) -> IO Table
newTableBytes size dims origin step = do
  let bytes = size * product dims
  buf <- mallocForeignPtrBytes (max 1 bytes)
  withForeignPtr buf $ \p -> fillBytes p 0 bytes
  frozen <- newIORef False
  pure (Table dims origin step buf frozen)

-- | Fill every cell of a table with a value, in place (only for a table
-- no script can see yet).
fillTable :: TypeProxy t -> Table -> HaskellType t -> IO ()
fillTable ty t v = forM_ (elementSize ty) $ \size -> withForeignPtr (tableBuffer t) $ \p ->
  forM_ [0 .. tableCellCount t - 1] $ \cell -> pokeElement ty (p `plusPtr` (cell * size)) v

-- | A new table of the given size, filled with zeros, covering the
-- rectangle with the given corners (top left, then bottom right): its
-- cells split the rectangle evenly, so cell (1, 1) is centered half a
-- cell in from the top left corner. (Only the first two dimensions are
-- laid out in the plane.)
allocateTable :: TypeProxy t -> [Int] -> Complex Double -> Complex Double -> IO (Maybe Table)
allocateTable ty dims p q = case elementSize ty of
  Nothing -> pure Nothing
  Just size -> Just <$> allocateTableBytes size dims p q

-- | 'allocateTable', for elements of the given size in bytes.
allocateTableBytes :: Int -> [Int] -> Complex Double -> Complex Double -> IO Table
allocateTableBytes size dims (x1 :+ y1) (x2 :+ y2) =
  let (w, h) = case dims of
        (a : b : _) -> (a, b)
        [a] -> (a, 1)
        [] -> (1, 1)
      dx = (x2 - x1) / fromIntegral (max 1 w)
      dy = (y1 - y2) / fromIntegral (max 1 h)
  in newTableBytes size dims (x1 + dx / 2, y1 - dy / 2) (dx, dy)

-- | Mark a table as one that others may be looking at (see 'tableFrozen').
freezeTable :: Table -> IO ()
freezeTable t = writeIORef (tableFrozen t) True

-- | A table that can be written in place: this one, unless it's frozen,
-- or else a copy of it.
thawForWrite :: TypeProxy t -> Table -> IO Table
thawForWrite ty t = readIORef (tableFrozen t) >>= \case
  False -> pure t
  True -> copyTableBytes (fromMaybe 0 (elementSize ty)) t

-- | A copy of a table with elements of the given size in bytes, which
-- no one else is looking at.
copyTableBytes :: Int -> Table -> IO Table
copyTableBytes size t = do
  let bytes = size * tableCellCount t
  buf <- mallocForeignPtrBytes (max 1 bytes)
  withForeignPtr (tableBuffer t) $ \src -> withForeignPtr buf $ \dst -> copyBytes dst src bytes
  frozen <- newIORef False
  pure t { tableBuffer = buf, tableFrozen = frozen }

------------------------------------------------------------------------
-- Slices

-- | One index of a slice: a fixed index (1-based), or every index along
-- that dimension (a @*@).
data SliceIndex = Fixed Int | Every
  deriving (Eq, Show)

-- | A slice's indices, one per dimension, from a point standing for the
-- first two (if there is one; off the table is an error) and the rest,
-- each an integer or a @*@ ('Nothing').
sliceIndices :: Table -> Maybe (Complex Double) -> [Maybe Int] -> [SliceIndex]
sliceIndices t mp ixs = maybe [] pointed mp ++ map (maybe Every Fixed) ixs
  where
    pointed p = map Fixed $ fromMaybe (error ("The point " ++ show p ++ " is off the table"))
                                      (pointCell t p)

-- | Where a slice's cells are: the first one (counting cells from 0), and
-- for each @*@, in order, how many cells it spans and how far apart they
-- are.
data SliceView = SliceView
  { sliceBase  :: Int
  , sliceSpans :: [(Int, Int)]
  } deriving (Eq, Show)

-- | Where the slice with the given indices (one per dimension) is, or
-- 'Nothing' if there are the wrong number of them or a fixed one is out
-- of range.
sliceView :: Table -> [SliceIndex] -> Maybe SliceView
sliceView t ixs = do
  let dims = tableDims t
      strides = scanl (*) 1 dims
  guard (length ixs == length dims)
  parts <- forM (zip3 ixs dims strides) $ \case
    (Fixed k, n, stride) | k >= 1 && k <= n -> Just (Left ((k - 1) * stride))
                         | otherwise -> Nothing
    (Every, n, stride) -> Just (Right (n, stride))
  pure (SliceView (sum (lefts parts)) (rights parts))

-- | The cells of a slice, counting from 0, the first @*@ varying fastest.
sliceCells :: SliceView -> [Int]
sliceCells (SliceView base spans) = map (base +) (go spans)
  where
    go [] = [0]
    go ((n, stride) : rest) = [ i * stride + o | o <- go rest, i <- [0 .. n - 1] ]

-- | The elements of a slice, if its indices are in range.
readSlice :: TypeProxy t -> Table -> [SliceIndex] -> Maybe [HaskellType t]
readSlice ty t ixs = do
  size <- elementSize ty
  view <- sliceView t ixs
  pure $ unsafePerformIO $ withForeignPtr (tableBuffer t) $ \p ->
    forM (sliceCells view) $ \cell -> peekElement ty (p `plusPtr` (cell * size))

-- | Write a slice's elements, in place (only for a table no script can
-- see yet, as 'writeCell'). False if its indices are out of range, or
-- there aren't as many values as it has cells.
writeSlice :: TypeProxy t -> Table -> [SliceIndex] -> [HaskellType t] -> IO Bool
writeSlice ty t ixs vs = case (,) <$> elementSize ty <*> sliceView t ixs of
  Nothing -> pure False
  Just (size, view) -> do
    let cells = sliceCells view
    if length cells /= length vs
      then pure False
      else withForeignPtr (tableBuffer t) $ \p -> do
        forM_ (zip cells vs) $ \(cell, v) -> pokeElement ty (p `plusPtr` (cell * size)) v
        pure True

-- | Copy one slice onto another, of elements of the given size in bytes:
-- the destination is written in place (only for a table no script can see
-- yet, as 'writeCell'). The source may be the same table. False if either
-- slice's indices are out of range, or their @*@s don't span the same
-- numbers of cells, in order.
copySliceBytes :: Int -> Table -> [SliceIndex] -> Table -> [SliceIndex] -> IO Bool
copySliceBytes size dst dstIxs src srcIxs = case (,) <$> sliceView dst dstIxs <*> sliceView src srcIxs of
  Nothing -> pure False
  Just (dv, sv)
    | map fst (sliceSpans dv) /= map fst (sliceSpans sv) -> pure False
    | otherwise -> do
        let pairs = zip (sliceCells dv) (sliceCells sv)
            count = length pairs
        -- (through a buffer of its own, in case the slices overlap)
        allocaBytes (max 1 (count * size)) $ \tmp ->
          withForeignPtr (tableBuffer src) $ \s -> withForeignPtr (tableBuffer dst) $ \d -> do
            forM_ (zip [0 ..] pairs) $ \(i, (_, from)) ->
              copyBytes (tmp `plusPtr` (i * size)) (s `plusPtr` (from * size)) size
            forM_ (zip [0 ..] pairs) $ \(i, (to, _)) ->
              copyBytes (d `plusPtr` (to * size)) (tmp `plusPtr` (i * size)) size
        pure True

------------------------------------------------------------------------
-- Resizing

-- | A new table with another size, in the same place: its cell (1, 1) is
-- where the old one's was, and its cells are the same size, so each cell
-- the two share is in the same place and keeps its value. New cells are
-- filled with the given value, or zeros. 'Nothing' if a size is negative
-- (or the type can't be in a table).
resizeTable :: TypeProxy t -> Table -> [Int] -> Maybe (HaskellType t) -> IO (Maybe Table)
resizeTable ty t dims fill = case elementSize ty of
  Nothing -> pure Nothing
  Just size -> case fill of
    Nothing -> resizeTableBytes size t dims Nothing
    Just v -> allocaBytes size $ \p -> do
      pokeElement ty p v
      resizeTableBytes size t dims (Just p)

-- | 'resizeTable', for elements of the given size in bytes, filling new
-- cells with a copy of the element at the given pointer (or zeros).
resizeTableBytes :: Int -> Table -> [Int] -> Maybe (Ptr Word8) -> IO (Maybe Table)
resizeTableBytes size t dims fill
  | any (< 0) dims || length dims /= length (tableDims t) = pure Nothing
  | otherwise = do
      t' <- newTableBytes size dims (tableOrigin t) (tableStep t)
      withForeignPtr (tableBuffer t') $ \d -> do
        forM_ fill $ \v -> forM_ [0 .. product dims - 1] $ \cell ->
          copyBytes (d `plusPtr` (cell * size)) v size
        let old = tableDims t
            common = zipWith min old dims
        case common of
          (run : rest) | run > 0 && all (> 0) rest ->
            withForeignPtr (tableBuffer t) $ \s ->
              -- one run along the first dimension for each choice of the
              -- other indices
              forM_ (mapM (\n -> [0 .. n - 1]) rest) $ \others -> do
                let offset ds = sum (zipWith (*) others (drop 1 (scanl (*) 1 ds)))
                copyBytes (d `plusPtr` (offset dims * size)) (s `plusPtr` (offset old * size)) (run * size)
          _ -> pure ()
      pure (Just t')
