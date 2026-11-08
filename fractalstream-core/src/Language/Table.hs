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
  , writeCell
  , newTable
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Table.Buffer
import Data.Color (colorToRGB, rgbToColor)

import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Utils (fillBytes)
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
  Just size -> do
    let bytes = size * product dims
    buf <- mallocForeignPtrBytes (max 1 bytes)
    withForeignPtr buf $ \p -> fillBytes p 0 bytes
    pure (Just (Table dims origin step buf))
