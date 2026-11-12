-- | A table's storage: one contiguous buffer of elements, laid out the way
-- compiled code reads them, along with its size and where it lies in the
-- plane. Kept free of "Language.Type" (which names 'Table' as the Haskell
-- type of a table value); "Language.Table" reads and writes elements.
--
-- A table value is immutable once a script can see it: a tool's writes go
-- to a copy, which replaces the table only when the tool's handler
-- succeeds. The buffer is a 'ForeignPtr', so GHC frees it once nothing
-- (no configuration value, no render in flight) refers to it.
module Language.Table.Buffer
  ( Table(..)
  , emptyTable
  , tableCellCount
  , newTableVersion
  ) where

import Prelude
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (mallocForeignPtrBytes)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

data Table = Table
  { tableDims   :: [Int]
    -- ^ The size along each dimension; for a 2-D table, (columns, rows).
  , tableOrigin :: (Double, Double)
    -- ^ The model point at the center of cell (1, 1), the top left.
  , tableStep   :: (Double, Double)
    -- ^ A cell's model width and height: columns go right, rows go down.
  , tableBuffer :: ForeignPtr Word8
    -- ^ The elements, the first index varying fastest.
  , tableFrozen :: IORef Bool
    -- ^ Whether anything else may be looking at this table: once it's
    -- the value of a configuration variable (so a viewer may be reading
    -- it), or another variable's. Writing a frozen table writes a copy;
    -- a table a tool has just made, or copied, it may write in place.
  , tableVersion :: Int
    -- ^ Which buffer this is: unique to it ('newTableVersion'), so a
    -- frontend can keep copies of a table's contents (as the web app's
    -- workers do) and know them again. (0 for every empty table.) A
    -- frozen table's contents never change, so neither do its version's.
  }

-- | Two tables are the same value when they share their storage.
instance Eq Table where
  a == b = tableBuffer a == tableBuffer b && tableDims a == tableDims b

instance Ord Table where
  compare a b = compare (tableBuffer a, tableDims a) (tableBuffer b, tableDims b)

instance Show Table where
  show t = "(table of size " ++ show (tableDims t) ++ ")"

-- | A table with no cells, which is what a table variable starts out as.
emptyTable :: Int -> Table
emptyTable n = Table (replicate n 0) (0, 0) (1, 1) emptyBuffer frozen 0
  where frozen = unsafePerformIO (newIORef True)

-- | A new table buffer's version (see 'tableVersion').
newTableVersion :: IO Int
newTableVersion = atomicModifyIORef' versions (\v -> (v + 1, v + 1))

versions :: IORef Int
versions = unsafePerformIO (newIORef 0)
{-# NOINLINE versions #-}

emptyBuffer :: ForeignPtr Word8
emptyBuffer = unsafePerformIO (mallocForeignPtrBytes 0)
{-# NOINLINE emptyBuffer #-}

tableCellCount :: Table -> Int
tableCellCount = product . tableDims
