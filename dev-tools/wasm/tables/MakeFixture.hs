{-# LANGUAGE DataKinds, TypeApplications #-}
-- | Write tables-read.fs: tables-read.fs.in (a project whose viewer reads
-- tables in every way a viewer can), with saved tables for it to read,
-- made here with the core's own code. (fsrun can't save tables.)
--
--   cd fractalstream-ui-headless && stack runghc ../dev-tools/wasm/tables/MakeFixture.hs
module Main (main) where

import Actor.SavedTables (captureTables, writeSavedTables)
import Data.Color (rgbToColor)
import Data.Complex (Complex(..))
import Data.DynamicValue (newVariable)
import Language.Table (Table, newTable, writeCell)
import Language.Type
import GHC.TypeNats (natSing)

import Actor.Ensemble (Ensemble(..), parseEnsembleFromBytes, encodeEnsemble)
import Data.DynamicValue (modifyValue)
import qualified Data.ByteString as BS
import Control.Monad (forM_, void)

-- | A table covering [-1.5, 1.5] x [-1, 1] (the viewer's view), each cell
-- given by f.
table :: TypeProxy t -> [Int] -> ([Int] -> HaskellType t) -> IO Table
table ty dims f = do
  let (w, h) = case dims of { (a : b : _) -> (a, b); [a] -> (a, 1); [] -> (1, 1) }
      step = (3 / fromIntegral w, 2 / fromIntegral h)
      origin = (-1.5 + fst step / 2, 1 - snd step / 2)
  Just t <- newTable ty dims origin step
  forM_ (sequence [ [1 .. n] | n <- dims ]) $ \ixs -> void (writeCell ty t ixs (f ixs))
  pure t

main :: IO ()
main = do
  let real = fromIntegral :: Int -> Double
  t <- table ComplexType [20, 15] (\[j, k] -> real j * 0.1 :+ real k * 0.07)
  b <- table BooleanType [20, 15] (\[j, k] -> (j + k) `mod` 3 == 0)
  p <- table (PairType RealType IntegerType) [10, 10, 3] (\[j, k, m] -> (real j * 0.5 + real m, fromIntegral (k * m)))
  k <- table ColorType [7, 5] (\[j, i] -> rgbToColor (fromIntegral (j * 30), fromIntegral (i * 50), 200))
  l <- table IntegerType [9] (\[j] -> fromIntegral (j * j))
  u <- table ComplexType [300, 200] (\[j, i] -> real j :+ real i)
  vars <- mapM newVariable [t, b, p, k, l, u]
  saved <- captureTables (zip3 ["T", "B", "P", "K", "L", "U"]
    [ SomeType (TableType (natSing @2) ComplexType), SomeType (TableType (natSing @2) BooleanType)
    , SomeType (TableType (natSing @3) (PairType RealType IntegerType)), SomeType (TableType (natSing @2) ColorType)
    , SomeType (TableType (natSing @1) IntegerType), SomeType (TableType (natSing @2) ComplexType) ] vars)
  -- (saved as the app saves a project with its tables)
  let template = "../dev-tools/wasm/tables/tables-read.fs.in"
  ens <- BS.readFile template >>= parseEnsembleFromBytes template >>= either fail pure
  modifyValue (ensembleFrontendProperties ens) (writeSavedTables saved)
  encodeEnsemble ens >>= BS.writeFile "../dev-tools/wasm/tables/tables-read.fs"
