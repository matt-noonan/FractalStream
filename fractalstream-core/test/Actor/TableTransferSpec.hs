{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# language OverloadedStrings #-}
module Actor.TableTransferSpec (spec) where

import Test.Hspec

import Actor.TableTransfer
import Data.Zip (readZip, writeZip, Packing(..))
import Language.Table
import Language.Type

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified GHC.TypeNats as TN

-- | A filled table, and its type.
filled :: TypeProxy t -> [Int] -> (Double, Double) -> (Double, Double) -> ([Int] -> HaskellType t) -> IO Table
filled ty size origin step f = do
  Just t <- newTable ty size origin step
  forM_ (cells size) $ \ixs -> writeCell ty t ixs (f ixs)
  pure t

cells :: [Int] -> [[Int]]
cells = mapM (\n -> [1 .. n])

table :: TN.SNat n -> TypeProxy t -> TypeProxy ('TableT n t)
table n ty = withKnownType ty (TableType n ty)

spec :: Spec
spec = describe "exporting and importing a table" $ do

  describe "as a zip of its description and elements" $ do
    let pairT = PairType ComplexType IntegerType
        ty3 = table (TN.SNat @3) pairT
    it "holds exactly <name>.yaml and <name>.bin" $ do
      t <- filled pairT [2, 3, 2] (-1, 1) (0.5, 0.5) (\[a, b, c] -> (fromIntegral a :+ fromIntegral b, fromIntegral c))
      Right zipped <- pure (exportTableZip Deflated "crits" ty3 t)
      fmap (map fst) (readZip (BL.toStrict zipped)) `shouldBe` Right ["crits.yaml", "crits.bin"]
    it "comes back as it was, size, origin and step and all" $ do
      t <- filled pairT [2, 3, 2] (-1, 1) (0.5, 0.25) (\[a, b, c] -> (fromIntegral a :+ fromIntegral b, fromIntegral (a * c)))
      Right zipped <- pure (exportTableZip Deflated "crits" ty3 t)
      Right t' <- importTableZip ty3 (BL.toStrict zipped)
      (tableDims t', tableOrigin t', tableStep t') `shouldBe` ([2, 3, 2], (-1, 1), (0.5, 0.25))
      forM_ (cells [2, 3, 2]) $ \ixs -> readCell pairT t' ixs `shouldBe` readCell pairT t ixs
    it "refuses one of another type, or another number of dimensions" $ do
      t <- filled RealType [4] (0, 0) (1, 1) (\[a] -> fromIntegral a)
      Right zipped <- pure (exportTableZip Deflated "T" (table (TN.SNat @1) RealType) t)
      importTableZip (table (TN.SNat @1) IntegerType) (BL.toStrict zipped)
        >>= (`shouldBe` Left "This is a Table 1 of ℝ, but the table is a Table 1 of ℤ.") . fmap (const ())
      importTableZip (table (TN.SNat @2) RealType) (BL.toStrict zipped)
        >>= (`shouldBe` Left "This is a Table 1 of ℝ, but the table is a Table 2 of ℝ.") . fmap (const ())
    it "refuses a file that isn't a zip, or a zip without a table" $ do
      importTableZip (table (TN.SNat @1) RealType) "1,2,3\n"
        >>= (`shouldBe` Left "This isn't a FractalStream table: it isn't a zip file.") . fmap (const ())
      Right other <- pure (writeZip Deflated [("notes.txt", "hello")])
      importTableZip (table (TN.SNat @1) RealType) (BL.toStrict other)
        >>= (`shouldBe` Left "This isn't a FractalStream table: there's no table description in it.") . fmap (const ())
    it "refuses one whose data isn't what its description says" $ do
      t <- filled RealType [4] (0, 0) (1, 1) (\[a] -> fromIntegral a)
      Right zipped <- pure (exportTableZip Deflated "T" (table (TN.SNat @1) RealType) t)
      Right entries <- pure (readZip (BL.toStrict zipped))
      Right damaged <- pure (writeZip Deflated [ (p, if p == "T.bin" then BL.map (+ 1) b else b) | (p, b) <- entries ])
      r <- importTableZip (table (TN.SNat @1) RealType) (BL.toStrict damaged)
      fmap (const ()) r `shouldSatisfy` isLeft

  describe "as CSV" $ do
    it "is offered for one or two dimensions of reals, integers, truth values and complex numbers" $ do
      csvAvailable (table (TN.SNat @1) RealType) `shouldBe` True
      csvAvailable (table (TN.SNat @2) ComplexType) `shouldBe` True
      csvAvailable (table (TN.SNat @2) BooleanType) `shouldBe` True
      csvAvailable (table (TN.SNat @3) RealType) `shouldBe` False
      csvAvailable (table (TN.SNat @2) ColorType) `shouldBe` False
      csvAvailable (table (TN.SNat @2) (PairType RealType RealType)) `shouldBe` False
    it "writes a 2-dimensional table a line a row, the first index along it, complex numbers as two values" $ do
      t <- filled ComplexType [2, 3] (0, 0) (1, 1) (\[a, b] -> fromIntegral a :+ fromIntegral (10 * b))
      exportTableCsv (table (TN.SNat @2) ComplexType) t `shouldBe`
        Right "1.0,10.0,2.0,10.0\n1.0,20.0,2.0,20.0\n1.0,30.0,2.0,30.0\n"
    it "writes a 1-dimensional table a value a line, truth values as 0 and 1" $ do
      t <- filled BooleanType [3] (0, 0) (1, 1) (\[a] -> odd a)
      exportTableCsv (table (TN.SNat @1) BooleanType) t `shouldBe` Right "1\n0\n1\n"
    it "reads back what it writes, keeping the table's origin and step" $ do
      let ty = table (TN.SNat @2) RealType
      t <- filled RealType [3, 2] (0, 0) (1, 1) (\[a, b] -> fromIntegral a / fromIntegral b)
      current <- filled RealType [1, 1] (-2, 2) (0.1, 0.1) (const 0)
      Right csv <- pure (exportTableCsv ty t)
      Right t' <- importTableCsv ty current (BL.toStrict csv)
      (tableDims t', tableOrigin t', tableStep t') `shouldBe` ([3, 2], (-2, 2), (0.1, 0.1))
      forM_ (cells [3, 2]) $ \ixs -> readCell RealType t' ixs `shouldBe` readCell RealType t ixs
    it "reads numbers as other programs write them" $ do
      current <- filled RealType [1] (0, 0) (1, 1) (const 0)
      Right t <- importTableCsv (table (TN.SNat @1) RealType) current
                   (BS8.pack "1, .5 ,-.25,+3,1e3,2.5E-1,inf,-Infinity,nan\r\n")
      tableDims t `shouldBe` [9]
      map (\j -> readCell RealType t [j]) [1 .. 8] `shouldBe`
        map Just [1, 0.5, -0.25, 3, 1000, 0.25, 1 / 0, -1 / 0]
      fmap isNaN (readCell RealType t [9]) `shouldBe` Just True
    it "says what's wrong with a CSV it can't read" $ do
      current <- filled IntegerType [1, 1] (0, 0) (1, 1) (const 0)
      let tryCsv :: BS.ByteString -> IO (Either String ())
          tryCsv s = fmap (const ()) <$> importTableCsv (table (TN.SNat @2) IntegerType) current s
      tryCsv "1,2\n3\n" >>= (`shouldBe` Left "This CSV can't be read: its lines don't all have the same number of values.")
      tryCsv "1,2\n3,x\n" >>= (`shouldBe` Left "This CSV can't be read: line 2, value 2: \"x\" isn't an integer.")
      tryCsv "1,2.5\n" >>= (`shouldBe` Left "This CSV can't be read: line 1, value 2: \"2.5\" isn't an integer.")
      tryCsv "\n\n" >>= (`shouldBe` Left "This CSV has no values in it.")
      currentC <- filled ComplexType [1, 1] (0, 0) (1, 1) (const 0)
      r <- importTableCsv (table (TN.SNat @2) ComplexType) currentC "1,2,3\n"
      fmap (const ()) r `shouldBe` Left "This CSV can't be read: line 1 has 3 values, which isn't a whole number of complex numbers (two values each)."
