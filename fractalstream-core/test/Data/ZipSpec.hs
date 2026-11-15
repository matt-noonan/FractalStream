{-# LANGUAGE OverloadedStrings #-}
module Data.ZipSpec (spec) where

import Test.Hspec

import Data.Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

spec :: Spec
spec = describe "Data.Zip" $ do
  let entries = [ ("project.fs", "script: |\n  pass\n")
                , ("tables/ℂ grid.bin", BL.pack (concat (replicate 1000 [0 .. 255])))
                , ("empty.bin", "") ]

  it "reads back what it writes, names and contents" $ do
    Right archive <- pure (writeZip entries)
    isZip (BL.toStrict archive) `shouldBe` True
    readZip (BL.toStrict archive) `shouldBe` Right entries

  it "writes the same bytes for the same entries" $
    writeZip entries `shouldBe` writeZip entries

  it "isn't fooled by what isn't an archive" $ do
    isZip "script: |\n" `shouldBe` False
    either (const True) (const False) (readZip "PK\3\4 not really") `shouldBe` True

  it "computes zlib's CRC-32" $
    crc32 (BLC.pack "The quick brown fox jumps over the lazy dog") `shouldBe` 0x414fa339

  it "reads a stored entry (as other tools may write)" $ do
    -- (an archive made by Python's zipfile, ZIP_STORED: "hi\n" in a.txt)
    let stored = BS.pack
          [ 0x50,0x4b,0x03,0x04,0x14,0,0,0,0,0,0,0,0x21,0,0x7a,0x7a,0x6f,0xed,3,0,0,0,3,0,0,0,5,0,0,0
          , 0x61,0x2e,0x74,0x78,0x74, 0x68,0x69,0x0a
          , 0x50,0x4b,0x01,0x02,0x14,0x03,0x14,0,0,0,0,0,0,0,0x21,0,0x7a,0x7a,0x6f,0xed,3,0,0,0,3,0,0,0
          , 5,0,0,0,0,0,0,0,0,0,0,0,0x80,0x01,0,0,0,0, 0x61,0x2e,0x74,0x78,0x74
          , 0x50,0x4b,0x05,0x06,0,0,0,0,1,0,1,0,0x33,0,0,0,0x26,0,0,0,0,0 ]
    readZip stored `shouldBe` Right [("a.txt", "hi\n")]
