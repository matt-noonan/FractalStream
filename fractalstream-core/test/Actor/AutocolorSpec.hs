{-# language OverloadedStrings, ForeignFunctionInterface, RecordWildCards #-}
module Actor.AutocolorSpec (spec) where

import Test.Hspec

import Actor.Autocolor
import Actor.Ensemble
import Data.Color
import Data.DynamicValue

import Control.Concurrent
import Data.Bits (shiftR)
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Foreign (FunPtr, Ptr, withForeignPtr)
import Foreign.C.Types (CDouble(..))
import Control.Monad (forM_, replicateM_)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

foreign import ccall "dynamic"
  callCacheLookup :: FunPtr (Ptr () -> Word8 -> Int32 -> CDouble -> CDouble -> IO Word32)
                  -> Ptr () -> Word8 -> Int32 -> CDouble -> CDouble -> IO Word32

tableSize :: AutocolorGroup -> IO Int
tableSize g = Map.size <$> getDynamic (agTable g)

spec :: Spec
spec = do
  describe "an autocolor group" $ do
    it "gives a point the same color each time, and nearby points the same color" $ do
      g <- makeEmptyAutocolorGroup
      c1 <- autocolor g 1 0
      c2 <- autocolor g 1.0005 0
      c3 <- autocolor g 1 0
      c2 `shouldBe` c1
      c3 `shouldBe` c1
      c4 <- autocolor g (-1) 0
      c4 `shouldNotBe` c1
      tableSize g >>= (`shouldBe` 2)

    it "matches the nearest point within the tolerance" $ do
      g <- makeEmptyAutocolorGroup
      setValue (agTolerance g) 1
      a <- autocolor g 0 0
      setValue (agTolerance g) 0.001
      b <- autocolor g 0.5 0
      setValue (agTolerance g) 1
      autocolor g 0.4 0 >>= (`shouldBe` b)
      autocolor g 0.1 0 >>= (`shouldBe` a)

    it "adds nothing when locked, giving its miss color instead" $ do
      g <- makeEmptyAutocolorGroup
      _ <- autocolor g 1 0
      st <- newAutocolorState
      setAutocolorLocked st g True
      autocolor g 5 5 >>= (`shouldBe` grey)
      setAutocolorMissColor st g red
      autocolor g 5 5 >>= (`shouldBe` red)
      tableSize g >>= (`shouldBe` 1)

    it "get_autocolor never adds a point" $ do
      g <- makeEmptyAutocolorGroup
      getAutocolor g 1 0 >>= (`shouldBe` grey)
      tableSize g >>= (`shouldBe` 0)

    it "stops adding points once full" $ do
      g <- makeEmptyAutocolorGroup
      forM_ [1 .. maxAutocolorEntries + 10] $ \k -> autocolor g (fromIntegral k) 0
      tableSize g >>= (`shouldBe` maxAutocolorEntries)

    it "adds a point only once, when many renderers miss it at the same time" $ do
      g <- makeEmptyAutocolorGroup
      done <- newEmptyMVar
      let n = 64 :: Int
      forM_ [1 .. n] $ \k -> forkIO $ do
        _ <- autocolor g (1 + 1e-7 * fromIntegral k) 0
        putMVar done ()
      replicateM_ n (takeMVar done)
      tableSize g >>= (`shouldBe` 1)

  describe "autocolor state" $ do
    it "is read back as it was written" $ do
      st <- newAutocolorState
      g <- autocolorGroup st (AutocolorGroupId 2)
      c <- autocolor g 1 0.5
      setAutocolorLocked st g True
      setAutocolorMissColor st g blue
      Just v <- writeAutocolorState st
      st' <- newAutocolorState
      readAutocolorState st' v >>= (`shouldBe` Right ())
      g' <- autocolorGroup st' (AutocolorGroupId 2)
      getDynamic (agLocked g') >>= (`shouldBe` True)
      getAutocolor g' 1 0.5 >>= (`shouldBe` c)
      getAutocolor g' 9 9 >>= (`shouldBe` blue)

    it "counts edits at once, and a burst of new points as one, shortly after" $ do
      st <- newAutocolorState
      g <- autocolorGroup st (AutocolorGroupId 0)
      e0 <- getDynamic (autocolorEdits st)
      forM_ [1 .. 5] $ \k -> autocolor g (fromIntegral (k :: Int)) 0
      getDynamic (autocolorEdits st) >>= (`shouldBe` e0)
      threadDelay 500000
      e1 <- getDynamic (autocolorEdits st)
      e1 `shouldBe` e0 + 1
      setAutocolorEntryColor st g (1, 0) red
      getDynamic (autocolorEdits st) >>= (`shouldSatisfy` (> e1))
      getAutocolor g 1 0 >>= (`shouldBe` red)

    it "answers compiled code's lookups from its snapshot" $ do
      st <- newAutocolorState
      g <- autocolorGroup st (AutocolorGroupId 3)
      c <- autocolor g 1 0
      let AutocolorLookup{..} = autocolorLookup st
          viaC add n x y = withForeignPtr autocolorCache $ \dir ->
            unpack <$> callCacheLookup autocolorCacheLookup dir (if add then 1 else 0) n x y
          unpack w = rgbToColor ( fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 8)
                                , fromIntegral w )
      -- a point it has (near enough), without adding one
      viaC True 3 1.0005 0 >>= (`shouldBe` c)
      tableSize g >>= (`shouldBe` 1)
      -- a point it doesn't have, in a locked group: its miss color
      setAutocolorLocked st g True
      setAutocolorMissColor st g blue
      viaC True 3 7 7 >>= (`shouldBe` blue)
      viaC False 3 7 7 >>= (`shouldBe` blue)
      tableSize g >>= (`shouldBe` 1)

  describe "a project's autocolor:" $ do
    it "is read from the project file and written back" $ do
      let file = BS8.pack $ unlines
            [ "script: |"
            , "  @viewer \"V\""
            , "  color <- autocolor(0, z)"
            , "autocolor:"
            , "  '0':"
            , "    tolerance: 0.01"
            , "    locked: true"
            , "    entries:"
            , "    - point: 1"
            , "      color: red"
            , "    - point: -0.5 + 0.25i"
            , "      color: rgb(0, 0, 1)"
            ]
      loadProjectBytes "a.fs" file >>= \case
        Right (Built ens) -> do
          st <- getDynamic (ensembleAutocolor ens)
          g <- autocolorGroup st (AutocolorGroupId 0)
          getAutocolor g 1.005 0 >>= (`shouldBe` red)
          getAutocolor g (-0.5) 0.25 >>= (`shouldBe` blue)
          bytes <- BS8.unpack <$> encodeEnsemble ens
          bytes `shouldSatisfy` isInfixOf "autocolor:"
          bytes `shouldSatisfy` isInfixOf "locked: true"
          bytes `shouldSatisfy` isInfixOf "point: -0.5 + 0.25i"
          bytes `shouldSatisfy` isInfixOf "color: rgb(1.000, 0.000, 0.000)"
        _ -> expectationFailure "expected the project to build"

    it "only takes a group number written as a number" $
      checkScript "@viewer \"V\"\nn := 1\ncolor <- autocolor(n, z)\n" `shouldSatisfy` \case
        Left (_, err) -> "has to be a plain number" `isInfixOf` err
        Right () -> False

  describe "showAutocolorPoint" $
    it "writes points as complex literals, in ASCII, rounded" $ do
      showAutocolorPoint (1.1673039779804448, -8.26250045673913e-11) `shouldBe` "1.167303978"
      showAutocolorPoint (0.1812324444592382, -1.0839541013127065) `shouldBe` "0.1812324445 - 1.0839541013i"
      showAutocolorPoint (0, 2) `shouldBe` "2i"
      showAutocolorPoint (-1, 0.5) `shouldBe` "-1 + 0.5i"
