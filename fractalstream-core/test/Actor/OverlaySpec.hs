module Actor.OverlaySpec (spec) where

import Test.Hspec

import Actor.Viewer.Overlay
import Language.Draw
import Language.Environment
import Data.Color (red)

import Codec.Picture (PixelRGBA8(..), imageWidth, imageHeight, pixelAt)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Data.Word (Word8)

spec :: Spec
spec = describe "compositeDrawLayers" $ do

  it "draws text centered on its point, in the stroke color" $ do
    painted <- paintedPixels [ SetStroke EmptyEnvProxy red
                             , Write EmptyEnvProxy "Hg ★" (0, 0) ]
    painted `shouldSatisfy` (not . null)
    -- everything painted is reddish, with no green or blue
    all (\(_, _, PixelRGBA8 _ g b _) -> g == 0 && b == 0) painted `shouldBe` True
    -- the text's extent is centered on the middle of the 120x40 image
    let xs = [ x | (x, _, _) <- painted ]
        ys = [ y | (_, y, _) <- painted ]
        center zs = fromIntegral (minimum zs + maximum zs) / (2 :: Double)
    center xs `shouldSatisfy` (\x -> abs (x - 60) <= 2)
    center ys `shouldSatisfy` (\y -> abs (y - 20) <= 3)
    -- and it's roughly the size of a 13 pixel font
    (maximum ys - minimum ys) `shouldSatisfy` (\h -> h >= 9 && h <= 16)

  it "skips shapes at non-finite points, rather than crashing" $ do
    let nan = 0 / 0 :: Double
        inf = 1 / 0 :: Double
    painted <- paintedPixels
      [ DrawPoint EmptyEnvProxy (nan, 0)
      , DrawLine EmptyEnvProxy (0, 0) (inf, 1)
      , DrawCircle EmptyEnvProxy True nan (0, 0)
      , DrawCircle EmptyEnvProxy False 3 (0, nan)
      , DrawRect EmptyEnvProxy True (0, 0) (nan, nan)
      , Write EmptyEnvProxy "x" (nan, nan)
      ]
    painted `shouldBe` []
    -- and finite shapes after them are still drawn
    paintedPixels [DrawPoint EmptyEnvProxy (nan, 0), DrawPoint EmptyEnvProxy (0, 0)]
      >>= (`shouldSatisfy` (not . null))

  it "draws nothing for the empty string" $
    paintedPixels [Write EmptyEnvProxy "" (0, 0)] >>= (`shouldBe` [])

-- | Composite one layer of draw commands onto a black 120x40 image, with
-- the model-space origin in the middle, and return the non-black pixels.
paintedPixels :: [DrawCommand] -> IO [(Int, Int, PixelRGBA8)]
paintedPixels cmds = allocaArray (120 * 40 * 3) $ \buf -> do
  pokeArray buf (replicate (120 * 40 * 3) (0 :: Word8))
  img <- compositeDrawLayers (0, 0) 1 [cmds] (120, 40) buf
  pure [ (x, y, p) | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]
                   , let p@(PixelRGBA8 r g b _) = pixelAt img x y
                   , (r, g, b) /= (0, 0, 0) ]
