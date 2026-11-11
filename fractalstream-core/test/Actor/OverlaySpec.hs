module Actor.OverlaySpec (spec) where

import Test.Hspec

import Actor.Viewer.Overlay
import Actor.Viewer.Sphere (ScreenView(..), Projection(..))
import Language.Draw
import Language.Environment
import Data.Color (red, blue)

import Codec.Picture (PixelRGBA8(..), imageWidth, imageHeight, pixelAt)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Data.Word (Word8)
import System.Timeout (timeout)
import Control.Monad (forM_)

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

  it "draws points in the stroke color, not the fill color" $ do
    painted <- paintedPixels [ SetStroke EmptyEnvProxy red
                             , SetFill EmptyEnvProxy blue
                             , DrawPoint EmptyEnvProxy (0, 0) ]
    painted `shouldSatisfy` (not . null)
    all (\(_, _, PixelRGBA8 _ g b _) -> g == 0 && b == 0) painted `shouldBe` True

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

  describe "draws shapes reaching far outside the image quickly, and just as far as the image goes" $ do
    let e = EmptyEnvProxy
        far = [1e7, 1e9, 1e20] :: [Double]
        -- the same pixels as a smaller shape that also reaches past the image
        sameAs cmds reference = do
          got <- quickly cmds
          want <- quickly reference
          got `shouldBe` want
          got `shouldSatisfy` (not . null)

    it "lines" $ forM_ far $ \d -> do
      [DrawLine e (0, 0) (d, 0)] `sameAs` [DrawLine e (0, 0) (200, 0)]
      [DrawLine e (0, 0) (0, -d)] `sameAs` [DrawLine e (0, 0) (0, -200)]
      [DrawLine e (-d, 5) (d, 5)] `sameAs` [DrawLine e (-200, 5) (200, 5)]
      -- (at an angle, the far end's direction can't be exact, but it's close)
      got <- quickly [DrawLine e (0, 0) (3 * d, d)]
      want <- quickly [DrawLine e (0, 0) (300, 100)]
      length got `shouldSatisfy` (\n -> abs (n - length want) <= 2)

    it "rectangles" $ forM_ far $ \d -> do
      [DrawRect e True (-d, -d) (d, 5)] `sameAs` [DrawRect e True (-200, -200) (200, 5)]
      [DrawRect e False (-d, -d) (d, 5)] `sameAs` [DrawRect e False (-200, -200) (200, 5)]
      -- (one around the whole image doesn't show)
      quickly [DrawRect e False (-d, -d) (d, d)] >>= (`shouldBe` [])

    it "circles" $ do
      forM_ far $ \d -> do
        -- one the image is inside: all of it, when filled
        quickly [DrawCircle e True d (0, 0)] >>= (`shouldSatisfy` ((== 120 * 40) . length))
        quickly [DrawCircle e False d (0, 0)] >>= (`shouldBe` [])
        -- one far away
        quickly [DrawCircle e True 1 (d, d)] >>= (`shouldBe` [])
        quickly [DrawCircle e False d (3 * d, 0)] >>= (`shouldBe` [])
      -- and one whose edge runs across the image, 5 pixels above the
      -- middle: nearly straight there. (Not 1e20 away: its center, 5 +
      -- 1e20, rounds to 1e20, so where its edge falls is lost before it's
      -- drawn.)
      forM_ (filter (< 1e10) far) $ \d -> do
        edge <- quickly [DrawCircle e False d (0, 5 + d)]
        [ y | (_, y, _) <- edge ] `shouldSatisfy` all (\y -> y >= 13 && y <= 17)
        length edge `shouldSatisfy` (>= 120 * 2)
        inside <- quickly [DrawCircle e True d (0, 5 + d)]
        [ y | (_, y, _) <- inside ] `shouldSatisfy` all (<= 16)
        length inside `shouldSatisfy` (\n -> n >= 120 * 15 && n <= 120 * 17)

    it "points and text" $ forM_ far $ \d ->
      quickly [DrawPoint e (d, d), Write e "x" (d, 0)] >>= (`shouldBe` [])

  it "draws nothing for the empty string" $
    paintedPixels [Write EmptyEnvProxy "" (0, 0)] >>= (`shouldBe` [])

-- | 'paintedPixels', failing if it takes more than five seconds.
quickly :: [DrawCommand] -> IO [(Int, Int, PixelRGBA8)]
quickly cmds = timeout 5000000 (paintedPixels cmds >>= \ps -> length ps `seq` pure ps)
  >>= maybe (fail "took more than five seconds") pure

-- | Composite one layer of draw commands onto a black 120x40 image, with
-- the model-space origin in the middle, and return the non-black pixels.
paintedPixels :: [DrawCommand] -> IO [(Int, Int, PixelRGBA8)]
paintedPixels cmds = allocaArray (120 * 40 * 3) $ \buf -> do
  pokeArray buf (replicate (120 * 40 * 3) (0 :: Word8))
  img <- compositeDrawLayers (ScreenView (0, 0) (1, 1) FlatProjection) [cmds] (120, 40) buf
  pure [ (x, y, p) | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1]
                   , let p@(PixelRGBA8 r g b _) = pixelAt img x y
                   , (r, g, b) /= (0, 0, 0) ]
