-- | A viewer's pixels come out the same however its image is cut into
-- blocks: each pixel's point is the image's origin plus its index times
-- the step ('vaOffset'), so blocks rendered separately (as a tiled,
-- progressive render does) match a render of the whole image, to the
-- last bit.
--
-- The viewer's color is the parity of x's last bit, so a point a single
-- ulp off shows. (As a control, blocks each stepped from their own first
-- point, as the kernels used to step, don't match.)
--
-- And so a progressive render ('renderProgressively': pieces, in coarse
-- passes, then at full resolution, then smoothed) comes out, once it's
-- done, just as 'renderViewerBuffer' renders the image, to the byte.
module BlocksSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), CodeWithArgs(..), ViewerArgs(..), ViewerFunction(..), RenderedBuffer(..),
                     renderViewerBuffer, renderProgressively, Progressive(..))
import Actor.Layout (Dimensions(..))
import Data.DynamicValue (setValue)
import Foreign.ForeignPtr (withForeignPtr)
import Backend (withBackend)
import Data.DynamicValue (getDynamic)

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Int (Int32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)

project :: String
project = unlines
  [ "@viewer \"V\" xvar=x yvar=y center=(1.0000001, 0) pixelSize=0.0000000000001"
  , "t := (x - 1) * 2251799813685248.0"   -- (half x's ulps past 1: 2^51)
  , "color <- if t - floor(t) < 0.25 then black else white"
  ]

width, height :: Int32
width = 64
height = 48

-- | Render the viewer's image as these blocks (column, row, width,
-- height), each given its first pixel's point by @pointOf@ and its
-- offset by @offsetOf@: the image's pixels, as RGB rows.
renderBlocks :: Viewer -> ((Int32, Int32) -> ((Double, Double), (Int32, Int32)))
             -> [(Int32, Int32, Int32, Int32)] -> IO BS.ByteString
renderBlocks viewer place blocks = case vCodeWithArgs viewer of
  CodeWithArgs getArgs _ code -> getArgs >>= \case
    Left err -> fail err
    Right args -> do
      ViewerFunction bind <- getDynamic code
      fn <- bind args
      image <- newIORef (BS.replicate (fromIntegral (3 * width * height)) 0)
      forM_ blocks $ \(c, r, w, h) ->
        allocaBytes (fromIntegral (3 * w * h)) $ \buf -> do
          let (point, offset) = place (c, r)
          _ <- fn ViewerArgs { vaPoint = point, vaOffset = offset, vaStep = (step, step)
                             , vaPixel = (step, step), vaWidth = w, vaHeight = h
                             , vaSubsamples = 1, vaBuffer = buf }
          block <- BS.packCStringLen (castPtr buf, fromIntegral (3 * w * h))
          forM_ [0 .. h - 1] $ \j -> modifyIORef' image $ \img ->
            let at = fromIntegral (3 * ((r + j) * width + c))
                line = BS.take (fromIntegral (3 * w)) (BS.drop (fromIntegral (3 * w * j)) block)
            in BS.take at img <> line <> BS.drop (at + fromIntegral (3 * w)) img
      readIORef image
  where step = 1e-13

origin :: (Double, Double)
origin = (1.0000001 - fromIntegral width / 2 * 1e-13, fromIntegral height / 2 * 1e-13)

withViewer :: (Viewer -> IO a) -> IO a
withViewer = withViewerOf project

withViewerOf :: String -> (Viewer -> IO a) -> IO a
withViewerOf script k = do
  ens <- buildEnsembleFromScript script >>= either (fail . snd) pure
  ref <- newIORef Nothing
  let ui = UI { newEnsemble = pure ()
              , makeLayout = \_ _ _ -> pure (pure ())
              , makeViewer = \_ _ _ _ _ v -> writeIORef ref (Just v) >> pure (pure ()) }
  withBackend $ \backend -> do
    runEnsemble backend ui ens
    readIORef ref >>= maybe (fail "no viewer") k

spec :: Spec
spec = describe "a viewer's image, cut into blocks" $ do
  -- (uneven blocks: the image cut at column 37 and row 23)
  let blocks = [ (0, 0, 37, 23), (37, 0, width - 37, 23)
               , (0, 23, 37, height - 23), (37, 23, width - 37, height - 23) ]
      fromOrigin _ = (origin, (0, 0))
      ownOffset (c, r) = (origin, (c, r))
      -- (the old way: each block from its own first point)
      ownPoint (c, r) = ((fst origin + fromIntegral c * 1e-13, snd origin - fromIntegral r * 1e-13), (0, 0))

  it "is the same, to the last bit, as rendered whole" $ withViewer $ \v -> do
    whole <- renderBlocks v fromOrigin [(0, 0, width, height)]
    tiled <- renderBlocks v ownOffset blocks
    BS.length (BS.filter (== 0) whole) `shouldSatisfy` (\n -> n > 0 && n < BS.length whole)
    tiled `shouldBe` whole

  it "(and the script would show a block stepped from its own first point)" $ withViewer $ \v -> do
    whole <- renderBlocks v fromOrigin [(0, 0, width, height)]
    stepped <- renderBlocks v ownPoint blocks
    stepped `shouldNotBe` whole

  describe "rendered progressively" $ do
    forM_ [False, True] $ \smooth -> forM_ [(64, "nothing"), (24, "nothing"), (24, "some pieces")] $ \(size, shown) ->
      it ("is, once done, the image " ++ (if smooth then "smoothed" else "unsmoothed")
          ++ ", in pieces of " ++ show size ++ ", with " ++ shown ++ " shown already") $
        forM_ [project, mandelbrot] $ \script -> withViewerOf script $ \v -> do
          -- (an odd size: pieces at the edges smaller, and some too small
          -- for the coarsest passes)
          setValue (vSize v) (Dimensions (150, 101))
          expected <- renderViewerBuffer smooth v >>= either fail pure >>= bytes
          drawn <- newIORef (0 :: Int)
          got <- renderProgressively Progressive
            { prPieceSize = size, prConcurrency = 3, prScale = 1, prSmooth = smooth
            , prShown = \c r -> shown /= "nothing" && even (c + r)
            , prStale = pure False, prDrawn = \_ _ -> modifyIORef' drawn (+ 1) } v
          image <- either fail (maybe (fail "abandoned") pure) got
          bytes image >>= (`shouldBe` expected)
          readIORef drawn >>= (`shouldSatisfy` (> 0))

    it "stops, once it's stale" $ withViewerOf mandelbrot $ \v -> do
      drawn <- newIORef (0 :: Int)
      got <- renderProgressively Progressive
        { prPieceSize = 16, prConcurrency = 1, prScale = 1, prSmooth = False, prShown = \_ _ -> False
        , prStale = (> 3) <$> readIORef drawn, prDrawn = \_ _ -> modifyIORef' drawn (+ 1) } v
      either fail (maybe (pure ()) (const (expectationFailure "it wasn't abandoned"))) got
      readIORef drawn >>= (`shouldBe` 4)
  where
    bytes RenderedBuffer{..} = withForeignPtr rbPixels $ \p ->
      BS.packCStringLen (castPtr p, 3 * rbWidth * rbHeight)

mandelbrot :: String
mandelbrot = unlines
  [ "@viewer \"M\" var=C center=-0.75 pixelSize=1/50"
  , "z := 0", "k := 0"
  , "while |z| < 10 and k < 100:", "  z <- z^2 + C", "  k <- k + 1"
  , "color <- rgb(k / 100, 0.5, 1 - k / 100)" ]
