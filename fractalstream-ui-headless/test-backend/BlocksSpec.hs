-- | A viewer's pixels come out the same however its image is cut into
-- blocks: each pixel's point is the image's origin plus its index times
-- the step ('vaOffset'), so blocks rendered separately (as a tiled,
-- progressive render does) match a render of the whole image, to the
-- last bit.
--
-- The viewer's color is the parity of x's last bit, so a point a single
-- ulp off shows. (As a control, blocks each stepped from their own first
-- point, as the kernels used to step, don't match.)
module BlocksSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), CodeWithArgs(..), ViewerArgs(..), ViewerFunction(..))
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
withViewer k = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
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
