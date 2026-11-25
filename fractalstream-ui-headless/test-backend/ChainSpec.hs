-- | Viewers are drawn by the first backend in their order that will
-- (GPU_PLAN.md, "Declining"): one that declines the script, the view, or
-- the render (giving up part way) passes it to the next, which draws it
-- from scratch, as it would have alone; a viewer can put a backend ahead
-- of the app's order; and when none will, the render says why.
--
-- The platform's CPU backend is the real one; the others are made up
-- here, from it, to decline in each way (or to draw red).
module ChainSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer
import Actor.Layout (Dimensions(..))
import Backends (withBackend)
import Data.DynamicValue (getDynamic, setValue)
import FractalStream.Backends

import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import Foreign.Storable (pokeByteOff)
import Data.Word (Word8)

mandelbrot :: String
mandelbrot = unlines
  [ "@viewer \"M\" over C\nsetup:\n  center: -0.75\n  pixel size: 1/50"
  , "z := 0", "k := 0"
  , "while |z| < 10 and k < 100:", "  z <- z^2 + C", "  k <- k + 1"
  , "color <- rgb(k / 100, 0.5, 1 - k / 100)" ]

-- | A backend made from another by changing its block functions.
mapBlocks :: (BlockFunction -> BlockFunction) -> ViewerCompiler -> ViewerCompiler
mapBlocks f compiler = ViewerCompiler $ \autocolor code ->
  fmap (\(CompiledViewer bind) -> CompiledViewer (fmap f . bind)) <$> compileViewer compiler autocolor code

declinesScript, declinesView, givesUp, drawsRed :: ViewerCompiler -> ViewerCompiler
declinesScript _ = ViewerCompiler (\_ _ -> pure (Left (Decline "can't compile it")))
declinesView = mapBlocks (\bf -> bf { bfView = const (Just (Decline "too deep")) })
-- (after scribbling on its block: what it drew must not survive)
givesUp = mapBlocks $ \bf -> bf { bfBlock = \args -> do
  fill args (0x33, 0x33, 0x33)
  pure (Left (Decline "overflowed")) }
drawsRed = mapBlocks $ \bf -> bf { bfBlock = \args -> Right mempty <$ fill args (0xff, 0, 0) }

fill :: ViewerArgs -> (Word8, Word8, Word8) -> IO ()
fill ViewerArgs{..} (r, g, b) =
  forM_ [0 .. fromIntegral (vaWidth * vaHeight) - 1] $ \k -> do
    pokeByteOff vaBuffer (3 * k) r
    pokeByteOff vaBuffer (3 * k + 1) g
    pokeByteOff vaBuffer (3 * k + 2) b

-- | The viewer, on the platform's CPU backend and these made-up ones (made
-- from it), or on just the made-up ones. (Not the platform's GPU backends:
-- what they'd say, the GPU's own, would be in the reports.)
withChain :: Bool -> [(BackendId, ViewerCompiler -> ViewerCompiler)] -> (Viewer -> IO a) -> IO a
withChain keepCpu fakes k = do
  ens <- buildEnsembleFromScript mandelbrot >>= either (fail . snd) pure
  ref <- newIORef Nothing
  let ui = UI { newEnsemble = pure ()
              , makeLayout = \_ _ _ -> pure (pure ())
              , makeViewer = \_ _ _ _ _ v -> writeIORef ref (Just v) >> pure (pure ()) }
  withBackend $ \backend -> do
    let cpu = fromMaybe (error "no CPU backend") (lookup Cpu (bViewerCompilers backend))
        backend' = backend { bViewerCompilers = [ (b, f cpu) | (b, f) <- fakes ]
                                                ++ (if keepCpu then [ c | c@(Cpu, _) <- bViewerCompilers backend ] else []) }
    runEnsemble backend' ui ens
    v <- readIORef ref >>= maybe (fail "no viewer") pure
    setValue (vSize v) (Dimensions (90, 61))
    k v

-- | With the app's order set to this, for the test.
withOrder :: [BackendId] -> IO a -> IO a
withOrder order act = bracket (getDynamic backendPreference) (setValue backendPreference)
                              (\_ -> setValue backendPreference order >> act)

bytes :: RenderedBuffer -> IO BS.ByteString
bytes RenderedBuffer{..} = withForeignPtr rbPixels $ \p ->
  BS.packCStringLen (castPtr p, 3 * rbWidth * rbHeight)

-- | The viewer's image as the CPU alone draws it.
onTheCpu :: Viewer -> Bool -> IO BS.ByteString
onTheCpu v smooth = do
  setValue (vBackendOverride v) (Just Cpu)
  img <- renderViewerBuffer smooth v >>= either fail pure
  rbReport img `shouldBe` RenderReport (Just Cpu) []
  setValue (vBackendOverride v) Nothing
  bytes img

progressively :: Bool -> Viewer -> IO (Either String (Maybe RenderedBuffer))
progressively smooth = renderProgressively Progressive
  { prPieceSize = 24, prConcurrency = 3, prScale = 1, prSmooth = smooth
  , prShown = \_ _ -> False, prStale = pure False, prStarted = \_ -> pure ()
  , prCovered = pure (), prDrawn = \_ -> pure (), prBackend = \_ -> pure () }

spec :: Spec
spec = describe "a viewer's backends" $ do

  forM_ [ ("its script", declinesScript, "can't compile it")
        , ("the view", declinesView, "too deep")
        , ("the render, part way", givesUp, "overflowed") ] $ \(what, fake, why) ->
    describe ("when the first declines " ++ what) $ forM_ [False, True] $ \smooth -> do
      let declined = RenderReport (Just Cpu) [(GpuDf64, Decline why)]
          smoothed = if smooth then " (smoothed)" else ""
      it ("draws it as the next would alone" ++ smoothed) $
        withOrder defaultBackendOrder $ withChain True [(GpuDf64, fake), (GpuF32, drawsRed)] $ \v -> do
          expected <- onTheCpu v smooth
          img <- renderViewerBuffer smooth v >>= either fail pure
          rbReport img `shouldBe` declined
          bytes img >>= (`shouldBe` expected)
      it ("draws it so progressively too" ++ smoothed) $
        withOrder defaultBackendOrder $ withChain True [(GpuDf64, fake), (GpuF32, drawsRed)] $ \v -> do
          expected <- onTheCpu v smooth
          img <- progressively smooth v >>= either fail (maybe (fail "abandoned") pure)
          rbReport img `shouldBe` declined
          bytes img >>= (`shouldBe` expected)

  it "are tried in the app's order" $
    withOrder [GpuF32, Cpu, GpuDf64] $ withChain True [(GpuF32, drawsRed)] $ \v -> do
      img <- renderViewerBuffer False v >>= either fail pure
      rbReport img `shouldBe` RenderReport (Just GpuF32) []
      bytes img >>= (`shouldSatisfy` all (`elem` [0, 0xff]) . BS.unpack)

  it "start with the one a viewer prefers, if it prefers one" $
    withOrder defaultBackendOrder $ withChain True [(GpuDf64, declinesView), (GpuF32, drawsRed)] $ \v -> do
      setValue (vBackendOverride v) (Just GpuF32)
      img <- renderViewerBuffer False v >>= either fail pure
      rbReport img `shouldBe` RenderReport (Just GpuF32) []
      BS.take 3 <$> bytes img >>= (`shouldBe` BS.pack [0xff, 0, 0])

  it "say why, when none will draw it" $
    withOrder defaultBackendOrder $ withChain False [(GpuDf64, declinesScript), (GpuF32, declinesView)] $ \v -> do
      let says r = r `shouldSatisfy` either (\e -> all (`isInfixOf` e) ["No backend could draw", "can't compile it", "too deep"]) (const False)
      renderViewerBuffer False v >>= says . fmap (const ())
      progressively False v >>= says . fmap (const ())

  describe "their order" $ do
    it "has every backend once, those left out after, in the default order" $ do
      normalizeOrder [Cpu, Cpu] `shouldBe` [Cpu, GpuF64, GpuDf64, GpuF32]
      normalizeOrder [] `shouldBe` defaultBackendOrder
    it "puts a viewer's preferred backend first" $
      effectiveOrder defaultBackendOrder (Just GpuF32) `shouldBe` [GpuF32, GpuF64, GpuDf64, Cpu]
    it "names backends as settings do" $
      map backendKey allBackends `shouldBe` ["gpu-f64", "gpu-df64", "cpu", "gpu-f32"]
