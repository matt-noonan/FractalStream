-- | The GPU backends, without a GPU (CI has none: GPU_PLAN.md, G10): what
-- they compile and what they decline, the arguments' layout, and that a
-- render the GPU fails is drawn by the CPU instead. (Whether the shaders
-- compile and draw as the CPU does is checked by hand, in a browser:
-- dev-tools/web/check-gpu.mjs.)
--
-- With FS_WGSL_DUMP=folder, the shaders are written there.
module Backend.WGSLSpec (spec, shaders) where

import Test.Hspec

import Backend.Pure (pureBackend)
import Backend.WGSL
import Backend.WGSL.Code (argWords, packArgs)
import Data.Proxy (Proxy(..))
import Language.Environment (Context(..))
import Actor.Ensemble (buildEnsembleFromScript, runEnsemble)
import Actor.Layout (Dimensions(..))
import Actor.UI (UI(..))
import Actor.Viewer
import Data.Color (rgbToColor)
import Data.DynamicValue (setValue)
import FractalStream.Backends
import Language.Type

import Control.Monad (forM, forM_)
import Data.IORef
import Data.List (isInfixOf)
import GHC.Float (castWord32ToFloat, float2Double)
import qualified System.Environment as Env

-- | A device that keeps the shaders it's given, and fails every block.
keeping :: IORef [String] -> GpuDevice
keeping kept = GpuDevice { gdF64 = True, gdName = "", gdCompile = \source -> do
  modifyIORef kept (source :)
  pure (Right (GpuKernel (\_ _ -> pure (GpuArgs (\_ _ _ _ _ -> pure (Left "no GPU here")))))) }

-- | A script's viewer, with the platform's (pure) backend and the GPU's,
-- on this device.
withViewer :: Either String GpuDevice -> String -> (Viewer -> IO a) -> IO a
withViewer device script k = do
  ens <- buildEnsembleFromScript script >>= either (fail . snd) pure
  ref <- newIORef Nothing
  let ui = UI { newEnsemble = pure ()
              , makeLayout = \_ _ _ -> pure (pure ())
              , makeViewer = \_ _ _ _ _ v -> writeIORef ref (Just v) >> pure (pure ()) }
  withBackends pureBackend [gpuBackends allPrecisions (pure device)] $ \backend -> do
    runEnsemble backend ui ens
    readIORef ref >>= maybe (fail "no viewer") k

-- | What each GPU backend makes of a script's viewer: its shader, or why
-- it declined.
shaders :: String -> IO [(BackendId, Either String String)]
shaders script = do
  kept <- newIORef []
  withViewer (Right (keeping kept)) script $ \v -> do
    Renderer tries <- viewerRenderer v >>= either fail pure
    results <- forM [ t | t@(b, _) <- tries, b /= Cpu ] $ \(b, compileAndBind) -> (b,) <$> compileAndBind
    sources <- readIORef kept
    let precisionOf b = case b of
          GpuF64 -> "alias R = f64;"
          GpuDf64 -> "alias R = vec2f;"
          _ -> "alias R = f32;"
        sourceFor b = case filter (precisionOf b `isInfixOf`) sources of
          s : _ -> Right s
          [] -> Left "not compiled"
    Env.lookupEnv "FS_WGSL_DUMP" >>= mapM_ (\dir ->
      forM_ [ (b, s) | (b, _) <- results, Right s <- [sourceFor b] ] $ \(b, s) ->
        writeFile (dir ++ "/" ++ backendKey b ++ ".wgsl") s)
    pure [ (b, either (\(Decline why) -> Left why) (const (sourceFor b)) r) | (b, r) <- results ]

mandelbrot :: String
mandelbrot = unlines
  [ "@viewer \"M\" var=C center=-0.75 pixelSize=1/50"
  , "z := 0", "k := 0"
  , "while |z| < 10 and k < 100:", "  z <- z^2 + C", "  k <- k + 1"
  , "color <- rgb(k / 100, 0.5, 1 - k / 100)" ]

declines :: String -> String -> Expectation
declines script why = shaders script >>= \results -> do
  map fst results `shouldMatchList` [GpuF64, GpuDf64, GpuF32]
  forM_ results $ \(b, r) -> case r of
    Left said -> said `shouldSatisfy` (why `isInfixOf`)
    Right _ -> expectationFailure (backendKey b ++ " didn't decline it")

spec :: Spec
spec = describe "the GPU backends" $ do

  it "compile a viewer in double-float and in single precision" $ do
    results <- shaders mandelbrot
    map fst results `shouldMatchList` [GpuF64, GpuDf64, GpuF32]
    forM_ results $ \(_, r) -> r `shouldSatisfy` either (const False) ("fn main(" `isInfixOf`)

  it "compile the functions of real and complex numbers" $ do
    results <- shaders $ unlines
      [ "@viewer \"F\" var=z center=0 pixelSize=1/50"
      , "w := exp(z) + log(z) + sin(z) + cos(z) + tan(z) + sinh(z) + cosh(z) + tanh(z) + z^(1/3) + sqrt(z)"
      , "t := exp(re z) + log(|z|) + arg(w) + asin(0.5) + acos(0.5) + atan(re z)"
      , "u := asinh(re z) + acosh(2) + atanh(0.5) + mod(re z, 0.3) + (re z)^(im z)"
      , "color <- rgb(mod(t + u, 1), 0, 0)" ]
    forM_ results $ \(_, r) -> r `shouldSatisfy` either (const False) (const True)

  it "compile lists: literals, append, indexing, for each, and the rest" $ do
    results <- shaders $ unlines
      [ "@config \"C\""
      , "ws : List of C := [1, i, -1]"
      , "@viewer \"L\" var=z center=0 pixelSize=1/50"
      , "xs : List of C := [z, 2 z]"
      , "for each k in range(1, 5):"
      , "  xs <- append(xs, z^k)"
      , "for each w in ws:"
      , "  xs <- append(xs, w z)"
      , "ys := remove(xs, w -> |w| > 2)"
      , "n := length(join(xs, ys))"
      , "zs := transform(ys, w -> w / 2)"
      , "u := find(zs, w -> re w > 0, 0)"
      , "color <- if |xs @ 2| > 1 and n > 3 and |zs @@ 0| < 9 and |u| < 5 then white else black" ]
    map fst results `shouldMatchList` [GpuF64, GpuDf64, GpuF32]
    forM_ results $ \(_, r) -> r `shouldSatisfy` either (const False) ("arena" `isInfixOf`)

  it "compile tables: by index, by point, and their sizes" $ do
    results <- shaders $ unlines
      [ "@config \"C\""
      , "T : Table 2 of C"
      , "U : Table 3 of R x Z"
      , "@viewer \"T\" var=z center=0 pixelSize=1/50"
      , "w := T @ point z else 0"
      , "(r, k) := U @ (point z, 1) else (0, 0)"
      , "(nx, ny) := table_size(T)"
      , "(j, m) := table_cell(T, z)"
      , "q := table_point(T, (j, m))"
      , "vs := U @ (2, 3, *)"
      , "color <- if |w| > r + k + nx + ny + |q| + length(vs) then white else black" ]
    forM_ results $ \(_, r) -> r `shouldSatisfy` either (const False) ("rf64(" `isInfixOf`)

  it "compile autocolor, in each of its modes" $ do
    results <- shaders $ unlines
      [ "@viewer \"A\" var=z center=0 pixelSize=1/50"
      , "a := autocolor(0, z)"
      , "b := nearest_autocolor(1, z)"
      , "color <- blend(0.5, blend(0.5, get_autocolor(0, z), b), a)" ]
    map fst results `shouldMatchList` [GpuF64, GpuDf64, GpuF32]
    forM_ results $ \(_, r) -> do
      r `shouldSatisfy` either (const False) ("= ac_lookup(1u, 0, " `isInfixOf`)
      r `shouldSatisfy` either (const False) ("= ac_lookup(0u, 1, " `isInfixOf`)
      r `shouldSatisfy` either (const False) ("= ac_lookup(2u, 0, " `isInfixOf`)

  describe "decline" $ do
    it "text" $ declines (unlines
      [ "@viewer \"X\" var=z center=0 pixelSize=1/50"
      , "s := text(\"z = \", z)"
      , "color <- if length([s]) > 1 then white else black" ]) "not on the GPU yet: text"

    it "numbers past single precision's range (but for double precision)" $ do
      results <- shaders (unlines
        [ "@viewer \"N\" var=z center=0 pixelSize=1/50"
        , "color <- if |z| < 0.00000000000000000000000000000000000000000001 then white else black" ])
      forM_ results $ \(b, r) -> case (b, r) of
        (GpuF64, Right src) -> src `shouldSatisfy` ("rbits(0x" `isInfixOf`)
        (GpuF64, Left why) -> expectationFailure ("gpu-f64 declined it: " ++ why)
        (_, Left said) -> said `shouldSatisfy` ("numbers as large or as small as" `isInfixOf`)
        (_, Right _) -> expectationFailure (backendKey b ++ " didn't decline it")

    it "every viewer, without a GPU" $ withViewer (Left "No WebGPU here") mandelbrot $ \v -> do
      Renderer tries <- viewerRenderer v >>= either fail pure
      forM_ [ t | t@(b, _) <- tries, b /= Cpu ] $ \(_, compileAndBind) ->
        compileAndBind >>= either (`shouldBe` Decline "No WebGPU here") (const (expectationFailure "drawn"))

  it "give up a render the GPU fails, which the CPU draws instead" $ do
    kept <- newIORef []
    withViewer (Right (keeping kept)) mandelbrot $ \v -> do
      setValue (vSize v) (Dimensions (40, 30))
      img <- renderViewerBuffer False v >>= either fail pure
      rbReport img `shouldBe` RenderReport (Just Cpu) [(GpuF64, Decline "the GPU failed: no GPU here"), (GpuDf64, Decline "the GPU failed: no GPU here")]

  describe "lay out their arguments" $ do
    it "as a word for a truth value, an integer or a color" $ do
      argWords DoubleFloat BooleanType True `shouldBe` [1]
      argWords DoubleFloat IntegerType (-1) `shouldBe` [0xffffffff]
      argWords DoubleFloat ColorType (rgbToColor (1, 2, 3)) `shouldBe` [0x030201]
    it "as two f32s for a real number, their sum the double (to about 48 bits)" $ do
      argWords DoubleFloat RealType 1.5 `shouldBe` [0x3fc00000, 0]
      forM_ [0.1, -1 / 3, 1e10 + 1 / 7, 2.5e-20] $ \x -> case argWords DoubleFloat RealType x of
        [hi, lo] -> let y = float2Double (castWord32ToFloat hi) + float2Double (castWord32ToFloat lo)
                    in abs (y - x) `shouldSatisfy` (<= abs x * 2 ** (-47))
        ws -> expectationFailure (show ws)
    it "as the double's bits, low word first, for a real number in double precision" $ do
      argWords Double64 RealType 1.5 `shouldBe` [0, 0x3ff80000]
      argWords Double64 RealType (-0.1) `shouldBe` [0x9999999a, 0xbfb99999]
    it "as four f32s for a complex number" $
      length (argWords DoubleFloat ComplexType (1 :+ 2)) `shouldBe` 4
    it "as an address for a list, its length and elements after the slots" $ do
      let ctx = Bind (Proxy @"n") IntegerType 7 (Bind (Proxy @"xs") (ListType IntegerType) [3, 4] EmptyContext)
      fst (packArgs DoubleFloat ctx) `shouldBe` [7, 0x80000002, 2, 3, 4]
