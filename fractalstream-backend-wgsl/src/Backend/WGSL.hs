-- | The GPU backends (GPU_PLAN.md): viewers compiled to WGSL
-- ("Backend.WGSL.Code"), in double, double-float or single precision, and drawn
-- on a GPU device ("Backend.WGSL.Device"), which the platform supplies.
--
-- A backend declines a viewer it can't draw (text; in double-float, the
-- functions it hasn't yet), one the GPU won't compile, and every viewer
-- when there's no GPU. A block the GPU
-- fails to draw (the device was lost, say) gives up the render, which the
-- next backend draws instead. Its renders skip the coarse passes
-- ('bfCoarse'): it's fast enough to start at full resolution. A frontend
-- built with them includes 'gpuBackends', on its platform's device, in
-- its backends ('Actor.Viewer.withBackends').
module Backend.WGSL
  ( gpuBackends
  , allPrecisions
  , gpuViewerCompilers
  , viewerSource
  , module Backend.WGSL.Device
  , Precision(..)
  ) where

import FractalStream.Prelude

import Backend.WGSL.Code
import Backend.WGSL.Device
import Backend.WGSL.Runtime (Precision(..))

import Actor.Viewer (ViewerCompiler(..), CompiledViewer(..), BlockFunction(..), ViewerArgs(..), ViewerIssues(..), ViewerBackends(..))
import Actor.Viewer.Types (ViewerEnv, MissingViewerArgs)
import FractalStream.Backends (BackendId(..), Decline(..))
import Language.Code (Code, transformValues)
import Language.Environment
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform (shortCircuit, integerPowers)

import Actor.Autocolor (AutocolorLookup(..), autocolorPalette)
import Language.Type (AutocolorMode(..), autocolorModeCode)
import Data.Bits (testBit)
import Data.List (isInfixOf)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (peekByteOff)
import GHC.Float (float2Double)
import Language.Table (Table(..))
import Foreign.Ptr (Ptr, castPtr)
import Control.Concurrent.MVar (newMVar, modifyMVar)

-- | The GPU backends in these precisions (the platform's: the web's
-- WebGPU has no doubles), on the device this starts (the platform's: the
-- web's WebGPU, or the desktop's wgpu-native), or that says why there's
-- none. It's started once, when a GPU backend first compiles a viewer: a
-- session drawn on the CPU alone never touches the GPU.
gpuBackends :: [Precision] -> IO (Either String GpuDevice) -> ViewerBackends
gpuBackends ps start = ViewerBackends $ do
  started <- newMVar Nothing
  let device = modifyMVar started $ \case
        Just d -> pure (Just d, d)
        Nothing -> start >>= \d -> pure (Just d, d)
  pure (gpuViewerCompilers ps device)

-- | Every precision: a GPU backend each.
allPrecisions :: [Precision]
allPrecisions = [Double64, DoubleFloat, Single]

-- | The GPU backends' viewer compilers, on the device this gives (or,
-- if there's none, declining every viewer, saying why).
gpuViewerCompilers :: [Precision] -> IO (Either String GpuDevice) -> [(BackendId, ViewerCompiler)]
gpuViewerCompilers ps getDevice = [ (backendOf p, compiler p) | p <- ps ]
  where
    backendOf = \case
      Double64 -> GpuF64
      DoubleFloat -> GpuDf64
      Single -> GpuF32
    -- (what the GPU can't compile is declined without starting it)
    compiler p = ViewerCompiler $ \colors code -> case viewerSource p code of
      Left why -> pure (Left (Decline why))
      Right source -> getDevice >>= \case
        Left why -> pure (Left (Decline why))
        Right dev
          | p == Double64 && not (gdF64 dev) ->
              pure (Left (Decline ("this GPU" ++ (if null (gdName dev) then "" else " (" ++ gdName dev ++ ")")
                                   ++ " has no doubles (wgpu's SHADER_F64: Vulkan and DirectX 12 may, Metal and the web don't)")))
          | otherwise -> gdCompile dev source <&> \case
              Left err -> Left (Decline ("the GPU couldn't compile it: " ++ err))
              Right kernel -> Right (CompiledViewer (bind p (usesAutocolor source) colors kernel))

    bind :: Precision -> Bool -> AutocolorLookup -> GpuKernel -> Context HaskellValue env -> IO BlockFunction
    bind p withColors colors kernel args = do
      let (ws, tables) = packArgs p args
      -- (a binding can't be empty)
      gpuArgs <- gkBind kernel (if null ws then [0] else ws)
        [ GpuTable (tableVersion t) off (tableBuffer t) bytes | (t, off, bytes) <- tables ]
      pure BlockFunction
        { bfView = const Nothing
        , bfCoarse = False
        , bfBlock = \ViewerArgs{..} -> do
            let params = blockParams p (vaWidth, vaHeight) vaSubsamples vaOffset vaPoint vaStep vaPixel
                run ac = withArray params $ \paramsp ->
                  gaRun gpuArgs (castPtr paramsp) (fromIntegral vaWidth) (fromIntegral vaHeight) vaBuffer ac
            result <- if not withColors then run Nothing else allocaBytes missesBytes $ \misses -> do
              -- A block that met points to add (autocolor) is drawn
              -- again, once they're added, until it meets none: as the
              -- wasm backend does (and so as the LLVM backend's comes
              -- out, which adds them as it goes).
              let go :: Int -> IO (Either String Word32)
                  go tries = do
                    (palette, n) <- autocolorPalette colors
                    r <- withForeignPtr palette $ \pal -> run (Just (GpuAutocolor pal n misses))
                    missed <- either (const (pure [])) (const (readMisses misses)) r
                    if null missed || tries >= maxRedraws then pure r else do
                      forM_ missed $ \(mode, g, x, y) -> autocolorFunction colors mode g x y
                      go (tries + 1)
              go 0
            pure $ case result of
              Left err -> Left (Decline ("the GPU failed: " ++ err))
              Right issues -> Right ViewerIssues { viOutOfMemory = testBit issues 0
                                                 , viBadIndex = testBit issues 1 }
        }

-- | How many times a block is drawn again for the points it noted to add,
-- at most (as the wasm backend's).
maxRedraws :: Int
maxRedraws = 8

-- | Whether a viewer's shader looks autocolors up (so its blocks need the
-- groups, and to be drawn again for points they add): whether its code
-- calls @ac_lookup@, which every shader defines.
usesAutocolor :: String -> Bool
usesAutocolor = isInfixOf " = ac_lookup("

-- | The points a block noted to add (see 'GpuAutocolor'): each one's
-- mode, group, and point.
readMisses :: Ptr Word8 -> IO [(AutocolorMode, Int, Double, Double)]
readMisses p = do
  count <- peekByteOff p 0 :: IO Word32
  forM [0 .. min missCapacity (fromIntegral count) - 1] $ \k -> do
    let e = 8 + 24 * k
        real o = do
          hi <- peekByteOff p (e + o) :: IO Float
          lo <- peekByteOff p (e + o + 4) :: IO Float
          pure (float2Double hi + float2Double lo)
    group <- peekByteOff p e :: IO Int32
    code <- peekByteOff p (e + 4) :: IO Word32
    x <- real 8
    y <- real 16
    let mode = fromMaybe AddPoint (lookup (fromIntegral code)
                 [ (autocolorModeCode m, m) | m <- [minBound .. maxBound] ])
    pure (mode, fromIntegral group, x, y)

-- | A viewer's shader, in this precision, or why there can't be one.
-- It's rewritten as the CPU's is ('Language.Value.Transform.backendRewrites'),
-- but for 'Language.Value.Transform.avoidSqrt': comparing squares instead
-- of absolute values overflows f32's range, as the escape radius 1e24
-- would (squared), so the generator compares absolute values itself.
viewerSource :: (MissingViewerArgs env, KnownEnvironment env)
             => Precision -> Code (ViewerEnv env) -> Either String String
viewerSource p = kernelSource p . transformValues (shortCircuit . integerPowers)
