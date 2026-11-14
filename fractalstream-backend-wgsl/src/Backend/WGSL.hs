-- | The GPU backends (GPU_PLAN.md): viewers compiled to WGSL
-- ("Backend.WGSL.Code"), in double-float or single precision, and drawn
-- on a GPU device ("Backend.WGSL.Device"), which the platform supplies.
--
-- A backend declines a viewer it can't draw yet (lists, text, tables,
-- autocolor; in double-float, the functions it hasn't yet), one the GPU
-- won't compile, and every viewer when there's no GPU. A block the GPU
-- fails to draw (the device was lost, say) gives up the render, which the
-- next backend draws instead. Its renders skip the coarse passes
-- ('bfCoarse'): it's fast enough to start at full resolution.
module Backend.WGSL
  ( gpuViewerCompilers
  , viewerSource
  , module Backend.WGSL.Device
  , Precision(..)
  ) where

import FractalStream.Prelude

import Backend.WGSL.Code
import Backend.WGSL.Device
import Backend.WGSL.Runtime (Precision(..))

import Actor.Viewer (ViewerCompiler(..), CompiledViewer(..), BlockFunction(..), ViewerArgs(..))
import Actor.Viewer.Types (ViewerEnv, MissingViewerArgs)
import FractalStream.Backends (BackendId(..), Decline(..))
import Language.Code (Code, transformValues)
import Language.Environment
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform (shortCircuit, integerPowers)

import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)

-- | The GPU backends, on this device (or, if there's none, declining
-- every viewer, saying why).
gpuViewerCompilers :: Either String GpuDevice -> [(BackendId, ViewerCompiler)]
gpuViewerCompilers device = [(GpuDf64, compiler DoubleFloat), (GpuF32, compiler Single)]
  where
    compiler p = ViewerCompiler $ \_ code -> case device of
      Left why -> pure (Left (Decline why))
      Right dev -> case viewerSource p code of
        Left why -> pure (Left (Decline why))
        Right source -> gdCompile dev source <&> \case
          Left err -> Left (Decline ("the GPU couldn't compile it: " ++ err))
          Right kernel -> Right (CompiledViewer (bind kernel))

    bind :: GpuKernel -> Context HaskellValue env -> IO BlockFunction
    bind kernel args = do
      let ws = concat (fromContext (\_ t v -> argWords t v) args)
      -- (a binding can't be empty)
      gpuArgs <- gkBind kernel (if null ws then [0] else ws)
      pure BlockFunction
        { bfView = const Nothing
        , bfCoarse = False
        , bfBlock = \ViewerArgs{..} -> do
            let params = blockParams (vaWidth, vaHeight) vaSubsamples vaOffset vaPoint vaStep vaPixel
            result <- withArray params $ \p ->
              gaRun gpuArgs (castPtr p) (fromIntegral vaWidth) (fromIntegral vaHeight) vaBuffer
            pure $ case result of
              Left err -> Left (Decline ("the GPU failed: " ++ err))
              Right () -> Right mempty
        }

-- | A viewer's shader, in this precision, or why there can't be one.
-- It's rewritten as the CPU's is ('Language.Value.Transform.backendRewrites'),
-- but for 'Language.Value.Transform.avoidSqrt': comparing squares instead
-- of absolute values overflows f32's range, as the escape radius 1e24
-- would (squared), so the generator compares absolute values itself.
viewerSource :: (MissingViewerArgs env, KnownEnvironment env)
             => Precision -> Code (ViewerEnv env) -> Either String String
viewerSource p = kernelSource p . transformValues (shortCircuit . integerPowers)
