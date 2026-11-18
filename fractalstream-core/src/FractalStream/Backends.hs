-- | The backends a viewer can be drawn with, and the order to try them in
-- (GPU_PLAN.md, G1, G2 and G8; WEB_PLAN.md, D20).
--
-- Every viewer is drawn by the first backend, in the viewer's order, that
-- accepts its script and its view, and doesn't give up on the render. A
-- backend declines what it can't compute as well as doubles would, so
-- the order trades speed against nothing but the precision of the
-- backends after the CPU. The app has one order (a preference, saved with
-- FractalStream's settings), which a viewer can override, by putting a
-- backend first ('effectiveOrder').
module FractalStream.Backends
  ( BackendId(..)
  , allBackends
  , defaultBackendOrder
  , backendKey
  , backendFromKey
  , backendLabel
  , normalizeOrder
  , effectiveOrder
  , backendPreference
  , Decline(..)
  , RenderReport(..)
  , describeRenderReport
  , describeDeclines
  ) where

import Data.DynamicValue (Variable, newVariable)
import Data.List (intercalate, nub, (\\))
import Data.Maybe (isJust, isNothing)
import System.IO.Unsafe (unsafePerformIO)

-- | A way of drawing a viewer.
data BackendId
  = GpuF64
    -- ^ on the GPU, in doubles (where it has them: wgpu's native-only
    --   SHADER_F64, on Vulkan and DirectX 12; not Metal, or the web)
  | GpuDf64
    -- ^ on the GPU, in double-float (pairs of f32s: about 48 bits, with
    --   f32's range)
  | Cpu
    -- ^ on the CPU, in doubles (LLVM on the desktop, WebAssembly on the web)
  | GpuF32
    -- ^ on the GPU, in single precision
  deriving (Eq, Ord, Show, Enum, Bounded)

allBackends :: [BackendId]
allBackends = [minBound .. maxBound]

-- | Precision first, then speed: what a fresh install uses.
defaultBackendOrder :: [BackendId]
defaultBackendOrder = [GpuF64, GpuDf64, Cpu, GpuF32]

-- | How settings name a backend.
backendKey :: BackendId -> String
backendKey = \case
  GpuF64  -> "gpu-f64"
  GpuDf64 -> "gpu-df64"
  Cpu     -> "cpu"
  GpuF32  -> "gpu-f32"

backendFromKey :: String -> Maybe BackendId
backendFromKey k = lookup k [ (backendKey b, b) | b <- allBackends ]

-- | How the user sees a backend's name.
backendLabel :: BackendId -> String
backendLabel = \case
  GpuF64  -> "GPU, double precision"
  GpuDf64 -> "GPU, double-float"
  Cpu     -> "CPU"
  GpuF32  -> "GPU, single precision"

-- | An order with every backend in it once: as given, without repeats,
-- and then any it left out, in the default order; but a backend newer
-- than orders saved without it goes where it would have been
-- ('placedBefore'): GPU f64 just before GPU df64.
normalizeOrder :: [BackendId] -> [BackendId]
normalizeOrder bs =
  let bs' = nub bs
      missing = defaultBackendOrder \\ bs'
      appended = bs' ++ [ b | b <- missing, isNothing (placedBefore b) ]
      place order b = case placedBefore b of
        Just next | next `elem` order -> let (before, after) = break (== next) order in before ++ b : after
        _ -> order ++ [b]
  in foldl place appended [ b | b <- missing, isJust (placedBefore b) ]

-- | Where a backend added since orders were first saved goes in one saved
-- without it: before this one.
placedBefore :: BackendId -> Maybe BackendId
placedBefore = \case
  GpuF64 -> Just GpuDf64
  _ -> Nothing

-- | A viewer's order: the app's, with the backend the viewer was told to
-- prefer (if any) moved to the front.
effectiveOrder :: [BackendId] -> Maybe BackendId -> [BackendId]
effectiveOrder order = \case
  Nothing -> normalizeOrder order
  Just b  -> b : filter (/= b) (normalizeOrder order)

-- | The app's order, as its preferences say (the default until a
-- frontend sets it from them).
backendPreference :: Variable [BackendId]
backendPreference = unsafePerformIO (newVariable defaultBackendOrder)
{-# NOINLINE backendPreference #-}

-- | Why a backend won't draw a viewer, a view of it, or a render.
newtype Decline = Decline String
  deriving (Eq, Show)

-- | Which backend drew a render, and why the ones before it didn't
-- (those the platform has: one it lacks isn't tried, or mentioned).
data RenderReport = RenderReport
  { rrBackend  :: Maybe BackendId
    -- ^ Nothing: none of them would
  , rrDeclined :: [(BackendId, Decline)]
  } deriving (Eq, Show)

-- | A message for the user when no backend would draw a viewer.
describeRenderReport :: RenderReport -> Maybe String
describeRenderReport RenderReport{..} = case rrBackend of
  Just _  -> Nothing
  Nothing -> Just ("No backend could draw this viewer: " ++ describeDeclines rrDeclined ++ ".")

-- | Why each backend declined, in a sentence.
describeDeclines :: [(BackendId, Decline)] -> String
describeDeclines [] = "none is available"
describeDeclines ds = intercalate "; " [ backendLabel b ++ ": " ++ why | (b, Decline why) <- ds ]
