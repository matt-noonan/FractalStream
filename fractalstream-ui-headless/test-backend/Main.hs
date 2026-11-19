{-# LANGUAGE CPP #-}
-- | Tests that run projects end to end, so they link a backend. They're
-- kept apart from the @spec@ suite, whose quasiquotes would make GHC load
-- the backend's LLVM libraries at compile time (see the LLVM backend's
-- spec).
module Main (main) where

import Test.Hspec

import FractalStream.Console (transliterateConsole)
import FractalStream.Backends (BackendId(..), backendPreference)
import Data.DynamicValue (setValue')
import qualified DragSpec
import qualified GeometrySpec
import qualified DiskSpec
import qualified GroupSpec
import qualified PixelSizeSpec
import qualified BlocksSpec
import qualified ChainSpec
import qualified TextSpec
import qualified ToolAutocolorSpec
import qualified OverlaySpec
import qualified TableSpec
import qualified HoistSpec
import qualified SphereDrawSpec

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = do
  transliterateConsole
  -- (on the CPU, as fsrun draws by default: these check how the CPU
  -- draws, coarse passes and all; ChainSpec sets its own orders)
  setValue' backendPreference [Cpu]
  hspec (OverlaySpec.spec >> GeometrySpec.spec >> DiskSpec.spec >> GroupSpec.spec >> PixelSizeSpec.spec >> BlocksSpec.spec >> ChainSpec.spec >> DragSpec.spec >> TextSpec.spec >> ToolAutocolorSpec.spec >> TableSpec.spec >> HoistSpec.spec >> SphereDrawSpec.spec)
