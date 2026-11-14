{-# LANGUAGE CPP #-}
-- | Tests that run projects end to end, so they link a backend. They're
-- kept apart from the @spec@ suite, whose quasiquotes would make GHC load
-- the backend's LLVM libraries at compile time (see the LLVM backend's
-- spec).
module Main (main) where

import Test.Hspec

import FractalStream.Console (transliterateConsole)
import qualified DragSpec
import qualified GeometrySpec
import qualified DiskSpec
import qualified PixelSizeSpec
import qualified BlocksSpec
import qualified TextSpec
import qualified ToolAutocolorSpec
import qualified OverlaySpec
import qualified TableSpec
import qualified HoistSpec

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = transliterateConsole >> hspec (OverlaySpec.spec >> GeometrySpec.spec >> DiskSpec.spec >> PixelSizeSpec.spec >> BlocksSpec.spec >> DragSpec.spec >> TextSpec.spec >> ToolAutocolorSpec.spec >> TableSpec.spec >> HoistSpec.spec)
