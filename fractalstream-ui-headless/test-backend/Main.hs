{-# LANGUAGE CPP #-}
-- | Tests that run projects end to end, so they link a backend. They're
-- kept apart from the @spec@ suite, whose quasiquotes would make GHC load
-- the backend's LLVM libraries at compile time (see the LLVM backend's
-- spec).
module Main (main) where

import Test.Hspec

import FractalStream.Console (transliterateConsole)
import qualified GeometrySpec
import qualified TextSpec
import qualified OverlaySpec
#if !defined(wasm32_HOST_ARCH)
import qualified TableSpec
#endif
import qualified HoistSpec

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = transliterateConsole >> hspec (OverlaySpec.spec >> GeometrySpec.spec >> TextSpec.spec >> tables >> HoistSpec.spec)
  where
#if defined(wasm32_HOST_ARCH)
    -- (not yet on WebAssembly: the wasm backend doesn't compile tables, so
    -- their viewers are interpreted, which takes far too long at full size)
    tables = pure ()
#else
    tables = TableSpec.spec
#endif
