-- | Tests that run projects end to end, so they link a backend. They're
-- kept apart from the @spec@ suite, whose quasiquotes would make GHC load
-- the backend's LLVM libraries at compile time (see the LLVM backend's
-- spec).
module Main (main) where

import Test.Hspec

import FractalStream.Console (transliterateConsole)
import qualified DragSpec
import qualified GeometrySpec
import qualified OverlaySpec
import qualified TableSpec
import qualified HoistSpec

main :: IO ()
main = transliterateConsole >> hspec (OverlaySpec.spec >> GeometrySpec.spec >> DragSpec.spec >> TableSpec.spec >> HoistSpec.spec)
