-- | Tests that run projects end to end, so they link a backend. They're
-- kept apart from the @spec@ suite, whose quasiquotes would make GHC load
-- the backend's LLVM libraries at compile time (see the LLVM backend's
-- spec).
module Main (main) where

import Test.Hspec

import FractalStream.Console (transliterateConsole)
import qualified OverlaySpec

main :: IO ()
main = transliterateConsole >> hspec OverlaySpec.spec
