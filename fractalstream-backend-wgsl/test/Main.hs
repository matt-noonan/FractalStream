{-# LANGUAGE CPP #-}
-- | The GPU backends' spec (see "Backend.WGSLSpec"). On WebAssembly, a
-- reactor module exporting its main, run under Node by
-- dev-tools/wasm/node.sh fractalstream-backend-wgsl:test:spec.
module Main (main) where

import Test.Hspec
import qualified Backend.WGSLSpec

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = hspec Backend.WGSLSpec.spec
