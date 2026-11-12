#!/usr/bin/env bash
# Build FractalStream's web app into web/dist (see WEB_PLAN.md), to serve
# as static files (dev-tools/web/serve.mjs serves it locally):
#
#   dev-tools/web/build.sh
#
# The engine (fractalstream-ui-web) with the GHC wasm toolchain, the page
# and its worker (web/src) with TypeScript (npm install in web/ first).
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"
dist=web/dist
mkdir -p "$dist/vendor"
if ! command -v wasm32-wasi-cabal > /dev/null; then
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi

# The engine, and the wasm backend's libm.wasm and autocolor.wasm.
dev-tools/wasm/build.sh exe:fractalstream-web
wasm="$(wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm exe:fractalstream-web 2> /dev/null)"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$dist/fractalstream-web.js"
# Smaller (16.7 MB to 7.6 MB; gzipped, 2.9 MB to 2.4 MB): optimized for
# size, after the glue above is made from the module as GHC left it.
wasm-opt -Oz "$wasm" -o "$dist/fractalstream-web.wasm"
cp fractalstream-backend-wasm/js/kernel-worker.mjs fractalstream-backend-wasm/js/worker-pool.mjs \
   fractalstream-backend-wasm/js/libm.wasm fractalstream-backend-wasm/js/autocolor.wasm \
   fractalstream-backend-wasm/js/tables.wasm "$dist/"

# The page and the engine's worker. (Browsers don't resolve npm's bare
# module names, so the WASI shim is copied in, and imported by path.)
cp -R web/node_modules/@bjorn3/browser_wasi_shim/dist/. "$dist/vendor/browser_wasi_shim/"
(cd web && node node_modules/typescript/bin/tsc -p .)
sed -i.bak 's#from "@bjorn3/browser_wasi_shim"#from "./vendor/browser_wasi_shim/index.js"#' "$dist/engine.js"
rm -f "$dist/engine.js.bak"
cp web/index.html web/style.css "$dist/"
# (the font drawings' text is in: the core's, as fsrun's captures use)
cp fractalstream-core/fonts/DejaVuSans.ttf "$dist/"
echo "Built $dist"
