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
rm -rf "$dist"
mkdir -p "$dist"
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

# The page and the engine's worker: type-checked by TypeScript, and
# bundled with their npm packages (CodeMirror, the WASI shim) by esbuild.
# (drawing.js stands alone too, for dev-tools/web/check-tools.mjs; the
# worker pool is copied in above.)
(cd web && node node_modules/typescript/bin/tsc -p . --noEmit)
(cd web && node_modules/.bin/esbuild src/main.ts src/engine.ts src/drawing.ts \
   --bundle --format=esm --target=es2022 --sourcemap --minify-syntax \
   --external:./worker-pool.mjs --external:./kernel-worker.mjs --outdir=dist --log-level=warning)
rm -rf "$dist/vendor"
cp web/index.html web/style.css "$dist/"
# (the font drawings' text is in: the core's, as fsrun's captures use)
cp fractalstream-core/fonts/DejaVuSans.ttf "$dist/"
echo "Built $dist"
