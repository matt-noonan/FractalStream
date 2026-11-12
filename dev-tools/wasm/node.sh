#!/usr/bin/env bash
# Run a WebAssembly build of one of FractalStream's executables or test
# suites (a reactor module exporting its main as hs_start) under Node:
#
#   dev-tools/wasm/node.sh exe:fsrun scenario --project p.fs --name simple
#   dev-tools/wasm/node.sh fractalstream-ui-headless:test:end-to-end
#
# (Build it first with dev-tools/wasm/build.sh, with --enable-tests for a
# test suite.) Paths are relative to the current folder.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
target="$1"; shift
if ! command -v wasm32-wasi-cabal > /dev/null; then
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi
tests=""
case "$target" in *:test:*) tests="--enable-tests" ;; esac
wasm="$(cd "$root" && wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm $tests "$target" 2> /dev/null)"
js="${wasm%.wasm}.js"
# the JavaScript FFI's glue, made again whenever the module changes
if [ ! -f "$js" ] || [ "$wasm" -nt "$js" ]; then
  "$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$js"
fi
exec node --disable-warning=ExperimentalWarning "$root/dev-tools/wasm/run-node.mjs" "$wasm" "$js" "$@"
