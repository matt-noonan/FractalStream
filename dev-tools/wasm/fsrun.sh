#!/usr/bin/env bash
# Run the WebAssembly build of fsrun under Node, as the native one:
#
#   dev-tools/wasm/fsrun.sh scenario --project p.fs --name simple
#
# (Build it first with dev-tools/wasm/build.sh.) Paths are relative to the
# current folder.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if ! command -v wasm32-wasi-cabal > /dev/null; then
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi
wasm="$(cd "$root" && wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm exe:fsrun 2> /dev/null)"
js="${wasm%.wasm}.js"
# the JavaScript FFI's glue, made again whenever the module changes
if [ ! -f "$js" ] || [ "$wasm" -nt "$js" ]; then
  "$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$js"
fi
exec node --disable-warning=ExperimentalWarning "$root/dev-tools/wasm/run-node.mjs" "$wasm" "$js" "$@"
