#!/usr/bin/env bash
# Run the WebAssembly build of fsrun under Node, as the native one:
#
#   dev-tools/wasm/fsrun.sh scenario --project p.fs --name simple
#
# (Build it first with dev-tools/wasm/build.sh.) Paths are relative to the
# current folder.
exec "$(dirname "${BASH_SOURCE[0]}")/node.sh" exe:fsrun "$@"
