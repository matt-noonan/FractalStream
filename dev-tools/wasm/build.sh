#!/usr/bin/env bash
# Build FractalStream's WebAssembly targets (see WEB_PLAN.md), with the GHC
# wasm toolchain from ghc-wasm-meta (FLAVOUR=9.10, in ~/.ghc-wasm).
#
#   dev-tools/wasm/build.sh [cabal build arguments...]   (default: all)
#
# The wasm build's Template Haskell can't run git (as githash does for the
# desktop build), so this first writes what git says into the
# fractalstream-gitinfo package's module, which FractalStream.Metadata
# uses on wasm.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"

# The test suites run hspec-discover while they're compiled. Cabal would
# build it for wasm, which can't run here, so a native one (the
# snapshot's version, built once with the host's cabal and GHC) goes
# first on the PATH. It has to be there whenever a suite is configured:
# cabal remembers the PATH from then.
tools="$root/dist-newstyle/native-tools"
if [ ! -x "$tools/hspec-discover" ]; then
  echo "Building a native hspec-discover (once) in $tools"
  cabal install --installdir="$tools" --install-method=copy hspec-discover-2.11.13
fi
export PATH="$tools:$PATH"

if ! command -v wasm32-wasi-cabal > /dev/null; then
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi

# A Haskell string literal for $1.
literal() {
  printf '"%s"' "$(printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')"
}

# The same things githash reports for the desktop build.
tag=$(git describe --always --tags)
branch=$(git rev-parse --abbrev-ref HEAD)
hash=$(git rev-parse HEAD)
if [ -n "$(git status --porcelain)" ]; then dirty=True; else dirty=False; fi

module="-- Written by dev-tools/wasm/build.sh; don't edit (or commit) it.
module FractalStream.GitInfo (gitHash, gitTag, gitBranch, gitDirty) where

gitHash, gitTag, gitBranch :: String
gitHash = $(literal "$hash")
gitTag = $(literal "$tag")
gitBranch = $(literal "$branch")

gitDirty :: Bool
gitDirty = $dirty"

# Only rewrite the module when it changes, so an unchanged checkout
# doesn't rebuild the core.
file=fractalstream-gitinfo/src/FractalStream/GitInfo.hs
if [ "$(cat "$file" 2> /dev/null)" != "$module" ]; then
  mkdir -p "$(dirname "$file")"
  printf '%s\n' "$module" > "$file"
fi

# The math functions kernels import (see fractalstream-backend-wasm/libm),
# as a wasm module of their own, for the JavaScript host to load.
libm_src=fractalstream-backend-wasm/libm/fsmath.c
libm=fractalstream-backend-wasm/js/libm.wasm
if [ ! -f "$libm" ] || [ "$libm_src" -nt "$libm" ]; then
  wasm32-wasi-clang -O2 -nostartfiles -Wl,--no-entry "$libm_src" -o "$libm" -lm
fi

# The autocolor lookup kernels import. It works in the kernel's memory, so
# it mustn't keep data or a stack there.
ac_src=fractalstream-backend-wasm/libm/fsautocolor.c
ac_header=fractalstream-core/src/Actor/autocolor_lookup.h
ac=fractalstream-backend-wasm/js/autocolor.wasm
if [ ! -f "$ac" ] || [ "$ac_src" -nt "$ac" ] || [ "$ac_header" -nt "$ac" ]; then
  wasm32-wasi-clang -O2 -nostdlib -Wl,--no-entry -Wl,--import-memory "$ac_src" -o "$ac"
  if wasm-dis "$ac" | grep -qE '^ *\(data|global\.(get|set)'; then
    rm -f "$ac"
    echo "error: $ac_src uses memory of its own (data or a stack)" >&2
    exit 1
  fi
fi

if [ "$#" -eq 0 ]; then set -- all; fi
exec wasm32-wasi-cabal build --project-file=cabal.project.wasm "$@"
