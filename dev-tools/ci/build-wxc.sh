#!/usr/bin/env bash
# Build wxc (from wxHaskell-3.2 at WXC_COMMIT, deps.env) against the
# installed wxWidgets (wx-config), with its CMake build, and package it as
# installed in /usr/local (paths from /, for `fetch-deps.sh / ...`):
#
#   dev-tools/ci/build-wxc.sh TARBALL [cmake arguments...]
#
# (Windows builds wxc its own way: build-wxc-windows.sh.)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

tarball="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
shift

work="$(mktemp -d)"
git clone -q https://github.com/matt-noonan/wxHaskell-3.2.git "$work/wxHaskell-3.2"
git -C "$work/wxHaskell-3.2" checkout -q "$WXC_COMMIT"
cd "$work/wxHaskell-3.2/wxc"
./generate-version-header.sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local "$@"
cmake --build build --parallel
stage="$work/stage"
DESTDIR="$stage" cmake --install build
tar -czf "$tarball" -C "$stage" .
ls -l "$tarball"
cd /
rm -rf "$work"
