#!/usr/bin/env bash
# Build wxc (from wxHaskell-3.2 at WXC_COMMIT, deps.env) as a DLL, in an
# MSYS2 UCRT64 shell, against MSYS2's wxWidgets 3.2, and package it as the
# contents of a prefix (bin/wxc.dll, lib/libwxc.dll.a, include/wxc), for
# `fetch-deps.sh wxc-prefix ...`:
#
#   dev-tools/ci/build-wxc-windows.sh TARBALL
#
# The same sources as wxc's CMakeLists.txt, but compiled with g++ directly:
# CMake's FindwxWidgets is unreliable under MSYS2. As a DLL, so that its C++
# runtime stays inside it and GHC only sees a C interface.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

tarball="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

work="$(mktemp -d)"
prefix="$work/wxc-prefix"
mkdir -p "$prefix/bin" "$prefix/lib" "$prefix/include"

git clone -q https://github.com/matt-noonan/wxHaskell-3.2.git "$work/wxHaskell-3.2"
git -C "$work/wxHaskell-3.2" checkout -q "$WXC_COMMIT"
cd "$work/wxHaskell-3.2/wxc"
./generate-version-header.sh

CXXFLAGS="$(wx-config --cxxflags) -std=c++14 -O2 -DwxcREFUSE_MEDIACTRL -Iinclude"
export CXXFLAGS
mkdir -p obj
grep -o 'src/cpp/[A-Za-z_]*\.cpp' CMakeLists.txt \
  | xargs -P 4 -I{} sh -c 'echo "$1"; g++ $CXXFLAGS -c "$1" -o "obj/$(basename "$1" .cpp).o"' _ {}

# Some wxc functions are marked dllexport and some aren't, so export
# everything.
# shellcheck disable=SC2046
g++ -shared -o "$prefix/bin/wxc.dll" obj/*.o \
  -Wl,--export-all-symbols \
  -Wl,--out-implib,"$prefix/lib/libwxc.dll.a" \
  $(wx-config --libs gl,base,core,richtext,stc,xrc,propgrid,qa,html,adv,aui,ribbon,net)
cp -r include/wxc "$prefix/include/"

tar -czf "$tarball" -C "$prefix" .
ls -l "$tarball"
cd /
rm -rf "$work"
