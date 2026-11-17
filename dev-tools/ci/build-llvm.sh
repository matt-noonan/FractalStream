#!/usr/bin/env bash
# Build LLVM (LLVM_VERSION, from deps.env) for llvm-hs, and package it:
#
#   dev-tools/ci/build-llvm.sh PREFIX TARBALL [cmake arguments...]
#
# Only what llvm-hs uses: the libraries (static), the headers, and
# llvm-config, for the host's target only, without zlib, terminfo or
# libxml2 (so llvm-config asks for no system libraries beyond the basics).
# With Ninja. TARBALL holds what's installed in PREFIX (/usr/local, or on
# Windows C:/llvm12), for `fetch-deps.sh PREFIX ...`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

prefix="$1"
tarball="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
shift 2

case "$(uname -m)" in
  arm64|aarch64) target=AArch64 ;;
  *) target=X86 ;;
esac
windows=false
case "${OSTYPE:-}" in msys*|cygwin*) windows=true ;; esac

work="$(mktemp -d)"
src="llvm-$LLVM_VERSION.src"
curl -fsSL --retry 5 -o "$work/llvm.tar.xz" \
  "https://github.com/llvm/llvm-project/releases/download/llvmorg-$LLVM_VERSION/$src.tar.xz"
# (a few test symlinks can't be made on Windows)
tar -xf "$work/llvm.tar.xz" -C "$work" || $windows

cmake -G Ninja -S "$work/$src" -B "$work/build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$prefix" \
  -DLLVM_TARGETS_TO_BUILD="$target" \
  -DLLVM_ENABLE_ZLIB=OFF \
  -DLLVM_ENABLE_TERMINFO=OFF \
  -DLLVM_ENABLE_LIBXML2=OFF \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_INCLUDE_EXAMPLES=OFF \
  -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_DOCS=OFF \
  -DLLVM_BUILD_TOOLS=OFF \
  -DLLVM_BUILD_UTILS=OFF \
  "$@"

if $windows; then
  ninja -C "$work/build" install-llvm-headers install-llvm-libraries llvm-config
  mkdir -p "$(cygpath -u "$prefix")/bin"
  cp "$work/build/bin/llvm-config.exe" "$(cygpath -u "$prefix")/bin/"
  tar -czf "$tarball" -C "$(cygpath -u "$prefix")" .
else
  stage="$work/stage"
  DESTDIR="$stage" ninja -C "$work/build" install-llvm-headers install-llvm-libraries llvm-config
  mkdir -p "$stage$prefix/bin"
  cp "$work/build/bin/llvm-config" "$stage$prefix/bin/"
  tar -czf "$tarball" -C "$stage$prefix" .
fi
ls -l "$tarball"
rm -rf "$work"
