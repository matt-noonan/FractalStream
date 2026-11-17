#!/usr/bin/env bash
# The file name of a prebuilt dependency (see deps.env), for this machine:
#
#   dev-tools/ci/asset.sh llvm
#   dev-tools/ci/asset.sh wxwidgets static|dynamic    (macOS)
#   dev-tools/ci/asset.sh wxc [static|dynamic]        (the variant: macOS)
#
# so that deps.yml, which makes them, and the builds, which fetch them,
# agree. Each name says what's in it, and which platform it's for.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

case "${OSTYPE:-}" in
  darwin*) os=macos ;;
  msys*|cygwin*) os=windows ;;
  *) os=linux ;;
esac
case "$(uname -m)" in
  arm64|aarch64) arch=arm64 ;;
  *) arch=x86_64 ;;
esac
platform="$os-$arch"

case "$1" in
  llvm)
    if [ "$os" = windows ]; then
      echo "llvm-$LLVM_VERSION-$platform-ghc-$GHC_VERSION.tar.gz"
    else
      echo "llvm-$LLVM_VERSION-$platform.tar.gz"
    fi ;;
  wxwidgets)
    echo "wxwidgets-$WX_VERSION-$platform-$2.tar.gz" ;;
  wxc)
    echo "wxc-${WXC_COMMIT:0:12}-$platform${2:+-wx-$2}.tar.gz" ;;
  *)
    echo "no such dependency: $1" >&2
    exit 1 ;;
esac
