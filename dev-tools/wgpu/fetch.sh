#!/bin/sh
# Fetch wgpu-native (the desktop's WebGPU: fractalstream-backend-wgpu
# links it, statically) into third_party/wgpu-native: its headers and its
# static library, for this machine (macOS or Linux), and its chip (or the
# one given: aarch64, x86_64). A pinned release, checked against its
# SHA-256. Run once, before building the desktop app or fsrun; again when
# the version here changes.
#
#   dev-tools/wgpu/fetch.sh [aarch64|x86_64]
set -eu

VERSION=v29.0.1.1
ARCH=${1:-$(uname -m | sed 's/arm64/aarch64/')}
case "$(uname -s)" in
  Darwin) OS=macos ;;
  Linux)  OS=linux ;;
  *) echo "fetch.sh: no wgpu-native for $(uname -s) here" >&2; exit 2 ;;
esac
case "$OS-$ARCH" in
  macos-aarch64) SHA256=a5797a37b1adf720bcd5dcffb291edbbd5b7b14be0a3874c28e6393a655a7a3e ;;
  macos-x86_64)  SHA256=8e2f7378548ddd0e2cf21e7d864dda46e953f0af724855a33778b85ead206d41 ;;
  linux-aarch64) SHA256=015fcdf1dbae82e614a783cc38017e5399ae0927a889fe9b69c9b664bc61b47a ;;
  linux-x86_64)  SHA256=95a4d90c071005a98d03eab348beaa6b07e16eb00d1dcdb9f8348f75eb97ec5a ;;
  *) echo "fetch.sh: no wgpu-native for $OS on $ARCH here" >&2; exit 2 ;;
esac

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
DEST="$ROOT/third_party/wgpu-native"
if [ -f "$DEST/VERSION" ] && [ "$(cat "$DEST/VERSION")" = "$VERSION $OS $ARCH" ]; then
  echo "wgpu-native $VERSION ($OS, $ARCH) is already in third_party/wgpu-native"
  exit 0
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
ZIP="$WORK/wgpu.zip"
curl -fsSL -o "$ZIP" "https://github.com/gfx-rs/wgpu-native/releases/download/$VERSION/wgpu-$OS-$ARCH-release.zip"
if command -v sha256sum >/dev/null; then GOT=$(sha256sum "$ZIP" | cut -d' ' -f1)
else GOT=$(shasum -a 256 "$ZIP" | cut -d' ' -f1); fi
if [ "$GOT" != "$SHA256" ]; then
  echo "fetch.sh: wgpu-native's checksum is $GOT, not $SHA256" >&2
  exit 1
fi
unzip -q "$ZIP" -d "$WORK/x"

# (only the static library: with the dylib beside it, the linker would
# take that instead)
rm -rf "$DEST"
mkdir -p "$DEST/include/webgpu" "$DEST/lib"
cp "$WORK/x/include/webgpu/webgpu.h" "$WORK/x/include/webgpu/wgpu.h" "$DEST/include/webgpu/"
cp "$WORK/x/lib/libwgpu_native.a" "$DEST/lib/"
echo "$VERSION $OS $ARCH" > "$DEST/VERSION"
echo "wgpu-native $VERSION ($OS, $ARCH) is in third_party/wgpu-native"
