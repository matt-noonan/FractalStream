#!/bin/sh
# Fetch wgpu-native (the desktop's WebGPU: fractalstream-backend-wgpu
# links it) into third_party/wgpu-native: its headers and its library, for
# this machine (macOS, Linux, or Windows in an MSYS2 shell), and its chip
# (or the one given: aarch64, x86_64). A pinned release, checked against
# its SHA-256. Run once, before building the desktop app or fsrun; again
# when the version here changes.
#
# On macOS and Linux, the static library (linked in). On Windows, the DLL
# and its import library: GHC's toolchain there is LLVM's mingw, and
# wgpu-native's Windows build is gcc's, so they meet at the DLL's C API;
# the DLL goes beside the app (and on the PATH, for building). And on
# Windows, Microsoft's DXC (dxcompiler.dll, with its licences; not
# dxil.dll, whose licence is Microsoft's own, and which DXC since 1.8.2502
# doesn't need), which Direct3D 12 compiles shaders with: only with it does
# wgpu give shaders doubles (the GPU f64 backend). It goes beside the app
# too (fsgpu.c looks for it there).
#
#   dev-tools/wgpu/fetch.sh [aarch64|x86_64]
set -eu

VERSION=v29.0.1.1
ARCH=${1:-$(uname -m | sed 's/arm64/aarch64/')}
case "$(uname -s)" in
  Darwin) OS=macos ;;
  Linux)  OS=linux ;;
  MINGW*|MSYS*|UCRT64*|CLANG64*|CYGWIN*) OS=windows ;;
  *) echo "fetch.sh: no wgpu-native for $(uname -s) here" >&2; exit 2 ;;
esac
case "$OS-$ARCH" in
  macos-aarch64) SHA256=a5797a37b1adf720bcd5dcffb291edbbd5b7b14be0a3874c28e6393a655a7a3e ;;
  macos-x86_64)  SHA256=8e2f7378548ddd0e2cf21e7d864dda46e953f0af724855a33778b85ead206d41 ;;
  linux-aarch64) SHA256=015fcdf1dbae82e614a783cc38017e5399ae0927a889fe9b69c9b664bc61b47a ;;
  linux-x86_64)  SHA256=95a4d90c071005a98d03eab348beaa6b07e16eb00d1dcdb9f8348f75eb97ec5a ;;
  windows-x86_64) SHA256=d471e3614733c1d4ddd61bfd19868356477d0d37bf531bf8c6cb64a7f579bd2a ;;
  *) echo "fetch.sh: no wgpu-native for $OS on $ARCH here" >&2; exit 2 ;;
esac

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
DEST="$ROOT/third_party/wgpu-native"
# (with DXC, on Windows: a folder fetched before it was added is fetched again)
if [ -f "$DEST/VERSION" ] && [ "$(cat "$DEST/VERSION")" = "$VERSION $OS $ARCH" ] \
   && { [ "$OS" != windows ] || [ -f "$DEST/lib/dxcompiler.dll" ]; }; then
  echo "wgpu-native $VERSION ($OS, $ARCH) is already in third_party/wgpu-native"
  exit 0
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
ZIP="$WORK/wgpu.zip"
# (Windows': its x86_64-gnu build)
NAME="wgpu-$OS-$ARCH"
[ "$OS" = windows ] && NAME="$NAME-gnu"
curl -fsSL -o "$ZIP" "https://github.com/gfx-rs/wgpu-native/releases/download/$VERSION/$NAME-release.zip"
if command -v sha256sum >/dev/null; then GOT=$(sha256sum "$ZIP" | cut -d' ' -f1)
else GOT=$(shasum -a 256 "$ZIP" | cut -d' ' -f1); fi
if [ "$GOT" != "$SHA256" ]; then
  echo "fetch.sh: wgpu-native's checksum is $GOT, not $SHA256" >&2
  exit 1
fi
unzip -q "$ZIP" -d "$WORK/x"

# (only the one library: with both beside each other, the linker would
# take the shared one on macOS and Linux, and the static one on Windows)
rm -rf "$DEST"
mkdir -p "$DEST/include/webgpu" "$DEST/lib"
cp "$WORK/x/include/webgpu/webgpu.h" "$WORK/x/include/webgpu/wgpu.h" "$DEST/include/webgpu/"
if [ "$OS" = windows ]; then
  cp "$WORK/x/lib/libwgpu_native.dll.a" "$WORK/x/lib/wgpu_native.dll" "$DEST/lib/"
else
  cp "$WORK/x/lib/libwgpu_native.a" "$DEST/lib/"
fi
if [ "$OS" = windows ]; then
  DXC_VERSION=v1.8.2505.1
  DXC_SHA256=9ad895a6b039e3a8f8c22a1009f866800b840a74b50db9218d13319e215ea8a4
  curl -fsSL -o "$WORK/dxc.zip" "https://github.com/microsoft/DirectXShaderCompiler/releases/download/$DXC_VERSION/dxc_2025_07_14.zip"
  if command -v sha256sum >/dev/null; then GOT=$(sha256sum "$WORK/dxc.zip" | cut -d' ' -f1)
  else GOT=$(shasum -a 256 "$WORK/dxc.zip" | cut -d' ' -f1); fi
  if [ "$GOT" != "$DXC_SHA256" ]; then
    echo "fetch.sh: DXC's checksum is $GOT, not $DXC_SHA256" >&2
    exit 1
  fi
  unzip -q "$WORK/dxc.zip" bin/x64/dxcompiler.dll LICENSE-LLVM.txt LICENSE-MIT.txt -d "$WORK/dxc"
  cp "$WORK/dxc/bin/x64/dxcompiler.dll" "$DEST/lib/"
  cat "$WORK/dxc/LICENSE-MIT.txt" "$WORK/dxc/LICENSE-LLVM.txt" > "$DEST/lib/dxcompiler-LICENSE.txt"
fi
echo "$VERSION $OS $ARCH" > "$DEST/VERSION"
echo "wgpu-native $VERSION ($OS, $ARCH) is in third_party/wgpu-native"
