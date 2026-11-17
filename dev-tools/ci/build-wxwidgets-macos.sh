#!/usr/bin/env bash
# Build wxWidgets (WX_VERSION, from deps.env) on macOS, monolithic, with
# patches/atomic-refcounts.patch, and package what it installs in
# /usr/local (for `fetch-deps.sh /usr/local ...`), with wx-config-3.2
# and wxrc-3.2 beside wx-config and wxrc:
#
#   dev-tools/ci/build-wxwidgets-macos.sh static|dynamic TARBALL
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

case "$1" in
  static) shared=OFF ;;
  dynamic) shared=ON ;;
  *) echo "static or dynamic, not $1"; exit 1 ;;
esac
tarball="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"

work="$(mktemp -d)"
src="$work/wxWidgets-$WX_VERSION"
curl -fsSL --retry 5 -o "$work/wx.tar.bz2" \
  "https://github.com/wxWidgets/wxWidgets/releases/download/v$WX_VERSION/wxWidgets-$WX_VERSION.tar.bz2"
tar -xf "$work/wx.tar.bz2" -C "$work"
# (a plain patch: the sources aren't a git checkout)
patch -d "$src" -p1 < "$root/patches/atomic-refcounts.patch"

cmake -G Ninja -S "$src" -B "$work/build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DwxBUILD_SHARED="$shared" \
  -DwxBUILD_MONOLITHIC=ON
ninja -C "$work/build"
stage="$work/stage"
DESTDIR="$stage" ninja -C "$work/build" install
ln -s wx-config "$stage/usr/local/bin/wx-config-3.2"
if [ -e "$stage/usr/local/bin/wxrc" ]; then ln -s wxrc "$stage/usr/local/bin/wxrc-3.2"; fi
tar -czf "$tarball" -C "$stage/usr/local" .
ls -l "$tarball"
rm -rf "$work"
