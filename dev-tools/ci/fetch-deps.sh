#!/usr/bin/env bash
# Download prebuilt dependencies from the DEPS_RELEASE release (see
# deps.env) and unpack each into the prefix it was built for:
#
#   dev-tools/ci/fetch-deps.sh PREFIX ASSET...
#
# ASSET is a file name like llvm-12.0.1-macos-arm64.tar.gz (from asset.sh),
# holding what's installed in PREFIX (/usr/local on macOS and Linux, which
# takes sudo). Needs the gh CLI, with GH_TOKEN set.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"

dir="$1"
shift
repo="${GITHUB_REPOSITORY:-matt-noonan/FractalStream}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for asset in "$@"; do
  echo "Fetching $asset from $DEPS_RELEASE"
  if ! gh release download "$DEPS_RELEASE" --repo "$repo" --pattern "$asset" --dir "$tmp"; then
    echo "::error::$asset isn't in the $DEPS_RELEASE release: run the CI dependencies workflow (deps.yml) for it"
    exit 1
  fi
  if mkdir -p "$dir" 2> /dev/null && [ -w "$dir" ]; then
    tar -xzf "$tmp/$asset" -C "$dir"
  else
    sudo mkdir -p "$dir"
    sudo tar --no-same-owner -xzf "$tmp/$asset" -C "$dir"
  fi
  rm -f "$tmp/$asset"
done
