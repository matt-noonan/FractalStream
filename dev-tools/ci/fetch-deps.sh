#!/usr/bin/env bash
# Download prebuilt dependencies from the DEPS_RELEASE release (see
# deps.env) and unpack each into a directory:
#
#   dev-tools/ci/fetch-deps.sh DIR ASSET...
#
# ASSET is a file name like llvm-12.0.1-macos-arm64.tar.gz. Unpacking into
# / (as the macOS and Linux packages are made to be, under /usr/local) uses
# sudo. Needs the gh CLI, with GH_TOKEN set.
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
  if [ "$dir" = / ]; then
    sudo tar -xzf "$tmp/$asset" -C /
  else
    mkdir -p "$dir"
    tar -xzf "$tmp/$asset" -C "$dir"
  fi
  rm -f "$tmp/$asset"
done
