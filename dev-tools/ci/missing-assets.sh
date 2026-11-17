#!/usr/bin/env bash
# Whether any of these files is missing from the DEPS_RELEASE release (see
# deps.env): prints true or false. deps.yml builds only what's missing, so
# that running it again (after a job failed, say) doesn't rebuild the rest,
# and a release's files, once there, don't change.
#
#   dev-tools/ci/missing-assets.sh ASSET...
#
# Needs the gh CLI, with GH_TOKEN set.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$here/deps.env"
repo="${GITHUB_REPOSITORY:-matt-noonan/FractalStream}"

have="$(gh release view "$DEPS_RELEASE" --repo "$repo" --json assets --jq '.assets[].name')"
for asset in "$@"; do
  if ! grep -qxF "$asset" <<< "$have"; then
    echo "$asset isn't there yet" >&2
    echo true
    exit 0
  fi
done
echo "all there: $*" >&2
echo false
