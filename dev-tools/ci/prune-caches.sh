#!/usr/bin/env bash
# Delete the GitHub Actions caches whose keys start with PREFIX, except
# KEEP (the one just saved): the ones it replaces, which would otherwise
# use up the repository's 10 GB until they went unused for a week.
#
#   dev-tools/ci/prune-caches.sh PREFIX KEEP
#
# Needs the gh CLI, with GH_TOKEN set to a token with actions: write.
set -euo pipefail

prefix="$1"
keep="$2"
repo="${GITHUB_REPOSITORY:-matt-noonan/FractalStream}"

gh cache list --repo "$repo" --key "$prefix" --limit 100 --json id,key \
  --jq ".[] | select(.key != \"$keep\") | \"\\(.id) \\(.key)\"" |
while read -r id key; do
  echo "Deleting $key"
  gh cache delete --repo "$repo" "$id"
done
