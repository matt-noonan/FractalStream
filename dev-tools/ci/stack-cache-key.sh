#!/usr/bin/env bash
# A fingerprint of what decides which packages stack builds into ~/.stack
# (the snapshot's and the extra-deps): the stack.yaml files, the packages'
# build-depends (not their module lists, which change all the time), and
# the prebuilt native dependencies (deps.env: wxcore and llvm-hs are built
# against them). The desktop builds key their ~/.stack cache on it, so a
# new cache is saved only when it changes.
#
#   dev-tools/ci/stack-cache-key.sh     prints a short hash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"

{
  cat dev-tools/ci/deps.env
  for f in $(git ls-files '*stack*.yaml' | sort); do
    echo "== $f"
    cat "$f"
  done
  for f in $(git ls-files '*.cabal' | sort); do
    echo "== $f"
    # each build-depends field, with its continuation lines
    awk '
      match($0, /^[ \t]*/) { indent = RLENGTH }
      /^[ \t]*build-depends[ \t]*:/ { inside = 1; field = indent; print; next }
      inside && (/^[ \t]*$/ || indent > field) { print; next }
      { inside = 0 }
    ' "$f"
  done
} | { sha256sum 2>/dev/null || shasum -a 256; } | cut -c1-16
