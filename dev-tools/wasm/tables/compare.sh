#!/usr/bin/env bash
# Compare the WebAssembly build's tables with the native LLVM build's: run
# each scenario here (*.scenario, against the project of the same name,
# .fs) with both, and compare the PNGs each one captures.
#
#   dev-tools/wasm/tables/compare.sh [-o DIR]
#
# The projects' saved tables are made by MakeFixture.hs (see its header).
# Needs the wasm build (dev-tools/wasm/build.sh exe:fsrun) and the native
# fsrun with LLVM. The exit status is 1 if any capture differs or is
# missing.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
out=""
while getopts "o:" opt; do
  case "$opt" in
    o) out="$OPTARG" ;;
    *) sed -n '2,12p' "$0"; exit 2 ;;
  esac
done
keep=1
if [ -z "$out" ]; then out="$(mktemp -d)"; keep=0; fi
native="$(cd "$root/fractalstream-ui-headless" && stack path --local-install-root)/bin/fsrun"

failed=0
for scenario in "$here"/*.scenario; do
  name="$(basename "$scenario" .scenario)"
  project="$here/${name%%.*}.fs"
  d="$out/$name"; mkdir -p "$d/wasm" "$d/llvm"
  (cd "$d/wasm" && "$root/dev-tools/wasm/fsrun.sh" commands --project "$project" --commands "$scenario" > log 2>&1) || true
  (cd "$d/llvm" && "$native" commands --project "$project" --commands "$scenario" -j 1 > log 2>&1) || true
  if grep -q '^note:' "$d/wasm/log"; then
    echo "$name: $(grep '^note:' "$d/wasm/log" | head -1)"; failed=$((failed + 1)); continue
  fi
  for png in "$d"/llvm/*.png; do
    f="$(basename "$png")"
    if [ ! -f "$d/wasm/$f" ]; then
      echo "$name / $f: missing from the wasm run ($(tail -1 "$d/wasm/log"))"; failed=$((failed + 1))
    elif cmp -s "$png" "$d/wasm/$f"; then
      echo "$name / $f: identical"
    else
      echo "$name / $f: differs in $(magick compare -metric AE "$png" "$d/wasm/$f" null: 2>&1 | cut -d' ' -f1) pixels"
      failed=$((failed + 1))
    fi
  done
done
if [ "$failed" -gt 0 ]; then echo "$failed failed (see $out)"; exit 1; fi
echo "all identical"
if [ "$keep" -eq 0 ]; then rm -rf "$out"; fi
