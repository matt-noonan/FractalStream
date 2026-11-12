#!/usr/bin/env bash
# Compare the WebAssembly build's renders with the native LLVM build's:
# every viewer of every project given (default: the whole library), at a
# small size, each captured by both, and the PNGs compared.
#
#   dev-tools/wasm/compare.sh [-s SIZE] [-w WORKERS] [-o DIR] [project.fs ...]
#
#   -s SIZE     the viewers' width and height, in pixels (default 100)
#   -w WORKERS  the wasm build's workers (default: one per core; 0 for none)
#   -o DIR      where the captures and logs go (default: a fresh temporary
#               folder, removed afterwards unless something failed)
#
# Each viewer is reported as identical, as differing (with how many pixels
# differ: a few, in chaotic regions, come from the C libraries' math
# functions, which aren't the same), as interpreted (the wasm backend
# couldn't compile it), or as failed. The exit status is 1 if any wasm
# render failed, else 0.
#
# Needs the wasm build (dev-tools/wasm/build.sh), the native fsrun with
# LLVM (built here with stack if it isn't), and ImageMagick (`magick`).
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
size=100
workers=""
out=""
while getopts "s:w:o:" opt; do
  case "$opt" in
    s) size="$OPTARG" ;;
    w) workers="$OPTARG" ;;
    o) out="$OPTARG" ;;
    *) sed -n '2,20p' "$0"; exit 2 ;;
  esac
done
shift $((OPTIND - 1))

projects=()
if [ "$#" -eq 0 ]; then
  while IFS= read -r -d '' f; do projects+=("$f"); done \
    < <(find "$root/fractalstream-library/projects" -name '*.fs' -print0 | sort -z)
else
  for p in "$@"; do projects+=("$(cd "$(dirname "$p")" && pwd)/$(basename "$p")"); done
fi

native="$(cd "$root/fractalstream-ui-headless" && stack path --local-install-root)/bin/fsrun"
if [ ! -x "$native" ]; then
  echo "Building the native fsrun (LLVM backend)..." >&2
  (cd "$root/fractalstream-ui-headless" && stack build >&2)
fi

keep=1
if [ -z "$out" ]; then out="$(mktemp -d)"; keep=0; fi
if [ -n "$workers" ]; then export FS_WASM_WORKERS="$workers"; fi

k=0; failed=0; identical=0; differing=0; interpreted=0
for p in "${projects[@]}"; do
  while IFS= read -r v; do
    k=$((k + 1)); d="$out/$k"; mkdir -p "$d/wasm" "$d/llvm"
    printf 'select viewer "%s"\nresize viewer to %sx%s\ncapture viewer "%s" to "v.png"\n' \
      "$v" "$size" "$size" "$v" > "$d/commands.txt"
    (cd "$d/wasm" && "$root/dev-tools/wasm/fsrun.sh" commands --project "$p" \
        --commands ../commands.txt > log 2>&1) || true
    (cd "$d/llvm" && "$native" commands --project "$p" --commands ../commands.txt -j 1 \
        > log 2>&1) || true
    note=$(grep -h '^note:' "$d/wasm/log" | sed 's/^note: //; s/; interpreting.*//' | sort -u | tr '\n' ' ' || true)
    if [ ! -f "$d/wasm/v.png" ]; then
      result="FAILED: $(grep -v '^note' "$d/wasm/log" | tail -1)"; failed=$((failed + 1))
    elif [ ! -f "$d/llvm/v.png" ]; then
      result="(the native render failed: $(tail -1 "$d/llvm/log"))"
    elif cmp -s "$d/wasm/v.png" "$d/llvm/v.png"; then
      result="identical"; identical=$((identical + 1))
    else
      n=$(magick compare -metric AE "$d/wasm/v.png" "$d/llvm/v.png" null: 2>&1 | cut -d' ' -f1 || true)
      result="differs in $n pixels"; differing=$((differing + 1))
    fi
    if [ -n "$note" ]; then result="$result (interpreted: $note)"; interpreted=$((interpreted + 1)); fi
    echo "$(basename "$p") / $v: $result"
  done < <(grep -o '^ *@viewer "[^"]*"' "$p" | sed 's/.*@viewer "\(.*\)"/\1/')
done

echo "$k viewers: $identical identical, $differing differing, $interpreted interpreted, $failed failed"
if [ "$failed" -gt 0 ]; then
  echo "(captures and logs are in $out)"
  exit 1
fi
if [ "$keep" -eq 0 ]; then rm -rf "$out"; fi
