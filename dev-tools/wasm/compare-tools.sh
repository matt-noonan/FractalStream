#!/usr/bin/env bash
# Compare the WebAssembly build's tools with the native LLVM build's: for
# every tool of every project given (default: the whole library), select
# it in its viewer, click, then drag, and capture every viewer (with the
# tools' drawings, and whatever the tool set showing in the renders); then
# compare each capture of the wasm build with the native one's.
#
#   dev-tools/wasm/compare-tools.sh [-s SIZE] [-o DIR] [project.fs ...]
#
#   -s SIZE     the viewers' width and height, in pixels (default 100)
#   -o DIR      where the captures and logs go (default: a fresh temporary
#               folder, removed afterwards unless something failed)
#
# The view is set the same for each tool's viewer (centered at (0.12, 0.31),
# 0.01 a pixel) so that the clicked points are in it, whatever the viewer.
# Each tool is reported as identical, as differing (in how many pixels, of
# which viewers), as not compiled by the wasm backend, or as failed. The
# exit status is 1 if any wasm run failed (other than not compiling), else 0.
#
# Needs the wasm build (dev-tools/wasm/build.sh), the native fsrun with LLVM
# (built here with stack if it isn't), and ImageMagick (`magick`).
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
size=100
out=""
while getopts "s:o:" opt; do
  case "$opt" in
    s) size="$OPTARG" ;;
    o) out="$OPTARG" ;;
    *) sed -n '2,22p' "$0"; exit 2 ;;
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

# A project's tools, each with its viewer (or none, if it doesn't say),
# tab-separated: those it declares, and each viewer's Select tool (for its
# var=, or xvar= and yvar=: a viewer whose variables aren't the
# configuration's has none, and the native run says so).
tools() {
  grep -o '^ *@tool "[^"]*"\( *for=\("[^"]*"\|[^ ]*\)\)\?' "$1" \
    | sed -E 's/^ *@tool "([^"]*)"( *for=("([^"]*)"|([^ ]*)))?/\1\t\4\5/'
  grep -oE '^ *@viewer "[^"]*" (.* )?var=[^ ]+' "$1" \
    | sed -E 's/^ *@viewer "([^"]*)" (.* )?var=([^ ]+).*/Select \3\t\1/'
  # (a real viewer's: "Select (x, y)")
  grep -oE '^ *@viewer "[^"]*" (.* )?xvar=[^ ]+ yvar=[^ ]+' "$1" \
    | sed -E 's/^ *@viewer "([^"]*)" (.* )?xvar=([^ ]+) yvar=([^ ]+).*/Select (\3, \4)\t\1/'
}

k=0; failed=0; identical=0; differing=0; uncompiled=0
for p in "${projects[@]}"; do
  viewers=()
  while IFS= read -r v; do viewers+=("$v"); done \
    < <(grep -o '^ *@viewer "[^"]*"' "$p" | sed 's/.*@viewer "\(.*\)"/\1/')
  # each tool, and its viewer (all of them, if it doesn't say)
  while IFS=$'\t' read -r tool for; do
    if [ -n "$for" ]; then targets=("$for"); else targets=("${viewers[@]}"); fi
    for v in "${targets[@]}"; do
      k=$((k + 1)); d="$out/$k"; mkdir -p "$d/wasm" "$d/llvm"
      {
        for w in "${viewers[@]}"; do printf 'select viewer "%s"\nresize viewer to %sx%s\n' "$w" "$size" "$size"; done
        printf 'select viewer "%s"\n' "$v"
        printf 'set viewer center to (0.12, 0.31)\nset viewer pixel size to 0.01\n'
        printf 'select tool "%s"\n' "$tool"
        printf 'click at (0.1, 0.3)\n'
        printf 'drag from (0.1, 0.3) to (0.15, 0.25)\n'
        i=0
        for w in "${viewers[@]}"; do printf 'capture viewer "%s" to "%s.png"\n' "$w" "$i"; i=$((i + 1)); done
      } > "$d/commands.txt"
      (cd "$d/wasm" && "$root/dev-tools/wasm/fsrun.sh" commands --project "$p" \
          --commands ../commands.txt > log 2>&1) || true
      (cd "$d/llvm" && "$native" commands --project "$p" --commands ../commands.txt -j 1 \
          > log 2>&1) || true
      if grep -q "can't compile" "$d/wasm/log"; then
        result="not compiled: $(grep -o "the wasm backend can't compile [^;]*" "$d/wasm/log" | head -1)"
        uncompiled=$((uncompiled + 1))
      elif [ ! -f "$d/llvm/0.png" ]; then
        result="(the native run failed: $(tail -1 "$d/llvm/log"))"
      elif [ ! -f "$d/wasm/0.png" ]; then
        result="FAILED: $(tail -1 "$d/wasm/log")"; failed=$((failed + 1))
      else
        diffs=""
        i=0
        for w in "${viewers[@]}"; do
          if [ ! -f "$d/wasm/$i.png" ]; then diffs="$diffs $w: missing;"
          elif ! cmp -s "$d/wasm/$i.png" "$d/llvm/$i.png"; then
            n=$(magick compare -metric AE "$d/wasm/$i.png" "$d/llvm/$i.png" null: 2>&1 | cut -d' ' -f1 || true)
            diffs="$diffs $w: $n pixels;"
          fi
          i=$((i + 1))
        done
        if [ -z "$diffs" ]; then result="identical"; identical=$((identical + 1))
        else result="differs ($diffs )"; differing=$((differing + 1)); fi
      fi
      echo "$(basename "$p") / $v / $tool: $result"
    done
  done < <(tools "$p")
done

echo "$k tools: $identical identical, $differing differing, $uncompiled not compiled, $failed failed"
if [ "$failed" -gt 0 ]; then
  echo "(captures and logs are in $out)"
  exit 1
fi
if [ "$keep" -eq 0 ]; then rm -rf "$out"; fi
