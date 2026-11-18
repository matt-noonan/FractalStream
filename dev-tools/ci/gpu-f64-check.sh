#!/usr/bin/env bash
# Draw some library viewers with the CPU and with the GPU in doubles
# (gpu-f64), and compare them: on a runner whose GPU (a software one:
# lavapipe on Linux, WARP on Windows) has doubles, the pictures must be
# nearly the same. One without doubles says so, and passes.
#
#   dev-tools/ci/gpu-f64-check.sh FSRUN OUTDIR
set -uo pipefail

mkdir -p "$2"
fsrun="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
out="$(cd "$2" && pwd)"
here="$(cd "$(dirname "$0")" && pwd)"
# (projects by paths relative to the library: on Windows, MSYS's
# conversion of an absolute path to fsrun's lost one with an apostrophe)
cd "$here/../../fractalstream-library/projects"

checks=(
  "1 Getting started/mandelbrot-set.fs|Mandelbrot set with smooth coloring"
  "2 Quadratic family/bottcher-coordinates.fs|Dynamical plane"
  "3 Newton's method/newton-cubic.fs|Roots of f"
  "6 Quadratic rational maps/period-3.fs|Parameter plane"
  "7 The hyperbolic plane/j-invariant.fs|j-invariant"
)

py="$(command -v python3 || command -v python)"
failed=0
n=0
for check in "${checks[@]}"; do
  project="${check%%|*}"
  viewer="${check#*|}"
  n=$((n + 1))
  echo "== $project: $viewer"
  "$fsrun" snapshot --backend cpu --project "$project" --viewer "$viewer" --output "$out/$n-cpu.png" >/dev/null || { echo "the CPU couldn't draw it"; failed=1; continue; }
  start=$(date +%s)
  if ! "$fsrun" snapshot --backend gpu-f64 --project "$project" --viewer "$viewer" --output "$out/$n-gpu-f64.png" > "$out/$n.log" 2>&1; then
    cat "$out/$n.log"
    if grep -q "has no doubles" "$out/$n.log"; then echo "(no doubles on this GPU)"; continue; fi
    failed=1
    continue
  fi
  echo "gpu-f64 took $(( $(date +%s) - start )) s"
  "$py" "$here/compare-png.py" "$out/$n-cpu.png" "$out/$n-gpu-f64.png" --min-close 99 || failed=1
done
exit $failed
