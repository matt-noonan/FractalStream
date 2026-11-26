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

# (project|viewer|how much must be within 2 levels of the CPU's, in %:
# Period 3's parameter plane is as sensitive as that to a nudge of 1e-15
# on the CPU itself: 96.36% of it stays within 8, as df64's 96.01%)
checks=(
  "1 Getting started/mandelbrot-set.fs|Mandelbrot set with smooth coloring|99"
  "2 Quadratic family/bottcher-coordinates.fs|Dynamical plane|99"
  "3 Newton's method/newton-cubic.fs|Roots of f|99"
  "7 Quadratic rational maps/period-3.fs|Parameter plane|95"
  "8 Language examples/j-invariant.fs|j-invariant|99"
)

py="$(command -v python3 || command -v python)"
failed=0
n=0
for check in "${checks[@]}"; do
  IFS='|' read -r project viewer minimum <<< "$check"
  n=$((n + 1))
  echo "== $project: $viewer"
  "$fsrun" snapshot --backend cpu --project "$project" --viewer "$viewer" --output "$out/$n-cpu.png" >/dev/null || { echo "the CPU couldn't draw it"; failed=1; continue; }
  start=$(date +%s)
  if ! FS_WGPU_LOG="${FS_WGPU_LOG:-}" "$fsrun" snapshot --backend gpu-f64 --project "$project" --viewer "$viewer" --output "$out/$n-gpu-f64.png" > "$out/$n.log" 2>&1; then
    cat "$out/$n.log"
    if grep -q "has no doubles" "$out/$n.log"; then echo "(no doubles on this GPU)"; continue; fi
    failed=1
    continue
  fi
  echo "gpu-f64 took $(( $(date +%s) - start )) s"
  "$py" "$here/compare-png.py" "$out/$n-cpu.png" "$out/$n-gpu-f64.png" --min-close "$minimum" || failed=1
done
exit $failed
