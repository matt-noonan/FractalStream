#!/usr/bin/env bash
# Patch the JavaScript glue GHC's post-link.mjs writes for a wasm module,
# so that it works with more than 2 GB of memory: GHC's own string
# conversions (toJSString and fromJSString, in ghc-internal's
# GHC.Internal.Wasm.Prim.Types) view the module's memory at an address
# that JavaScript reads as a signed 32-bit number, which is negative past
# 2 GB ("Start offset ... is outside the bounds of the buffer"). Our own
# imports read their addresses unsigned (`$N >>> 0`); this does the same
# for GHC's. It fails if the glue doesn't have the views it expects (a
# new GHC may have changed them, or fixed this), and if any view of the
# memory is still at an address read signed (an import of ours written
# without `>>> 0`).
#
#   dev-tools/wasm/fix-glue.sh GLUE.js
set -euo pipefail
glue="$1"
before='new Uint8Array(__exports.memory.buffer, $1, $2)'
before2='new Uint8Array(__exports.memory.buffer, $2, $3)'
if ! grep -qF "$before" "$glue" || ! grep -qF "$before2" "$glue"; then
  echo "fix-glue.sh: $glue doesn't have GHC's string views to patch; check them by hand" >&2
  exit 1
fi
sed -i.orig \
  -e 's/new Uint8Array(__exports\.memory\.buffer, \$1, \$2)/new Uint8Array(__exports.memory.buffer, ($1 >>> 0), $2)/g' \
  -e 's/new Uint8Array(__exports\.memory\.buffer, \$2, \$3)/new Uint8Array(__exports.memory.buffer, ($2 >>> 0), $3)/g' \
  "$glue"
rm -f "$glue.orig"
# (every view of the memory at an unsigned address)
if grep -Eo '(memory\.buffer|\(m), \$[0-9]+' "$glue" | grep -q .; then
  echo "fix-glue.sh: $glue views memory at an address read signed (write it as (\$N >>> 0)):" >&2
  grep -Eo '.{60}(memory\.buffer|\(m), \$[0-9]+.{20}' "$glue" >&2
  exit 1
fi
