#!/usr/bin/env bash
# TU3 equivalence proof for the filmstrip swap (card 6e833f9b).
#
# The tuna-native strip (TU1/TU2, server/lib/pages/filmstrip.ml) was green on
# the TU3 gate (72b824b5: the checked-in test, the CDP mobile script, the
# smoke one-liner).  This slice moves the strip's markup and CSS into
# filmstrip-shared; the gate is the safety net: the SAME capture must come out
# of the shared package, and the same contract literals must be present in
# what it renders.
#
# Run:  bash filmstrip-shared/test/gate.sh
# Writes: filmstrip-shared/test/tiles_post_swap.txt
# Exits 0 only when every check below holds.
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
root=$(cd -- "$here/../.." && pwd)
out="$here/tiles_post_swap.txt"

exe=${1:-}
if [ -z "$exe" ]; then
  dune build --root "$root" filmstrip-shared/test/emit.exe
  exe="$root/_build/default/filmstrip-shared/test/emit.exe"
fi
if [ ! -x "$exe" ]; then
  echo "gate: emit executable not found at $exe" >&2
  exit 2
fi

# 1. the checked-in test: the shared package renders the pre-swap capture.
"$exe" > "$out"
diff -u "$here/tiles_pre_swap.txt" "$out"
echo "gate: tiles_post_swap.txt matches tiles_pre_swap.txt"

# 2. the contract literals, read off what the shared package RENDERS (not off
#    a CSS string that nothing emits): .strip-wrap is the scroller, the feed
#    path is on the container, the tiles are anchors with data-glyph.
strip=$(mktemp)
trap 'rm -f "$strip"' EXIT
"$exe" strip > "$strip"
for needle in \
  'class="strip-wrap"' \
  'hx-get="/console/feed/strip"' \
  'data-glyph=' \
  'data-name=' \
  'min-height:44px' \
  'min-width:9rem' \
  'overflow-x:auto' \
  'overflow-x:hidden' \
  '@media (max-width:480px)' \
  'class="tile" href="'
do
  if ! grep -qF -- "$needle" "$strip"; then
    echo "gate: rendered strip is missing: $needle" >&2
    exit 1
  fi
done
echo "gate: rendered strip carries the tile contract (44px, media query, overflow, data attributes)"

# 3. the CDP mobile script, when a server and a browser driver are present.
#    Chromium drives the 390x844 and 1280x800 metrics against a live page, so
#    this half of the gate needs the URL; it is skipped - loudly - when the
#    page is not up, and a real run is NEVER made silent with `|| true`.
url=${TUNA_URL:-http://127.0.0.1:8080}
if command -v node >/dev/null 2>&1 && curl -sf -o /dev/null "$url/"; then
  TUNA_URL="$url" node "$root/tools/filmstrip_cdp.mjs"
  echo "gate: CDP mobile metrics green on $url"
else
  echo "gate: CDP mobile script skipped (node absent or $url not serving)"
fi
