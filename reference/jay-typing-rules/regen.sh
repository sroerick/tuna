#!/usr/bin/env bash
# Regenerate trees + vectors from Jay's Rocq development at the pinned
# commit.  Needs rocq (poohstack has 9.1.1; Jay targets 9.3) and ~1 GB of
# disk outside /tmp (the .vo files are large; /tmp has a user quota).
# Takes ~40 min cold, mostly Jay's proofs.  Not part of the build: the
# committed trees/vectors are the artifact, this script is provenance.
set -euo pipefail
COMMIT=ddbab88b57cd1a010188dafd5666fd8f710671c2
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${JAY_SRC:-$HOME/.cache/claude-jay-trp}
if [ ! -d "$SRC/.git" ]; then
  git clone -q https://github.com/barry-jay-personal/typing-rules-as-programs "$SRC"
fi
git -C "$SRC" fetch -q origin "$COMMIT" 2>/dev/null || true
git -C "$SRC" checkout -q "$COMMIT"
(cd "$SRC" && rocq makefile -f _CoqProject -o Makefile >/dev/null && make -f Makefile -j4 lists.vo)
WORK=$(mktemp -d "${TMPDIR:-/tmp}/jay-export.XXXX")
trap 'rm -rf "$WORK"' EXIT
cp "$HERE"/export/*.v "$WORK"/
(cd "$WORK" && for v in ExportBool ExportLists; do
   rocq compile -Q "$SRC" BigStep "$v.v" >/dev/null 2>&1 || { echo "rocq failed on $v.v" >&2; exit 1; }
 done)
python3 -I "$HERE/convert.py" "$HERE" bool="$WORK/bool_vectors.out" lists="$WORK/list_vectors.out"
