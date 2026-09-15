#!/usr/bin/env bash
# verify-differential.sh — M3 differential harness.
#
# For every scripts/diff-corpus/*.corpus entry, evaluates program+args
# under fuel/size_cap with three independent engines and requires all
# three to match the checked-in expect lines exactly:
#   1. tools/gen refeval — instrumented verbatim copy of upstream apply
#      (the generator that produced the expect lines),
#   2. reference/tree-calc.lisp — Common Lisp reference twin (sbcl),
#   3. the tuna CLI evaluator (cli/bin/main.exe eval).
# Exit 0 on green; exit 1 on the first mismatch, printing all three
# outputs and the expected line.
set -uo pipefail
cd "$(dirname "$0")/.."

eval $(opam env --switch=poohstack --set-switch)
command -v sbcl >/dev/null || { echo "FATAL: sbcl not found (needed for the CL reference twin)" >&2; exit 2; }
dune build tools/gen/gen.exe cli/bin/main.exe 2>/dev/null

GEN=_build/default/tools/gen/gen.exe
CLI=_build/default/cli/bin/main.exe
fails=0
total=0

for f in scripts/diff-corpus/*.corpus; do
  total=$((total + 1))
  name=$(head -1 "$f" | cut -d' ' -f2)
  estatus=$(awk '/^expect_status/{print $2}' "$f")
  eresult=$(awk '/^expect_result/{print $2}' "$f")
  esteps=$(awk '/^expect_steps/{print $2}' "$f")
  expected="$estatus $eresult $esteps"
  refeval=$("$GEN" refeval "$f" 2>/dev/null)
  cl=$(sbcl --script reference/tree-calc.lisp "$f" 2>/dev/null)
  tuna=$("$CLI" eval "$f" 2>/dev/null)
  if [ "$refeval" != "$expected" ] || [ "$cl" != "$expected" ] || [ "$tuna" != "$expected" ]; then
    echo "FAIL $name ($f)"
    echo "  expected: $expected"
    echo "  refeval:  $refeval"
    echo "  cl:       $cl"
    echo "  tuna:     $tuna"
    fails=$((fails + 1))
  fi
done

if [ "$fails" -gt 0 ]; then
  echo "differential: $fails/$total corpus entries FAILED" >&2
  exit 1
fi
echo "differential: all $total corpus entries agree (refeval == CL twin == tuna CLI)"
