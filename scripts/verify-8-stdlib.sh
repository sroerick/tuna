#!/bin/bash
# acceptance 8 (borg/stdlib.borg): sabra stdlib v1.
#   8.1 MANIFEST DRIFT  - recompute name -> size -> program hash for the
#      sabralib repl_dict from the live store, diff against the frozen
#      stdlib/v1/manifest; exit 1 on any drift.
#   8.2 RESOLUTION ORDER (parse time): lambda param > identity
#      repl_dict > sabralib repl_dict > builtins - probed live through
#      POST /api/repl (fallback, shadow-wins, param-beats-shadow,
#      undef-restores).  NOTE: this probe undefs/re-defs the ROOT
#      identity's local `not`; undef at the end restores the std view.
#   8.3 STD VOCABULARY through the fallback: list-length + int-add +
#      tree-eq answer correctly with an empty local dict.
#   8.4 SEEDING is attributed + additive: >=53 sabralib def rounds in
#      runs, and the row count stays 53 across a re-boot-shaped state
#      (skip-if-exists is boot-tested, here asserted as count==53).
#
# Requires: live dev server (scripts/dev.sh start), the sabralib
# dictionary seeded, psql, python3. Env via verify-lib.sh (set
# TUNA_HTTP_PORT if the server is not on 18090).
set -e
. "$(dirname "$0")/verify-lib.sh"

MANIFEST="$(dirname "$0")/../stdlib/v1/manifest"
[ -f "$MANIFEST" ] || fail "missing stdlib/v1/manifest"

repl() {
  curl -sf -m 300 -X POST -H "$AUTH" -H "Content-Type: application/json" \
    --data-binary "$1" "$BASE/api/repl"
}
repl_field() { jget "d['round']['$2']" <<<"$1"; }

echo "[8.1] manifest drift: recompute from repl_dict(sabralib), diff"
ROWS=$(mktemp)
trap 'rm -f "$ROWS"' EXIT
psql_q -At -F'|' -c \
  "SELECT d.name, d.ternary FROM repl_dict d
   JOIN identities i ON i.id=d.identity_id
   WHERE i.name='sabralib' ORDER BY d.name" > "$ROWS"
python3 - "$MANIFEST" "$ROWS" <<'PYEOF' || fail "manifest drift detected (see above)"
import sys, hashlib

def parse(s, i=0):
    c = s[i]; i += 1
    if c == '0':
        return 1, i
    if c == '1':
        n, i = parse(s, i)
        return n + 1, i
    n1, i = parse(s, i)
    n2, i = parse(s, i)
    return 1 + n1 + n2, i

live = {}
for line in open(sys.argv[2]):
    name, ternary = line.rstrip("\n").split("|", 1)
    size, off = parse(ternary)
    if off != len(ternary):
        sys.exit(f"unbalanced ternary for {name}")
    live[name] = (size, hashlib.sha256(ternary.encode()).hexdigest())

manifest = {}
for line in open(sys.argv[1]):
    name, size, h = line.split()
    manifest[name] = (int(size), h)

drift = []
for name, (size, h) in sorted(manifest.items()):
    if name not in live:
        drift.append(f"missing in store: {name}")
    elif live[name] != (size, h):
        drift.append(f"{name}: manifest {size}/{h[:12]} != store {live[name][0]}/{live[name][1][:12]}")
for name in sorted(live):
    if name not in manifest:
        drift.append(f"extra in store: {name}")
if drift:
    print("\n".join(drift))
    sys.exit(1)
print(f"manifest green: {len(manifest)} defs match store exactly")
PYEOF

echo "[8.2] resolution order"
# safety: end state must be "no local not on root" regardless of entry state
TERM_SHADOW=$(repl '{"command": "def not (lambda (b) %1110)"}')
[ "$(repl_field "$TERM_SHADOW" note)" = "defined not" ] || fail "shadow def rejected"

OUT=$(repl '{"command": "(not %10)"}')
[ "$(repl_field "$OUT" ternary)" = "1110" ] || \
  fail "identity shadow must win over sabralib (got $(repl_field "$OUT" ternary))"

OUT=$(repl '{"command": "(lambda (not) (not %10))", "inputs": ["22102000"]}')
[ "$(repl_field "$OUT" ternary)" = "0" ] || \
  fail "lambda param must win over identity shadow (got $(repl_field "$OUT" ternary))"

repl '{"command": "undef not"}' >/dev/null
OUT=$(repl '{"command": "(not %10)"}')
[ "$(repl_field "$OUT" ternary)" = "0" ] || \
  fail "undef must restore the sabralib view (got $(repl_field "$OUT" ternary))"

OUT=$(repl '{"command": "(not %0)"}')
[ "$(repl_field "$OUT" ternary)" = "10" ] || \
  fail "sabralib fallback must answer (not leaf = stem-leaf) (got $(repl_field "$OUT" ternary))"
echo "[8.2] fallback / shadow / param / undef-restore all green"

echo "[8.3] std vocabulary through the fallback"
OUT=$(repl '{"command": "(list-length %202100)"}')
[ "$(repl_field "$OUT" ternary)" = "110" ] || \
  fail "list-length [f,t] must be 2 (got $(repl_field "$OUT" ternary))"
OUT=$(repl '{"command": "(int-add %202100 %20202100)"}')
[ "$(repl_field "$OUT" ternary)" = "202102100" ] || \
  fail "int-add 1+2 must be +3 (got $(repl_field "$OUT" ternary))"
OUT=$(repl '{"command": "(tree-eq %22102000 %22102000)"}')
[ "$(repl_field "$OUT" ternary)" = "10" ] || \
  fail "tree-eq on not trees must be true (got $(repl_field "$OUT" ternary))"
echo "[8.3] list-length / int-add / tree-eq green"

echo "[8.4] seeding attributed + additive"
COUNT=$(psql_q -At -c \
  "SELECT count(*) FROM repl_dict d
   JOIN identities i ON i.id=d.identity_id WHERE i.name='sabralib'")
MF_COUNT=$(wc -l < "$MANIFEST")
[ "$COUNT" = "$MF_COUNT" ] || \
  fail "sabralib row count $COUNT != manifest $MF_COUNT"
ROUNDS=$(psql_q -At -c \
  "SELECT count(*) FROM runs r
   JOIN identities i ON i.id=r.caller WHERE i.name='sabralib'")
[ "$ROUNDS" -ge "$MF_COUNT" ] || \
  fail "expected >= $MF_COUNT sabralib-attributed def rounds, got $ROUNDS"
echo "[8.4] sabralib rows=$COUNT, attributed rounds=$ROUNDS (>= $MF_COUNT)"

echo "[8.5] ledger bridge: posting list -> balance by the int core"
# postings [+2, -3, +1, -1] -> balance -1 ... small (F6: <=2-bit mags)
#   +2 = 20202100, -3 = 2102102100, +1 = 202100, -1 = 2102100
# the list (cons-as-fork): [+2, -3, +1, -1]
POSTINGS="2202021002210210210022021002102100"  # placeholder, computed below
POSTINGS=$(python3 - <<'PYEOF'
def cons(h, t): return "2" + h + t
leaf = "0"
l = cons("20202100", cons("2102102100", cons("202100", cons("2102100", leaf))))
print(l)
PYEOF
)
OUT=$(repl "$(python3 - "$POSTINGS" <<'PYEOF'
import json, sys
print(json.dumps({"command": "(lambda (w) (list-fold int-add int-zero w))",
                  "inputs": [sys.argv[1]]}))
PYEOF
)")
STATUS=$(repl_field "$OUT" status)
[ "$STATUS" = "normal" ] || fail "balance run must be normal (got $STATUS)"
BAL=$(repl_field "$OUT" ternary)
[ "$BAL" = "2102100" ] || fail "balance must be -1 (got $BAL)"
RUN_ID=$(repl_field "$OUT" run_id)
V=$(run_verify_status "$RUN_ID")
[ "$V" = "verified" ] || fail "balance run must replay-verify (got $V)"
echo "[8.5] balance = $BAL, replay $V; deriv seal next"
STEPS=$(repl_field "$OUT" steps)

eval "$OPAM_ENV" 2>/dev/null || true
dune build tools/deriv-check/deriv_check.exe 2>/dev/null || \
  fail "could not build deriv-check"
DC=_build/default/tools/deriv-check/deriv_check.exe
curl -sf -m 300 -H "$AUTH" "$BASE/api/runs/$RUN_ID/deriv" | $DC - >/dev/null \
  && echo "[8.5] deriv-check exit 0 (sealed receipt verified offline)" \
  || fail "deriv-check failed on the balance run (steps=$STEPS)"

echo "[8.6] core untouched (acceptance 6)"
BASELINE="${TUNA_STDLIB_BASELINE:-0185b1a}"
if git diff --quiet "$BASELINE"..HEAD -- interpreter compiler common 2>/dev/null; then
  echo "[8.6] interpreter/compiler/common byte-identical since $BASELINE"
else
  git diff --stat "$BASELINE"..HEAD -- interpreter compiler common || true
  fail "core touched since $BASELINE"
fi

echo "verify-8-stdlib: green"
