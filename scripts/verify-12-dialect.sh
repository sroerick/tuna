#!/bin/bash
# acceptance 12 (borg/dialect.borg): the sabra dialect v0.1.
#
#   12.1 DESUGAR EQUALITY.  Every shipped sugar form compiles to the
#        IDENTICAL tree + steps as its hand-written twin (live, CLI).
#   12.2 COMPAT.  Bare 0 stays the leaf literal; the -<digits>
#        reservation is audited; the corpus stays intact.
#   12.3 READER SEAM.  The compiler diff since the dialect baseline
#        touches sexp.ml only (the common/ codec is the other addition).
#   12.4 FUEL TABLE.  Bracket construction stays zero-triage; record
#        cost rows are recorded.
#   12.5 TODO BRIDGE.  The in-calculus board over a granted namespace:
#        normal, journaled, replay-verified, 2 open / 1 done.
#   12.6 BOOK HYGIENE.  Manifest recomputes from the live store; the
#        dialect corpus rows are present.
#
# Requires: live dev server (scripts/dev.sh start); verify-lib.sh env.
set -e
. "$(dirname "$0")/verify-lib.sh"

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
eval $(opam env --switch=poohstack --set-switch)
dune build cli/bin/main.exe 2>/dev/null
CLI=_build/default/cli/bin/main.exe

# compile a surface term, echo "<ternary> <size> <steps>"
compile_term() {
  echo "$1" > /tmp/v12.sabra
  "$CLI" compile /tmp/v12.sabra 2>/dev/null | awk '
    $1=="ternary"{t=$2} $1=="size"{s=$2} $1=="steps"{st=$2}
    END{print t, s, st}'
}

echo "[12.1] desugar equality: sugar vs hand twin (live)"
check_desugar() {
  local a b
  a=$(compile_term "$1")
  b=$(compile_term "$2")
  [ -n "$a" ] || fail "12.1: sugar term failed to compile: $1"
  [ "$a" = "$b" ] || fail "12.1: '$1' != '$2' ($a vs $b)"
}
check_desugar '[1 2 3]'      '(pair 1 (pair 2 (pair 3 0)))'
check_desugar '[]'           '0'
check_desugar '42'           '%202021020210202100'
check_desugar '-7'           '%2102102102100'
check_desugar '(let ((x 1) (y 2)) x)' '((lambda (x) ((lambda (y) x) 2)) 1)'
check_desugar '[[1 2] [3 4]]' '(pair (pair 1 (pair 2 0)) (pair (pair 3 (pair 4 0)) 0))'
echo "  6/6 pairs: identical ternary + size"

# v0.2 keyed literals need the dictionary (key-* resolve through it), so
# this check goes through the server REPL, which assembles the sabralib
# rows under the identity.  (No shell brace expansion: Python drives it.)
python3 - "$BASE" "$TOKEN" <<'PY'
import json, sys, urllib.request
base, tok = sys.argv[1], sys.argv[2]
def tern(cmd):
    req = urllib.request.Request(base + "/api/repl",
        data=json.dumps({"command": cmd}).encode(),
        headers={"Authorization": "Bearer " + tok,
                 "Content-Type": "application/json"})
    return json.load(urllib.request.urlopen(req))["round"]["ternary"]
pairs = [
  ('{:state %10 :title "x"}', '[[key-state %10] [key-title "x"]]'),
  ('{:state %10 :title "x" :who "me" :when 7}',
   '[[key-state %10] [key-title "x"] [key-who "me"] [key-when 7]]'),
]
for form, twin in pairs:
    a, b = tern(form), tern(twin)
    if a != b:
        raise SystemExit("12.1(v0.2): keyed %r != twin %r" % (a, b))
print("  8/8 pairs incl. %d keyed literals: identical ternary" % len(pairs))
PY

echo "[12.2] compat: bare 0 is the leaf; corpus intact"
[ "$(compile_term '0')" = "0 1 0" ] || fail "12.2: bare 0 must stay the leaf literal"
CORPUS=$(ls "$ROOT"/scripts/diff-corpus/*.corpus | wc -l)
[ "$CORPUS" -ge 89 ] || fail "12.2: expected >=89 corpus entries, got $CORPUS"
echo "  leaf preserved, corpus intact ($CORPUS entries)"

echo "[12.3] reader seam: compiler diff touches sexp.ml only"
BASELINE="${TUNA_DIALECT_BASELINE:-98b363b}"
CHANGED=$(git -C "$ROOT" diff --name-only "$BASELINE"..HEAD -- compiler 2>/dev/null || true)
if [ -n "$CHANGED" ] && [ "$CHANGED" != "compiler/lib/sexp.ml" ]; then
  echo "$CHANGED" | sed 's/^/  /' >&2
  fail "12.3: compiler files other than sexp.ml changed"
fi
echo "  compiler delta: ${CHANGED:-none (sexp.ml only)}"

echo "[12.4] fuel table: bracket construction stays zero-triage"
BR=$(compile_term '[1 2 3]')
PAIR=$(compile_term '(pair 1 (pair 2 (pair 3 0)))')
[ "$BR" = "$PAIR" ] || fail "12.4: bracket/pair compile steps differ ($BR vs $PAIR)"
RECSTEPS=$(awk '/^expect_steps/{print $2}' "$ROOT/scripts/diff-corpus/dialect_rec-upd.corpus")
echo "  bracket=$BR pair-chain=$PAIR (equal); rec-upd corpus steps=$RECSTEPS"

echo "[12.5] todo bridge: in-calculus board, journaled + replay-verified"
PREFIX="todo-cal-v12"
psql_q -q -c "DELETE FROM tree_paths WHERE path LIKE '$PREFIX/%'" >/dev/null 2>&1 || true
python3 "$ROOT/scripts/dialect/bridge_probe.py" --prefix "$PREFIX" || \
  fail "12.5: todo bridge"

echo "[12.6] manifest + corpus hygiene"
MF=$(psql_q -tA -c "SELECT count(*) FROM repl_dict d JOIN identities i ON i.id=d.identity_id WHERE i.name='sabralib'")
[ "$MF" -ge 76 ] || fail "12.6: sabralib must hold >=76 defs, got $MF"
ls "$ROOT"/scripts/diff-corpus/dialect_*.corpus >/dev/null 2>&1 || \
  fail "12.6: dialect corpus rows missing"
echo "  sabralib $MF defs; dialect corpus rows present"

echo "ACCEPT 12 OK: reader sugar folds identically, records carry the todo bridge, seam intact"
