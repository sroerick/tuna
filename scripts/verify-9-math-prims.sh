#!/bin/bash
# acceptance (borg/math-prims.borg): the journaled math prim family.
#   9.1 BOUNDARY: math/add through the run boundary under a live grant:
#       result correct, journal row carries prim + contract "2" + grant
#       id, replay verified. Same balance-aggregate shape as the stdlib
#       ledger bridge - answered by ONE journal row.
#   9.2 GRANT LAW: no grant -> journaled denial error, run continues
#       (grants.invocation; AGENTS 7); replay still verifies (journal
#       answers, not the grant table).
#   9.3 FUEL CONTRAST OUTCOME: a 64-bit math/add's journaled wall_ms,
#       recorded next to the pure-v0 reference (11,106,917 steps; F6).
#
# Requires: live dev server (scripts/dev.sh start); verify-lib.sh env.
set -e
. "$(dirname "$0")/verify-lib.sh"

mkint() { # sign(0|1) bits-lsb-first -> raw int tree
  python3 -c "
import sys
sign, bs = sys.argv[1], sys.argv[2]
mag = '0'
for c in reversed(bs): mag = '2' + ('10' if c == '1' else '0') + mag
print('2' + ('10' if sign == '1' else '0') + mag)" "$1" "$2"
}
P2=$(mkint 0 01)          # +2
M5=$(mkint 1 101)         # -5
M3="2102102100"           # -3
ONES64=$(python3 -c 'print("1"*64)')
TWO63=$(python3 -c 'print("0"*63+"1")')

echo "[9.1] boundary: math/add under grant, journal + replay"
G=$(mint_grant math/add)
H=$(post_source '"(lambda (a b) (prim \"math/add\" a b))"')
RUN=$(do_run "$H" "[\"$P2\",\"$M5\"]" "[\"$G\"]")
RID=$(echo "$RUN" | jget "d['run']['id']")
echo "$RUN" | grep -q '"status":"normal"' || fail "math/add run must be normal"
echo "$RUN" | grep -q "\"result_ternary\":\"$M3\"" || \
  fail "math/add (+2)+(-5) must be -3 (got: $(echo "$RUN" | head -c 300))"
echo "$RUN" | grep -q '"prim":"math/add"' || fail "math/add must be journaled"
echo "$RUN" | grep -q '"prim_contract":"2"' || fail "journal must pin contract 2"
echo "$RUN" | grep -q "\"grant_id\":\"$G\"" || fail "grant id must be journaled"
V=$(run_verify_status "$RID")
[ "$V" = "verified" ] || fail "math/add run must replay-verify (got $V)"

echo "[9.2] grant law: no grant -> journaled denial, run continues, replay verifies"
RUN2=$(do_run "$H" "[\"$P2\",\"$M5\"]" "[]")
echo "$RUN2" | grep -q '"status":"normal"' || fail "denied run must still be normal"
echo "$RUN2" | grep -q "grant denial: no live grant for prim math/add" || \
  fail "denial must be journaled (got: $(echo "$RUN2" | head -c 300))"
RID2=$(echo "$RUN2" | jget "d['run']['id']")
V2=$(run_verify_status "$RID2")
[ "$V2" = "verified" ] || fail "denied run must replay-verify (got $V2)"

echo "[9.3] fuel contrast: 64-bit math/add journaled wall vs pure-v0 steps"
G3=$(mint_grant math/add)
RUN3=$(do_run "$H" "[\"$(mkint 0 "$ONES64")\",\"$(mkint 0 "$TWO63")\"]" "[\"$G3\"]")
echo "$RUN3" | grep -q '"status":"normal"' || fail "64-bit math/add must be normal"
RID3=$(echo "$RUN3" | jget "d['run']['id']")
WALL=$(curl -sf -H "$AUTH" "$BASE/api/journals/$RID3" | \
  python3 -c 'import json,sys
d=json.load(sys.stdin)
rows=d["journal"] if isinstance(d,dict) else d
print(next((r.get("wall_ms") for r in rows if r.get("prim")=="math/add"), -1))')
[ "$WALL" != "-1" ] || fail "math/add journal row must carry wall_ms"
echo "[9.3] math/add 64-bit wall_ms=$WALL; pure v0 reference = 11,106,917 steps (F6)"

echo "verify-9-math-prims: green"
