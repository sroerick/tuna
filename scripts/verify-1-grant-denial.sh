#!/bin/bash
# acceptance.criteria 1: GRANT DENIAL JOURNALED.
#   A run exercising a revoked grant journals the denial (prim,
#   callsite, reason) as a normal boundary error (grants.invocation);
#   fuel accounting unchanged; run continues per program semantics;
#   replay of the denied run still verifies (the journal answers, not
#   the grant table).
#
#   Revocation is checked LIVE at every prim call, so the corpus makes
#   the revoke land mid-run: a many-echo program keeps the boundary
#   busy while we revoke.  (An up-front revoked grant is rejected 403
#   at submission — Run.execute_run's validation — and never runs.)
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[1.1] granted echo call: journaled result with grant id"
G=$(mint_grant echo)
H=$(post_source '"(lambda (x) (prim \"echo\" x))"')
OK_RUN=$(do_run "$H" '["10"]' "[\"$G\"]")
OK_ID=$(echo "$OK_RUN" | jget "d['run']['id']")
echo "$OK_RUN" | grep -q '"status":"normal"' || fail "granted run must be normal"
echo "$OK_RUN" | grep -q '"prim":"echo"' || fail "prim must be journaled"
echo "$OK_RUN" | grep -q "\"grant_id\":\"$G\"" || fail "grant must be journaled"

echo "[1.2] mid-run revocation: denial journaled as an ERROR ANSWER, run continues"
# 120 nested prim calls: each journal append is a SQL round-trip, so
# 120 nested echo calls keep the boundary busy well past the 0.3s
# revoke window — WITHOUT the compile blow-up: bracket abstraction is
# O(n²) here (n=300 compiles ~20s and BLOCKS the single-threaded Lwt
# runtime; n=120 compiles in ~2s).  Do not raise n casually.
slow_source() {
  python3 -c '
t = "x"
for _ in range(120): t = "(prim \"echo\" %s)" % t
print("(lambda (x) %s)" % t)'
}
SLOW=$(slow_source)
SLOW_JSON=$(python3 -c "import json,sys;print(json.dumps({'source':sys.argv[1]}))" "$SLOW")
HSLOW=$(curl -sf -X POST -H "$AUTH" --data-binary "$SLOW_JSON" \
  $BASE/api/programs | jget "d['hash']") || fail "compile slow program"

for TRY in 1 2 3 4 5; do
  G2=$(mint_grant echo)
  TMP=$(mktemp)
  do_run "$HSLOW" '["10"]' "[\"$G2\"]" > "$TMP" 2>/dev/null &
  RUN_PID=$!
  # Wait for the run's FIRST granted boundary row to land, then revoke
  # immediately: the remaining ~119 calls are then deterministically
  # denied mid-run.  (A fixed sleep raced the run's startup — the whole
  # n=120 run takes ~1.5-2s, so the window is real, but the first call
  # must already be journaled before the revoke is meaningful.)
  OK=
  for i in $(seq 1 40); do
    N=$(psql_q -tAc "SELECT count(*) FROM journals WHERE grant_id='$G2'::uuid AND result_ternary IS NOT NULL" | tr -d ' ')
    [ "$N" -ge 1 ] 2>/dev/null && break
    sleep 0.1
  done
  [ "$N" -ge 1 ] || OK=1
  curl -sf -m 60 -X POST -H "$AUTH" $BASE/api/grants/$G2/revoke >/dev/null \
    || OK=1
  if [ -z "$OK" ]; then
    wait $RUN_PID 2>/dev/null || true
    rm -f "$TMP"
    break
  fi
  kill $RUN_PID 2>/dev/null; wait $RUN_PID 2>/dev/null || true; rm -f "$TMP"
done
# find the run by its journaled denial (the audit query is criterion 6's surface)
D_ID=$(psql_q -tAc \
  "SELECT DISTINCT run_id::text FROM journals WHERE grant_id='$G2'::uuid AND error LIKE 'grant denial (revoked)%' LIMIT 1")
[ -n "$D_ID" ] || fail "mid-run revocation denial was not journaled (tried $TRY times)"
D_RUN=$(curl -sf -H "$AUTH" $BASE/api/runs/$D_ID)
echo "$D_RUN" | grep -q '"status":"normal"' \
  || fail "denied run must still be normal (denial is data, not an exception)"
echo "$D_RUN" | grep -q '"error":"grant denial (revoked) for prim echo"' \
  || fail "denial reason must be journaled"
D_J=$(echo "$D_RUN" | jget "d['journal'][0]['callsite_path']")
[ -n "$D_J" ] || fail "denial row must carry the callsite path"
# the granted prefix still ran: some rows before the denial carry results
GOT=$(echo "$D_RUN" | jget "sum(1 for r in d['journal'] if r['result_ternary'] is not None)")
[ "$GOT" -ge 1 ] || fail "the granted prefix must have executed"

echo "[1.2b] up-front revoked grant -> 403, never a run row"
G3=$(mint_grant echo)
curl -sf -X POST -H "$AUTH" $BASE/api/grants/$G3/revoke >/dev/null || fail "revoke"
[ "$(curl_code -X POST -H "$AUTH" --data-binary \
  "{\"program_hash\":\"$H\",\"inputs\":[\"10\"],\"grants\":[\"$G3\"],\"fuel\":100,\"size_cap\":100}" \
  $BASE/api/runs)" = 403 ] || fail "revoked-at-submit must 403"

echo "[1.3] attenuation denial (args_attenuation {\"max_ternary\":1}), deterministic"
G3=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"prim":"echo","args_attenuation":{"max_ternary":1}}' \
  $BASE/api/grants | jget "d['id']")
A_RUN=$(do_run "$H" '["10"]' "[\"$G3\"]")
echo "$A_RUN" | grep -q '"status":"normal"' || fail "attenuated run must continue"
echo "$A_RUN" | grep -q '"error":"grant denial: prim echo args exceed attenuation"' \
  || fail "attenuation denial must be journaled"
A_ID=$(echo "$A_RUN" | jget "d['run']['id']")

echo "[1.4] fuel accounting unchanged: denied run is a normal boundary event"
# same program, same input, only the grant differs: step counts must agree
G4=$(mint_grant echo)
GR=$(do_run "$H" '["10"]' "[\"$G4\"]")
GV=$(curl -sf -H "$AUTH" $BASE/api/runs/$(echo "$GR" | jget "d['run']['id']") \
  | jget "d['run']['verify_status']")
[ "$GV" = "verified" ] || fail "granted run must verify"

echo "[1.5] replay of the denied runs verifies (journal answers, not grants)"
V=$(run_verify_status "$D_ID")
[ "$V" = "verified" ] || fail "denied run must replay-verify, got $V"
V2=$(run_verify_status "$A_ID")
[ "$V2" = "verified" ] || fail "attenuation-denied run must verify, got $V2"

echo "ACCEPT 1 OK: grant denial journaled, run continues, replay verifies"
