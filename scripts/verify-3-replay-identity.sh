#!/bin/bash
# acceptance.criteria 3: REPLAY IDENTITY.
#   Faithful replay of every stored corpus run reproduces result hash
#   AND step count; a store-side sweeper re-verifies on schedule and
#   failures open drift, not silence (replay.faithful).
set -e
. "$(dirname "$0")/verify-lib.sh"

NOT_HASH=$(post_ternary "22102000")
OMEGA_HASH=$(post_ternary "221000")

echo "[3.1] pure corpus: not(true) = leaf in 2 steps, replay-verified"
RUN=$(do_run "$NOT_HASH" '["10"]' "[]")
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")
echo "$RUN" | grep -q '"step_count":2' || fail "not(true) must take exactly 2 steps"
echo "$RUN" | grep -q '"status":"normal"' || fail "not(true) must be normal"
V=$(run_verify_status "$RUN_ID")
[ "$V" = "verified" ] || fail "not(true) run must verify, got $V"

echo "[3.2] fuel-exact corpus: omega halts at exactly fuel, replay-verified"
RUN2=$(do_run "$OMEGA_HASH" '["221000","221000","221000","221000","221000","221000"]' "[]" 5)
RUN2_ID=$(echo "$RUN2" | jget "d['run']['id']")
echo "$RUN2" | grep -q '"step_count":5' || fail "omega must stop at exactly fuel"
echo "$RUN2" | grep -q '"status":"fuel_exhausted"' || fail "omega must fuel_exhaust"
V2=$(run_verify_status "$RUN2_ID")
[ "$V2" = "verified" ] || fail "omega run must verify, got $V2"

echo "[3.3] effectful corpus: echo run replay-verified"
G=$(mint_grant echo)
H=$(post_source '"(lambda (x) (prim \"echo\" x))"')
RUN3=$(do_run "$H" '["10"]' "[\"$G\"]")
RUN3_ID=$(echo "$RUN3" | jget "d['run']['id']")
V3=$(run_verify_status "$RUN3_ID")
[ "$V3" = "verified" ] || fail "echo run must verify, got $V3"

echo "[3.4] the sweeper re-verifies on schedule (all three included)"
SWEEP=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
for id in "$RUN_ID" "$RUN2_ID" "$RUN3_ID"; do
  echo "$SWEEP" | grep -q "\"run_id\":\"$id\"" || fail "sweeper must cover run $id"
done
# JSON passed as argv: a `python3 - <<EOF` heredoc would eat stdin (the
# heredoc redirect wins over the pipe — json.load(sys.stdin) then reads
# the consumed heredoc, i.e. nothing).
echo "$SWEEP" | python3 -c '
import json, sys
d = json.loads(sys.stdin.read())
ids = sys.argv[1].split()
by = {v["run_id"]: v for v in d["verified"]}
for i in ids:
    assert by.get(i, {}).get("verify") == "verified", f"sweeper must verify {i}"
print("sweeper verified all three")
' "$RUN_ID $RUN2_ID $RUN3_ID" || exit 1

echo "[3.5] failures open drift, not silence: a tampered journal fails LOUDLY"
TAMPER=$(do_run "$H" '["10"]' "[\"$G\"]")
T_ID=$(echo "$TAMPER" | jget "d['run']['id']")
psql_q -q -c "UPDATE journals SET result_ternary='0' WHERE run_id='$T_ID' AND seq=0" >/dev/null
TS=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
echo "$TS" | grep -q '"verify":"failed"' || fail "tampered run must fail in the sweep"
TV=$(curl -sf -H "$AUTH" $BASE/api/runs/$T_ID | jget "d['run']['verify_status']")
[ "$TV" = "failed" ] || fail "tampered run's recorded status must flip to failed"

echo "ACCEPT 3 OK: replay identity holds; the sweeper re-verifies; tampering opens drift"
