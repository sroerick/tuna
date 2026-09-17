#!/bin/bash
# acceptance.criteria 7: GC TOMBSTONES.
#   GC'ing journals leaves cited tombstones; verifiers report GONE,
#   never VERIFIED (journal.retention-gc).  Redacted rows report
#   ANSWER-UNKNOWN with the chain break visible.
set -e
. "$(dirname "$0")/verify-lib.sh"

G=$(mint_grant echo)
H=$(post_source '"(lambda (x) (prim \"echo\" x))"')

echo "[7.1] GC a verified run's journal via the retention surface"
RUN=$(do_run "$H" '["10"]' "[\"$G\"]")
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")
V=$(run_verify_status "$RUN_ID")
[ "$V" = "verified" ] || fail "run must verify before GC"
NROWS=$(psql_q -tAc "SELECT count(*) FROM journals WHERE run_id='$RUN_ID'")
[ "$NROWS" = "1" ] || fail "expected 1 journal row"
GCR=$(curl -sf -X POST -H "$AUTH" --data-binary '{"policy":"keep-30d"}' \
  $BASE/api/runs/$RUN_ID/gc) || fail "gc call"
echo "$GCR" | grep -q '"status":"normal"' || fail "gc keeps the run row"

echo "[7.2] the tombstone is cited; the verifier reports GONE, never VERIFIED"
TOMB=$(psql_q -tAc \
  "SELECT journal_gced_policy FROM runs WHERE id='$RUN_ID' AND journal_gced_at IS NOT NULL")
[ "$TOMB" = "keep-30d" ] || fail "tombstone must cite the policy, got '$TOMB'"
NROWS2=$(psql_q -tAc "SELECT count(*) FROM journals WHERE run_id='$RUN_ID'")
[ "$NROWS2" = "0" ] || fail "journal rows must be gone"
ST=$(curl -sf -H "$AUTH" $BASE/api/runs/$RUN_ID | jget "d['run']['verify_status']")
[ "$ST" = "gced" ] || fail "verifier must report gced, got $ST"
# re-verify explicitly: the verdict is GONE with the policy cited
GONE=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
echo "$GONE" | grep -q "\"run_id\":\"$RUN_ID\"" || fail "sweep must cover the gc'd run"
echo "$GONE" | python3 -c '
import json, sys
d = json.loads(sys.stdin.read())
v = next(x for x in d["verified"] if x["run_id"] == sys.argv[1])
assert v["verify"] == "gced", f"GONE must never report VERIFIED, got {v}"
assert "retention policy" in v["reason"], "verdict must cite the policy"
print("gc tombstone cites the policy; GONE is not VERIFIED")
' "$RUN_ID" || exit 1

echo "[7.3] redaction = explicit chain break: ANSWER-UNKNOWN, visibly broken"
RED=$(do_run "$H" '["10"]' "[\"$G\"]")
R_ID=$(echo "$RED" | jget "d['run']['id']")
psql_q -q -c "UPDATE journals SET result_ternary=NULL, result_hash=NULL, \
  error='redacted (chain break): pii-scrub' WHERE run_id='$R_ID' AND seq=0" >/dev/null
psql_q -q -c "UPDATE runs SET verify_status=NULL, verified_at=NULL WHERE id='$R_ID'" >/dev/null
V2=$(run_verify_status "$R_ID")
[ "$V2" = "failed" ] || fail "redacted run must report ANSWER-UNKNOWN (failed), got $V2"
BREAK=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
echo "$BREAK" | python3 -c '
import json, sys
d = json.loads(sys.stdin.read())
v = next(x for x in d["verified"] if x["run_id"] == sys.argv[1])
assert v["verify"] == "failed", "redaction must break verification"
assert "chain broken" in v.get("reason", ""), "the chain break must be visible"
print("redaction is a visible chain break (ANSWER-UNKNOWN)")
' "$R_ID" || exit 1

echo "ACCEPT 7 OK: gc leaves cited tombstones (GONE never VERIFIED); redaction breaks visibly"
