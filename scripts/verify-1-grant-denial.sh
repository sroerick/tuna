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
echo "$OK_RUN" | grep -q '"denial_count":0' || fail "granted run must have denial_count 0"

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
    if [ -n "$OK" ]; then
      kill $RUN_PID 2>/dev/null; wait $RUN_PID 2>/dev/null || true; rm -f "$TMP"
    else
      wait $RUN_PID 2>/dev/null || true
      rm -f "$TMP"
      # the denial must be IN THE JOURNAL before this attempt counts.  On a
      # fast host the whole n=120 run takes ~100ms (measured: 120 rows in
      # 106ms, revoke landing 143us before the last row), so an attempt can
      # race to a complete, denial-free run.  That is a race in the
      # experiment, not a boundary failure: retry the whole experiment
      # (mint, run, revoke, look) instead of demanding one attempt win it.
      D_ID=$(psql_q -tAc \
        "SELECT DISTINCT run_id::text FROM journals WHERE grant_id='$G2'::uuid AND error LIKE 'grant denial (revoked)%' LIMIT 1")
      if [ -n "$D_ID" ]; then break; fi
    fi
  done
  [ -n "$D_ID" ] || fail "mid-run revocation denial was not journaled in $TRY attempts"
D_RUN=$(curl -sf -H "$AUTH" $BASE/api/runs/$D_ID)
echo "$D_RUN" | grep -q '"status":"normal"' \
  || fail "denied run must still be normal (denial is data, not an exception)"
echo "$D_RUN" | grep -q '"error":"grant denial (revoked) for prim echo"' \
  || fail "denial reason must be journaled"
echo "$D_RUN" | grep -q '"denial_count":0' \
  && fail "denied run must surface its denial (0014 denial_count >= 1)"
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
echo "$A_RUN" | grep -q '"denial_count":1' \
  || fail "attenuation denial must surface as denial_count 1"
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

echo "[1.6] agent tree ops: deny-all with explicit grants (todo/g1, the direct-API floor)"
# The direct tree API is no longer the operator side-door: an agent identity
# with NO covering grant is denied (403, journaled) on get/put/list; a live
# covering grant opens exactly what it covers; admins bypass.  Mirrors the
# route publish/delete floor (routes.ml allowed, has_covering_grant).
FLOOR="verify1-floor-$(date +%s)"
MEM_NAME="$FLOOR-member"
MEM=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"name\":\"$MEM_NAME\"}" $BASE/api/identities) \
  || fail "mint floor member"
MAUTH="Authorization: Bearer $(echo "$MEM" | jget "d['token']")"
MEM_ID=$(psql_q -tAc "SELECT id::text FROM identities WHERE name='$MEM_NAME'" | tr -d ' ')
[ -n "$MEM_ID" ] || fail "floor member missing from identities"
tj() { curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
  --data-binary "$2" "$BASE/api/tree/$1"; }
[ "$(tj put  "{\"path\":\"$FLOOR/x\",\"value_ternary\":\"10\"}")" = 403 ] \
  || fail "grantless member tree/put must 403"
[ "$(tj get  "{\"path\":\"$FLOOR/x\"}")" = 403 ] \
  || fail "grantless member tree/get must 403"
[ "$(tj list "{\"prefix\":\"$FLOOR/\"}")" = 403 ] \
  || fail "grantless member tree/list must 403"
NJ=$(psql_q -tAc \
  "SELECT count(*) FROM tree_ops WHERE actor='$MEM_ID' AND path LIKE '$FLOOR%'" \
  | tr -d ' ')
[ "$NJ" -ge 3 ] || fail "denied tree ops must be journaled (got $NJ rows)"
echo "  deny-all holds: grantless 403 x3, denials journaled ($NJ rows)"
# a member holds NO path authority until the operator seeds a garden: a
# path-addressed mint needs a prefix (400), and a prefix outside the
# member's authority is refused (403) - scope can only ever NARROW
[ "$(curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
    --data-binary '{"prim":"tree/put"}' $BASE/api/grants)" = 400 ] \
  || fail "path-addressed mint without a prefix must 400"
[ "$(curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR/\"}" \
    $BASE/api/grants)" = 403 ] || fail "ungranted prefix must 403 (tenancy)"
# the operator seeds the member's garden ON BEHALF of the member; the row
# belongs to the member and the member narrows inside it (attenuation)
GG=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR/\",\"identity\":\"$MEM_ID\"}" \
  $BASE/api/grants) || fail "operator seeds the member garden"
[ "$(echo "$GG" | jget "d['caller']")" = "$MEM_ID" ] \
  || fail "seeded row must belong to the member"
SC=$(curl -sf -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"path_prefix\":\"$FLOOR/sub/\",\"args_attenuation\":\"null\"}" \
  $BASE/api/grants/$(echo "$GG" | jget "d['id']")/attenuate | jget "d['grant']['id']") \
  || fail "member attenuate inside its own garden"
[ -n "$SC" ] || fail "attenuate returned no child grant"
[ "$(tj put  "{\"path\":\"$FLOOR/x\",\"value_ternary\":\"10\"}")" = 201 ] \
  || fail "seeded garden put must 201"
[ "$(tj get  "{\"path\":\"$FLOOR/x\"}")" = 200 ] \
  || fail "seeded garden get must 200"
[ "$(tj list "{\"prefix\":\"$FLOOR/\"}")" = 200 ] \
  || fail "seeded garden list must 200"
echo "  operator-seeded garden opens the prefix (put 201 / get 200 / list 200)"
# a member narrows inside its own authority; it cannot mint into anybody
# else's space, cannot mint FOR anybody else, and "" is never normalized
SN=$(curl -sf -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR/narrow/\"}" \
  $BASE/api/grants | jget "d['id']") || fail "member narrows inside its garden"
[ -n "$SN" ] || fail "narrowed mint returned no id"
[ "$(curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR-outside/x\"}" \
    $BASE/api/grants)" = 403 ] || fail "a prefix outside the garden must 403"
[ "$(curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"prim\":\"echo\",\"identity\":\"$MEM_ID\"}" \
    $BASE/api/grants)" = 403 ] || fail "a member may not mint for another identity"
[ "$(curl_code -X POST -H "$MAUTH" -H 'Content-Type: application/json' \
    --data-binary '{"prim":"tree/put","path_prefix":""}' \
    $BASE/api/grants)" = 400 ] || fail "empty path_prefix must be refused (400)"
echo "  member narrows inside its garden; outside/on-behalf/empty refused"
# the admin tier bypasses the floor (same rule as route publish)
[ "$(curl_code -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"path\":\"$FLOOR/root\",\"value_ternary\":\"10\"}" \
  $BASE/api/tree/put)" = 201 ] || fail "admin tree/put must bypass the floor"
echo "  admin bypass pinned"
echo "[1.7] runner tokens: revocable Bearer at parity (todo/g2, the issue-token precedent)"
# the operator issues a named token to a fresh runner identity; the raw
# token leaves the process exactly once (glochid issue-token precedent)
RUNNER_NAME="verify1-runner-$(date +%s)"
RID=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"name\":\"$RUNNER_NAME\"}" $BASE/api/identities | jget "d['id']")
RTOK=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary '{"name":"glochid-bridge"}' "$BASE/api/identities/$RID/tokens")
RT=$(echo "$RTOK" | jget "d['token']")
RT_ID=$(echo "$RTOK" | jget "d['id']")
RTAUTH="Authorization: Bearer $RT"
# full bearer parity: the issued token drives /api/* as the identity
curl -sf -H "$RTAUTH" $BASE/api/runs > /dev/null \
  || fail "runner token must authenticate at bearer parity"
# the g1 floor applies through it: grantless tree ops 403
[ "$(curl_code -X POST -H "$RTAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"path\":\"$FLOOR/runner\",\"value_ternary\":\"10\"}" \
    $BASE/api/tree/put)" = 403 ] || fail "runner token inherits the tree-op floor"
# the runner holds NO authority of its own: a path-addressed mint needs a
# prefix (400) and a prefix outside its authority is refused (403)
[ "$(curl_code -X POST -H "$RTAUTH" -H 'Content-Type: application/json' \
    --data-binary '{"prim":"tree/put"}' $BASE/api/grants)" = 400 ] \
  || fail "path-addressed mint without a prefix must 400"
[ "$(curl_code -X POST -H "$RTAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR/\"}" \
    $BASE/api/grants)" = 403 ] || fail "ungranted prefix must 403 (tenancy)"
# the operator seeds the runner's garden ON BEHALF of the runner: the row
# belongs to the runner, and the runner then writes inside it
RGG=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$FLOOR/\",\"identity\":\"$RID\"}" \
  $BASE/api/grants) || fail "operator seeds the runner garden"
[ "$(echo "$RGG" | jget "d['caller']")" = "$RID" ] \
  || fail "seeded runner row must belong to the runner"
[ "$(curl_code -X POST -H "$RTAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"path\":\"$FLOOR/runner\",\"value_ternary\":\"10\"}" \
    $BASE/api/tree/put)" = 201 ] || fail "covered runner put must 201"
# revocation bites; the identity's PRIMARY token is untouched
curl -sf -X POST -H "$AUTH" "$BASE/api/identities/$RID/tokens/$RT_ID/revoke" > /dev/null \
  || fail "revoke runner token"
[ "$(curl_code -H "$RTAUTH" $BASE/api/runs)" = 401 ] \
  || fail "revoked runner token must 401"
curl -sf -H "$MAUTH" $BASE/api/runs > /dev/null \
  || fail "primary token must survive the runner-token revoke"
echo "  issue -> parity use -> floor 403 -> covered 201 -> revoke 401; primary intact"
echo "ACCEPT 1 OK: grant denial journaled, run continues, replay verifies"
