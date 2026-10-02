#!/bin/bash
# acceptance 11 (borg/replay.borg §live + §prim-versioning):
#   LIVE REPLAY + PRIM CONTRACT VERSIONING.
#
#   Live replay re-executes against the CURRENT world under fresh grants.
#   It mints its OWN run row with parent_run_id = the original, and is
#   NEVER verification state (the new row is unverified).  The world diff
#   between original and live journal is a journal-versus-journal
#   comparison; a contract mismatch (row pinned an older prim contract
#   than this build) is reported first-class, distinguishable from a
#   regression.  Faithful replay stays unconditional either way.
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[11.1] live replay mints a linked, UNVERIFIED row + world diff"
G=$(mint_grant echo)
H=$(post_source '"(lambda (x) (prim \"echo\" x))"')

RUN=$(do_run "$H" '["10"]' "[\"$G\"]") || fail "original echo run"
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")

# live replay with the SAME recorded world: no diff, still a NEW row
LIVE=$(curl -sf -m 300 -X POST -H "$AUTH" --data-binary "{\"grants\":[\"$G\"]}" \
  $BASE/api/runs/$RUN_ID/live-replay) || fail "live-replay endpoint"
LIVE_ID=$(echo "$LIVE" | jget "d['run']['id']")
[ "$LIVE_ID" != "$RUN_ID" ] || fail "live replay must mint a new run row"
[ "$(echo "$LIVE" | jget "d['run']['parent_run_id']")" = "$RUN_ID" ] \
  || fail "live row must link parent_run_id to the original"
[ "$(echo "$LIVE" | jget "d['run']['verify_status']")" = "None" ] \
  || fail "live replay must never write verification state"
echo "  parent=$RUN_ID live=$LIVE_ID unverified, linked"

echo "[11.2] a changed world names the changed seq in the world diff"
# store/get is deterministic per journal; to force a world change we
# edit the RECORDED journal row (the recorded-world answer) and live-
# replay under a fresh grant.  Live replay re-executes the same program;
# store/get reads the CURRENT prim_kv, which we move between the two.
G2=$(mint_grant store/get)
H2=$(post_source '"(lambda (x) (prim \"store/get\" x))"')
# key_hash = sha256 of the canonical key ternary "10" (prim_kv PK)
KEYH=$(printf '10' | sha256sum | cut -d' ' -f1)
psql_q -q -c "INSERT INTO prim_kv (key_hash, key_ternary, value_ternary) \
  VALUES ('$KEYH', '10', '0') ON CONFLICT (key_hash) \
  DO UPDATE SET value_ternary = '0'" >/dev/null
RUN2=$(do_run "$H2" '["10"]' "[\"$G2\"]") || fail "original store/get run"
R2_ID=$(echo "$RUN2" | jget "d['run']['id']")
[ "$(echo "$RUN2" | jget "d['run']['result_ternary']")" = "0" ] \
  || fail "recorded world value must be 0"
# the world moves on
psql_q -q -c "UPDATE prim_kv SET value_ternary = '10' WHERE key_hash = '$KEYH'" >/dev/null
LIVE2=$(curl -sf -m 300 -X POST -H "$AUTH" --data-binary "{\"grants\":[\"$G2\"]}" \
  $BASE/api/runs/$R2_ID/live-replay) || fail "live-replay (changed world)"
[ "$(echo "$LIVE2" | jget "d['world_diff']['seq']")" = "0" ] \
  || fail "world diff must name seq 0"
[ "$(echo "$LIVE2" | jget "d['world_diff']['kind']")" = "result_hash" ] \
  || fail "world diff kind must be result_hash"
[ "$(echo "$LIVE2" | jget "d['world_diff']['prim']")" = "store/get" ] \
  || fail "world diff must name the prim"
echo "  world diff: seq 0 result_hash on store/get"

echo "[11.3] denied-grant live replay journals the denial (never raises)"
LIVE3=$(curl -sf -m 300 -X POST -H "$AUTH" --data-binary '{"grants":[]}' \
  $BASE/api/runs/$R2_ID/live-replay) || fail "denied live replay must answer 201"
[ "$(echo "$LIVE3" | jget "d['run']['status']")" = "normal" ] \
  || fail "denied live replay must still finish (denial is data)"
echo "$LIVE3" | jget "d['journal'][0]['error']" | grep -qi 'grant denial' \
  || fail "denial must be journaled"
echo "  denial journaled, run normal"

echo "[11.4] contract mismatch is first-class on the divergence surface"
# a divergent fork (hand-derived corpus from verify-5) carries the
# contract fields as first-class JSON, so an upgrade is distinguishable
# from a regression.  Current-contract rows report no mismatch.
H3=$(post_source '"(lambda (x) (prim \"echo\" (prim \"echo\" x)))"')
RUN3=$(do_run "$H3" '["10"]' "[\"$G\"]")
R3_ID=$(echo "$RUN3" | jget "d['run']['id']")
FORKV=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"0"}]}' \
  $BASE/api/journals/$R3_ID/fork) || fail "divergent fork probe"
[ "$(echo "$FORKV" | jget "d['verify']['verify']")" = "failed" ] \
  || fail "fork must diverge"
echo "$FORKV" | grep -q '"contract_mismatch"' \
  || fail "divergence JSON must carry contract_mismatch as a first-class field"
echo "$FORKV" | grep -q '"current_contract"' \
  || fail "divergence JSON must name the current contract"
[ "$(echo "$LIVE2" | jget "len(d['contract_mismatches'])")" = "0" ] \
  || fail "current-contract rows must not report a mismatch"
echo "  contract fields first-class; current rows clean"

echo "ACCEPT 11 OK: live replay is a linked unverified debugging run with a journal world-diff; contract mismatch is first-class"
