#!/bin/bash
# acceptance.criteria 4: JOURNAL COMPLETENESS + counterfactual precision.
#   For the effectful sample program, replaying the pure core over the
#   recorded journal reproduces the effects exactly (SPEC.md 6.5) —
#   and the counterfactual variant holds: replacing one journal answer
#   changes precisely the downstream journal suffix the edited answer
#   reaches (journal.counterfactual-edits).
#
#   Corpus (hand-derived; input x = "10"):
#     program: (lambda (k) (prim "store/put" k (prim "store/get" k)))
#     seq 0: store/get  args 2100     result 200      (seeded kv)
#     seq 1: store/put  args 21022000 result 0        (put returns Leaf)
#     run result: 0
#   Replay answers from the journal ONLY: the live kv row must not move.
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[4.1] seed kv: key '10' -> value '200'; run the get-then-put program"
KEY_HASH=4a44dc15364204a80fe80e9039455cc1608281820fe2b24f1e5233ade6af1dd5
psql_q -q -c "INSERT INTO prim_kv (key_hash, key_ternary, value_ternary) \
  VALUES ('$KEY_HASH','10','200') \
  ON CONFLICT (key_hash) DO UPDATE SET value_ternary='200'" >/dev/null
G=$(mint_grant "store/get") || fail "mint store/get grant"
G2=$(curl -sf -X POST -H "$AUTH" --data-binary '{"prim":"store/put"}' \
  $BASE/api/grants | jget "d['id']")
H=$(post_source '"(lambda (k) (prim \"store/put\" k (prim \"store/get\" k)))"')
RUN=$(do_run "$H" '["10"]' "[\"$G\",\"$G2\"]")
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")
echo "$RUN" | jget "len(d['journal'])" | grep -q 2 || fail "expected 2 journaled calls"
echo "$RUN" | grep -q '"status":"normal"' || fail "run must be normal"
echo "$RUN" | jget "d['run']['result_ternary']" | grep -q '^0$' \
  || fail "put must return Leaf"
echo "$RUN" | jget "d['journal'][0]['result_ternary']" | grep -q '^200$' \
  || fail "get must return the seeded value"
echo "$RUN" | jget "d['journal'][1]['args_ternary']" | grep -q '^21022000$' \
  || fail "put args must be [key; get-result]"
KV=$(psql_q -tAc "SELECT value_ternary FROM prim_kv WHERE key_hash='$KEY_HASH'")
[ "$KV" = "200" ] || fail "kv effect must be live: got $KV"

echo "[4.2] replay reproduces the effects as DATA and never touches the store"
V=$(run_verify_status "$RUN_ID")
[ "$V" = "verified" ] || fail "journal-fed replay must verify, got $V"

echo "[4.3] counterfactual precision: edit seq 0 (the get answer) ->"
echo "      exactly the downstream suffix (seq 1) diverges"
FORK=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"0"}]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "counterfactual fork"
echo "$FORK" | grep -q '"verify":"failed"' || fail "edit must diverge the suffix"
DSEQ=$(echo "$FORK" | jget "d['verify']['divergence_seq']")
[ "$DSEQ" = "1" ] || fail "divergence must be at seq 1 (the put), got $DSEQ"
FDP=$(echo "$FORK" | jget "d['verify']['first_diff_path']")
[ "$FDP" = "21" ] || fail "hand-derived first_diff_path is 21, got $FDP"
FPRIM=$(echo "$FORK" | jget "d['verify']['prim']")
[ "$FPRIM" = "store/put" ] || fail "divergence must address the put call, got $FPRIM"
FSTATUS=$(echo "$FORK" | jget "d['run']['status']")
[ "$FSTATUS" = "error" ] || fail "divergent fork must error"
# the edit never touched the live kv (replay is journal-fed, store-free)
KV2=$(psql_q -tAc "SELECT value_ternary FROM prim_kv WHERE key_hash='$KEY_HASH'")
[ "$KV2" = "200" ] || fail "replay must not execute effects live (kv moved)"

echo "[4.4] editing the LAST answer reaches nothing downstream: fork verifies"
COPYF=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":1,"result_ternary":"200"}]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "tail-edit fork"
CV=$(echo "$COPYF" | jget "d['verify']['verify']")
[ "$CV" = "verified" ] || fail "tail edit must verify, got $CV"
CRES=$(echo "$COPYF" | jget "d['run']['result_ternary']")
[ "$CRES" = "200" ] || fail "tail edit must flow into the counterfactual outcome"

echo "ACCEPT 4 OK: journal reproduces the effects; edits change exactly the reachable suffix"
