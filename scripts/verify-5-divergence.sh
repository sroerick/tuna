#!/bin/bash
# acceptance.criteria 5: DIVERGENCE HAND-CHECKED.
#   On a fixed divergence corpus, the computed first_diff_path and
#   divergence_seq match hand-derived expectations
#   (replay.divergence-surface).
#
#   Corpus (hand-derived; input x = "10" = Stem Leaf):
#     program: (lambda (x) (prim "echo" (prim "echo" x)))
#     seq 0: echo  args 2100    result 2100    (echo returns its args list)
#     seq 1: echo  args 221000  result 221000  (args = [seq0 result])
#
#     edit seq0 -> "0":   seq1 args replayed = Fork(0,0)=200
#                         vs recorded 221000: left child 2100 vs 0
#                         (Fork vs Leaf)      -> first_diff_path "1"
#     edit seq0 -> "200": seq1 args replayed 22000 vs recorded 221000:
#                         left child 10 vs 0 (Stem vs Leaf) -> "11"
#     edit seq0 -> "2100": identical answer          -> VERIFIED
set -e
. "$(dirname "$0")/verify-lib.sh"

G=$(mint_grant echo)
H=$(post_source '"(lambda (x) (prim \"echo\" (prim \"echo\" x)))"')
RUN=$(do_run "$H" '["10"]' "[\"$G\"]")
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")
echo "$RUN" | jget "len(d['journal'])" | grep -q 2 || fail "expected 2 rows"
echo "$RUN" | jget "d['run']['result_ternary']" | grep -q '^221000$' \
  || fail "echo-of-echo corpus result must be 221000"
SEQ1_CS=$(echo "$RUN" | jget "d['journal'][1]['callsite_path']")

echo "[5.1] edit seq0 -> '0': div_seq 1, callsite = seq1's, first_diff_path = 1"
F1=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"0"}]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "fork"
[ "$(echo "$F1" | jget "d['verify']['verify']")" = "failed" ] || fail "must diverge"
[ "$(echo "$F1" | jget "d['verify']['divergence_seq']")" = "1" ] \
  || fail "divergence_seq must be 1"
[ "$(echo "$F1" | jget "d['verify']['first_diff_path']")" = "1" ] \
  || fail "hand-derived first_diff_path is 1"
[ "$(echo "$F1" | jget "d['verify']['prim']")" = "echo" ] || fail "prim must be echo"
[ "$(echo "$F1" | jget "d['verify']['callsite_path']")" = "$SEQ1_CS" ] \
  || fail "divergence must address the seq1 callsite"

echo "[5.2] deeper edit seq0 -> '200': first_diff_path = 11"
F2=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"200"}]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "fork"
[ "$(echo "$F2" | jget "d['verify']['divergence_seq']")" = "1" ] || fail "divergence_seq 1"
[ "$(echo "$F2" | jget "d['verify']['first_diff_path']")" = "11" ] \
  || fail "hand-derived first_diff_path is 11"

echo "[5.3] identical edit seq0 -> '2100': replay identity holds, VERIFIED"
F3=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"2100"}]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "fork"
[ "$(echo "$F3" | jget "d['verify']['verify']")" = "verified" ] \
  || fail "identical answer must verify"

echo "ACCEPT 5 OK: divergence_seq + first_diff_path match the hand-derived corpus"
