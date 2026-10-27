#!/bin/bash
# verify-14-f3-attribution.sh — the FED3 attribution probe (board card
# todo/g4-fed3-proofs; pre-registered failure 3, borg/federation.borg):
# "a bridge write attributed to any identity other than the true actor
# (or its delegated identity) -> failure."
#
# 14.1  a peer's own tree write attributes to the peer: the ops chain
#         row's actor IS the peer identity, never the operator's.
# 14.2  a peer's run attributes to the peer: run rows carry caller.
# 14.3  fed apply with a SPOOFED source actor: the row verifies as
#         source data (the lie rides the source chain, where it
#         belongs), and the DESTINATION re-attributes at the boundary -
#         ops.actor and tree_paths.owner are the true actor (the
#         applying identity), server-derived, never client-set (the
#         glochid bridge law: the client never sets actor fields).
# 14.4  an out-of-namespace apply is refused AND journaled (a fed-apply
#         deny row attributed to the peer), never absorbed.
#
# The token-revocation half of the bridge law is pinned by verify-1
# [1.7]: a revoked runner token authenticates nothing, so nothing is
# attributed through it.
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[14] FED3 attribution probe (borg/federation.borg pre-registered failure 3)"

# --- the bridge peer: ONE identity named fed-peer-<ns>, minted by the
# operator, holding its own bearer (the issue-token law; verify-1 [1.7]
# exercises its revocable-token tier) -------------------------------
S=$(date +%s)
PEER="f3-$S"
PID_NAME="fed-peer-$PEER"
NS="ns/$PEER/"
MINT=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"name\":\"$PID_NAME\"}" $BASE/api/identities) \
  || fail "mint the bridge peer identity"
PID=$(echo "$MINT" | jget "d['id']")
PAUTH="Authorization: Bearer $(echo "$MINT" | jget "d['token']")"

echo "[14.1] peer's own write attributes to the peer (ops chain actor)"
# the g1 floor applies to the peer too: it writes only behind a covering
# grant scoped to its own namespace - and since scope can only ever be
# NARROWED, the operator seeds that namespace ON BEHALF of the peer
PG=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"prim\":\"tree/put\",\"path_prefix\":\"$NS\",\"identity\":\"$PID\"}" \
  $BASE/api/grants) || fail "operator seeds the peer namespace"
[ "$(echo "$PG" | jget "d['caller']")" = "$PID" ] \
  || fail "seeded peer row must belong to the peer"
# the peer cannot widen: a prefix outside its namespace is refused
[ "$(curl_code -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
    --data-binary '{"prim":"tree/put","path_prefix":"elsewhere/"}' \
    $BASE/api/grants)" = 403 ] || fail "peer must not mint outside its namespace"
[ "$(curl_code -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
    --data-binary "{\"path\":\"${NS}attr-a\",\"value_ternary\":\"10\"}" \
    $BASE/api/tree/put)" = 201 ] || fail "peer's covered in-ns put must 201"
ACTOR=$(psql_q -tAc \
  "SELECT actor FROM tree_ops WHERE path='${NS}attr-a' ORDER BY seq DESC LIMIT 1" \
  | tr -d ' ')
[ "$ACTOR" = "$PID" ] \
  || fail "ops chain actor must be the peer ($ACTOR != $PID)"
echo "  ops row actor == the peer identity"

echo "[14.2] peer's run attributes to the peer (run rows carry caller)"
H=$(curl -sf -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
  -d '{"source":"(lambda (x) x)"}' $BASE/api/programs | jget "d['hash']")
RID=$(curl -sf -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
  -d "{\"program_hash\":\"$H\",\"inputs\":[\"10\"]}" $BASE/api/runs \
  | jget "d['run']['id']")
CALLER=$(psql_q -tAc "SELECT caller::text FROM runs WHERE id='$RID'" | tr -d ' ')
[ "$CALLER" = "$PID" ] || fail "run rows must carry caller = the peer"
echo "  run row caller == the peer identity"

echo "[14.3] fed apply with a SPOOFED source actor re-attributes at the boundary"
# the value must be held before the apply (value-pull-before-ops law);
# the operator holds it - content-addressed, so who holds it is not
# attribution
VH=$(printf '%s' "10" | sha256sum | cut -d' ' -f1)
curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary '{"ternary":"10"}' $BASE/api/value/put > /dev/null \
  || fail "hold the probe value"
# the spoof: the source row CLAIMS the operator's identity as actor -
# the lie must ride the source chain only
ROOT_ID=$(psql_q -tAc "SELECT id::text FROM identities WHERE name='root'" | tr -d ' ')
[ -n "$ROOT_ID" ] || fail "root identity missing"
APPLIED=
for TRY in 1 2 3 4 5; do
  # the chain head: my row continues it, so its prev_hash is the head's
  # op_hash (a concurrent append between pull and apply moves the head;
  # the retry re-pulls - the a4 advisory lock serializes, it does not
  # reserve seq numbers)
  HEAD=$(curl -sf -H "$AUTH" "$BASE/api/fed/ops" | jget "d['head']")
  if [ "$HEAD" -gt 0 ]; then
    LAST=$(curl -sf -H "$AUTH" "$BASE/api/fed/ops?after_seq=$((HEAD-1))&limit=1" \
      | jget "d['ops'][-1]['op_hash']")
  else
    LAST=$(printf '0%.0s' $(seq 1 64))
  fi
  SEQ=$((HEAD+1))
  TS=$(date +%s)
  # op_concat law: seq:path|op|value_hash|prev_version|version|actor|ts
  CONCAT="${SEQ}:${NS}f3-spoof|put|${VH}||1|${ROOT_ID}|${TS}"
  OH=$(printf '%s' "${LAST}${CONCAT}" | sha256sum | cut -d' ' -f1)
  CODE=$(curl_code -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
    --data-binary \
    "{\"src_prefix\":\"$NS\",\"dst_prefix\":\"$NS\",\"ops\":[{\"seq\":$SEQ,\"path\":\"${NS}f3-spoof\",\"op\":\"put\",\"value_hash\":\"$VH\",\"version\":1,\"actor\":\"$ROOT_ID\",\"ts\":$TS,\"op_hash\":\"$OH\",\"prev_hash\":\"$LAST\"}],\"values\":[{\"hash\":\"$VH\",\"kind\":\"tree\",\"payload\":\"10\"}]}" \
    $BASE/api/fed/ops/apply)
  if [ "$CODE" = 200 ]; then APPLIED=1; break; fi
  sleep 0.3
done
[ -n "$APPLIED" ] || fail "spoofed-actor apply never landed (last code $CODE)"
D_ACTOR=$(psql_q -tAc \
  "SELECT actor FROM tree_ops WHERE path='${NS}f3-spoof' ORDER BY seq DESC LIMIT 1" \
  | tr -d ' ')
[ "$D_ACTOR" = "$PID" ] \
  || fail "destination ops actor must be the peer, never the spoofed root ($D_ACTOR)"
D_OWNER=$(psql_q -tAc \
  "SELECT owner FROM tree_paths WHERE path='${NS}f3-spoof'" | tr -d ' ')
[ "$D_OWNER" = "$PID" ] \
  || fail "destination owner must be the peer, never the spoofed root ($D_OWNER)"
echo "  spoofed source actor verified as source data; destination re-attributed to the peer"

echo "[14.4] out-of-namespace apply: refused AND journaled, never absorbed"
CODE=$(curl_code -X POST -H "$PAUTH" -H 'Content-Type: application/json' \
  --data-binary \
  "{\"src_prefix\":\"$NS\",\"dst_prefix\":\"ns/other-$S/\",\"ops\":[{\"bogus\":1}],\"values\":[]}" \
  $BASE/api/fed/ops/apply)
[ "$CODE" = 403 ] || fail "out-of-namespace apply must 403 (got $CODE)"
DENY=$(psql_q -tAc \
  "SELECT actor FROM tree_ops WHERE op='fed-apply' AND path='ns/other-$S/' \
   ORDER BY seq DESC LIMIT 1" | tr -d ' ')
[ "$DENY" = "$PID" ] \
  || fail "the fed-apply refusal must be journaled to the peer ($DENY)"
echo "  403 + journaled fed-apply deny row attributed to the peer"

echo "ACCEPT 14 OK: F3 exercised - bridge writes attribute to the true actor; spoofed source actors re-attribute at the boundary; out-of-namespace writes are refused and journaled, never absorbed"
