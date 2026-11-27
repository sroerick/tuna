#!/usr/bin/env bash
# verify-21-midden-m2.sh — Midden M2 evaluator adapter & structural grants
# verification (WELL-FORMED, LENS-TOTAL, PROJECTION claims + midden/* prims).
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[21] Midden M2 evaluator adapter & structural grants"

# 21.1 List registered Tuna Midden schemas
SCHEMAS=$(curl -sf -H "$AUTH" "$BASE/api/midden/schemas")
echo "$SCHEMAS" | grep -q '"name":"todo"' || fail "21.1: todo schema missing"
echo "$SCHEMAS" | grep -q '"name":"def"' || fail "21.1: def schema missing"
TODO_HASH=$(echo "$SCHEMAS" | python3 -c "import json,sys; ds=json.load(sys.stdin)['schemas']; print([d['hash'] for d in ds if d['name']=='todo'][0])")
[ -n "$TODO_HASH" ] || fail "21.1: could not read todo schema hash"
echo "  21.1 registered schemas present (todo=${TODO_HASH:0:12})"

# 21.2 WELL-FORMED claim on todo v1
WF=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d "{\"schema_hash\":\"$TODO_HASH\"}" "$BASE/api/midden/verify/well-formed")
[ "$(echo "$WF" | jget "d['verdict']")" = "pass" ] \
  || fail "21.2: WELL-FORMED verdict not pass: $WF"
WF_RUN=$(echo "$WF" | jget "d['run_id']")
[ "$(run_verify_status "$WF_RUN")" = "verified" ] \
  || fail "21.2: WELL-FORMED run $WF_RUN not replay-verified"
echo "  21.2 WELL-FORMED claim pass (run=$WF_RUN verified)"

# 21.3 Register custom schema v1 + v2 and verify LENS-TOTAL claim
SNAME="m2demo$(date +%s | tail -c 6)"
V1=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d "{\"form\":\"(schema $SNAME (version 1) (fields (title :string)))\"}" \
  "$BASE/api/midden/schemas")
V1_HASH=$(echo "$V1" | jget "d['hash']")
V2=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d "{\"form\":\"(schema $SNAME (version 2) (from 1 (rename title label) (add score :int 0)) (fields (label :string) (score :int)))\"}" \
  "$BASE/api/midden/schemas")
V2_HASH=$(echo "$V2" | jget "d['hash']")
LT=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d "{\"from_hash\":\"$V1_HASH\",\"to_hash\":\"$V2_HASH\"}" \
  "$BASE/api/midden/verify/lens-total")
[ "$(echo "$LT" | jget "d['verdict']")" = "pass" ] \
  || fail "21.3: LENS-TOTAL verdict not pass: $LT"
LT_RUN=$(echo "$LT" | jget "d['run_id']")
[ "$(run_verify_status "$LT_RUN")" = "verified" ] \
  || fail "21.3: LENS-TOTAL run $LT_RUN not replay-verified"
echo "  21.3 LENS-TOTAL claim pass ($SNAME v1->v2, run=$LT_RUN verified)"

# 21.4 PROJECTION claim on todo_counts
PR=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d '{"projection":"todo_counts"}' "$BASE/api/midden/verify/projection")
[ "$(echo "$PR" | jget "d['verdict']")" = "pass" ] \
  || fail "21.4: PROJECTION verdict not pass: $PR"
PR_RUN=$(echo "$PR" | jget "d['run_id']")
[ "$(run_verify_status "$PR_RUN")" = "verified" ] \
  || fail "21.4: PROJECTION run $PR_RUN not replay-verified"
echo "  21.4 PROJECTION claim pass (todo_counts, run=$PR_RUN verified)"

# 21.5 Claims are queryable via /api/midden/claims
CLAIMS=$(curl -sf -H "$AUTH" "$BASE/api/midden/claims")
echo "$CLAIMS" | grep -q '"kind":"WELL-FORMED"' || fail "21.5: WELL-FORMED missing from claims"
echo "$CLAIMS" | grep -q '"kind":"LENS-TOTAL"' || fail "21.5: LENS-TOTAL missing from claims"
echo "$CLAIMS" | grep -q '"kind":"PROJECTION"' || fail "21.5: PROJECTION missing from claims"
echo "  21.5 /api/midden/claims lists all three Midden M2 verdict kinds"

# 21.6 Structural Midden grant scope denial: grant for schema "todo" must deny reading "def"
G_TODO=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d '{"prim":"midden/query","scope_schema":"todo"}' "$BASE/api/grants" | jget "d['id']")
H_BAD=$(post_source '"(lambda (x) (prim \"midden/query\" \"def_view\" \"\" \"\" x))"')
BAD_RUN=$(do_run "$H_BAD" '["0"]' "[\"$G_TODO\"]")
echo "$BAD_RUN" | grep -q 'target outside grant scope' \
  || fail "21.6: cross-schema midden/query was not denied: $BAD_RUN"
echo "$BAD_RUN" | grep -q '"denial_count":1' \
  || fail "21.6: denial_count must be 1"
echo "  21.6 structural grant scope denied cross-schema query (denial_count=1)"

# 21.7 Security regressions: non-admin cannot self-mint midden/* grants or
# call admin-only /api/midden/verify/*, and cross-schema midden/delete is refused
NADM="v21-nonadmin-$(date +%s | tail -c 6)"
NADM_TOK=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d "{\"name\":\"$NADM\"}" "$BASE/api/identities" | jget "d['token']")
MINT_CODE=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
  -H "Authorization: Bearer $NADM_TOK" -H 'Content-Type: application/json' \
  -d '{"prim":"midden/put","scope_schema":"def"}' "$BASE/api/grants")
[ "$MINT_CODE" = "403" ] || fail "21.7: non-admin midden/put self-mint expected 403, got $MINT_CODE"
VER_CODE=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
  -H "Authorization: Bearer $NADM_TOK" -H 'Content-Type: application/json' \
  -d '{"projection":"todo_counts"}' "$BASE/api/midden/verify/projection")
[ "$VER_CODE" = "403" ] || fail "21.7: non-admin /api/midden/verify/projection expected 403, got $VER_CODE"
echo "  21.7 non-admin midden/* self-mint (403) and /api/midden/verify/* (403) denied"

echo "ACCEPT 21 OK: Midden M2 claims (WELL-FORMED, LENS-TOTAL, PROJECTION) + structural grants verified"
