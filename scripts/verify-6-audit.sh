#!/bin/bash
# acceptance.criteria 6: AUDIT QUERY.
#   "Which runs touched grant g" over the corpus answers exactly from
#   journal+run rows, no log grep (grants.audit): the effect record IS
#   the audit record — a plain relational query, not a text heuristic.
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[6.1] mint a fresh echo grant; run TWO programs with it"
G=$(mint_grant echo)
H1=$(post_source '"(lambda (x) (prim \"echo\" x))"')
H2=$(post_source '"(lambda (x) (%22102000 (prim \"echo\" x)))"')
R1=$(do_run "$H1" '["10"]' "[\"$G\"]"); R1_ID=$(echo "$R1" | jget "d['run']['id']")
R2=$(do_run "$H2" '["10"]' "[\"$G\"]"); R2_ID=$(echo "$R2" | jget "d['run']['id']")

echo "[6.2] a run WITHOUT grants touches the grant only as a NULL denial row"
R3=$(do_run "$H1" '["10"]' "[]")
R3_ID=$(echo "$R3" | jget "d['run']['id']")
echo "$R3" | grep -q '"error":"grant denial' || fail "denial must be journaled"
NULLS=$(psql_q -tAc "SELECT count(*) FROM journals \
  WHERE run_id='$R3_ID' AND grant_id IS NULL")
[ "$NULLS" -ge 1 ] || fail "denial row must carry grant_id NULL"

echo "[6.3] the audit query: runs that touched grant g — exactly R1 and R2"
AUDIT=$(psql_q -tAc \
  "SELECT j.run_id::text, r.status, r.step_count \
   FROM journals j JOIN runs r ON r.id = j.run_id \
   WHERE j.grant_id = '$G'::uuid GROUP BY j.run_id, r.status, r.step_count \
   ORDER BY j.run_id")
echo "$AUDIT" | grep -q "$R1_ID" || fail "audit must include R1"
echo "$AUDIT" | grep -q "$R2_ID" || fail "audit must include R2"
HITS=$(echo "$AUDIT" | grep -c "^[0-9a-f-]" || true)
[ "$HITS" = "2" ] || fail "audit must be exact (2 runs), got $HITS: $AUDIT"
echo "$AUDIT" | grep -q "$R3_ID" && fail "ungranted run must NOT appear in the audit" || true

echo "[6.4] grant-level detail: which prims ran under g, at which callsites"
DETAIL=$(psql_q -tAc \
  "SELECT DISTINCT j.prim, j.callsite_path, j.prim_contract \
   FROM journals j WHERE j.grant_id = '$G'::uuid")
echo "$DETAIL" | grep -q "echo" || fail "prim detail missing"

echo "ACCEPT 6 OK: 'which runs touched grant g' answers exactly from journal+run rows"
