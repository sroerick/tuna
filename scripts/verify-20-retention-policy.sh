#!/bin/bash
# acceptance.criteria 20: RETENTION POLICY (journal/retention-gc policy knob).
#   No policy row = no collection and no server timer.  A non-admin cannot
#   set a policy or sweep.  An enabled policy dry-runs by default (counts and
#   sample ids, nothing deleted); only dry_run=false collects that identity's
#   old runs through the existing tombstone path, citing the policy.  With
#   keep_failures=true a failed run and an unverified old run are kept;
#   another identity's old run is untouched; a second sweep collects nothing.
set -e
. "$(dirname "$0")/verify-lib.sh"

N="$RANDOM$RANDOM$(date +%s)"
journal_rows() { psql_q -tAc "SELECT count(*) FROM journals WHERE run_id='$1'"; }
run_verify() { psql_q -tAc "SELECT coalesce(verify_status,'-') FROM runs WHERE id='$1'"; }
gced_policy() { psql_q -tAc "SELECT coalesce(journal_gced_policy,'-') FROM runs WHERE id='$1'"; }
sweep() { curl -sf -m 300 -X POST -H "$AUTH" --data-binary "$1" \
            $BASE/api/admin/retention/sweep; }
set_policy() { curl -sf -m 60 -X PUT -H "$AUTH" --data-binary "$2" \
                 "$BASE/api/admin/retention/$1"; }
mint_id() { curl -sf -m 60 -X POST -H "$AUTH" --data-binary "{\"name\":\"$1\"}" \
              $BASE/api/identities; }
row() { printf '%s' "$1" | python3 -c '
import json, sys
d = json.load(sys.stdin)
rows = [x for x in d.get("swept") or [] if x.get("identity") == sys.argv[1]]
print(json.dumps(rows[0] if rows else {}))' "$2"; }
row_has() { printf '%s' "$1" | python3 -c '
import json, sys
d = json.loads(sys.stdin.read())
assert d, "no swept row"
assert d["collected"] == int(sys.argv[1]), d
assert d["eligible"] == int(sys.argv[2]), d' "$2" "$3"; }

# ---- two fresh identities (A: the policy, B: the control) -----------------
AJSON=$(mint_id "retention-a-$N"); TA=$(printf '%s' "$AJSON" | jget "d['token']")
IA=$(printf '%s' "$AJSON" | jget "d['id']"); NA=$(printf '%s' "$AJSON" | jget "d['name']")
BJSON=$(mint_id "retention-b-$N"); TB=$(printf '%s' "$BJSON" | jget "d['token']")
NB=$(printf '%s' "$BJSON" | jget "d['name']")
AA="Authorization: Bearer $TA"; AB="Authorization: Bearer $TB"

H=$(post_source '"(lambda (x) (prim \"echo\" x))"')

run_as() { # auth backdate verify(1|0) -> run id; each caller mints its own grant
  local auth="$1" back="$2" verify="$3" g r id
  g=$(curl -sf -m 60 -X POST -H "$auth" --data-binary '{"prim":"echo"}' \
        $BASE/api/grants | jget "d['id']")
  r=$(curl -sf -m 300 -X POST -H "$auth" --data-binary \
        "{\"program_hash\":\"$H\",\"inputs\":[\"10\"],\"grants\":[\"$g\"],\"fuel\":10000,\"size_cap\":100000}" \
        $BASE/api/runs)
  id=$(printf '%s' "$r" | jget "d['run']['id']")
  if [ "$verify" = 1 ]; then curl -sf -m 300 -H "$auth" $BASE/api/runs/$id >/dev/null; fi
  if [ -n "$back" ]; then
    psql_q -q -c "UPDATE runs SET created_at = now() - interval '$back' WHERE id='$id'"
  fi
  printf '%s' "$id"
}

A1=$(run_as "$AA" "3 days" 1)          # verified, old  -> collectable
A2=$(run_as "$AA" "3 days" 1)          # failed, old    -> kept (keep_failures)
psql_q -q -c "UPDATE runs SET status='error' WHERE id='$A2'"
A3=$(run_as "$AA" "" 1)                # verified, fresh -> kept by age
A4=$(run_as "$AA" "3 days" 0)          # unverified, old -> kept (keep_failures)
B1=$(run_as "$AB" "3 days" 1)          # another identity, old -> untouched

# ---- 1. non-admins get 403, with no existence leak ------------------------
CODE=$(curl_code -X PUT -H "$AA" --data-binary '{"keep_days":1,"keep_failures":true,"enabled":true}' "$BASE/api/admin/retention/$IA")
[ "$CODE" = 403 ] || fail "non-admin PUT must be 403, got $CODE"
CODE=$(curl_code -X POST -H "$AA" --data-binary '{"dry_run":false}' $BASE/api/admin/retention/sweep)
[ "$CODE" = 403 ] || fail "non-admin sweep must be 403, got $CODE"
CODE=$(curl_code -H "$AA" $BASE/api/admin/retention)
[ "$CODE" = 403 ] || fail "non-admin list must be 403, got $CODE"

# ---- 2. no policy: a real sweep collects nothing for A --------------------
S=$(sweep '{"dry_run":false}')
[ "$(row "$S" "$NA")" = "{}" ] || fail "A must have no swept row without a policy"
[ "$(journal_rows "$A1")" = 1 ] || fail "no policy must not delete journals"

# ---- 3. admin sets A's policy; A reads its own row ------------------------
P=$(set_policy "$IA" '{"keep_days":1,"keep_failures":true,"enabled":true}')
printf '%s' "$P" | python3 -c '
import json, sys
d = json.load(sys.stdin)["policy"]
assert d["enabled"] is True and d["keep_days"] == 1 and d["keep_failures"] is True, d' 
curl -sf -H "$AA" $BASE/api/retention | python3 -c '
import json, sys
d = json.load(sys.stdin)["policy"]
assert d["keep_days"] == 1 and d["enabled"] is True, d'

# ---- 4. dry run is the default: {} deletes nothing ------------------------
D=$(sweep '{}')
printf '%s' "$D" | python3 -c '
import json, sys
d = json.load(sys.stdin)
assert d["dry_run"] is True, d
r = [x for x in d["swept"] if x["identity"] == sys.argv[1]][0]
assert r["eligible"] == 1 and r["collected"] == 0, r
assert r["run_ids"] == [sys.argv[2]], r
assert r["policy"] == "retention:%s:keep-1d:keep-failures" % sys.argv[1], r' "$NA" "$A1"
[ "$(journal_rows "$A1")" = 1 ] || fail "dry run must not delete journals"

# ---- 5. dry_run=false collects A1 only; tombstone cites the policy --------
S=$(sweep '{"dry_run":false}')
printf '%s' "$(row "$S" "$NA")" | row_has 1 1
[ "$(journal_rows "$A1")" = 0 ] || fail "A1 journal must be gone"
[ "$(gced_policy "$A1")" = "retention:$NA:keep-1d:keep-failures" ] \
  || fail "tombstone must cite the policy, got $(gced_policy "$A1")"
[ "$(run_verify "$A1")" = gced ] || fail "A1 must be gced, got $(run_verify "$A1")"
for kept in "$A2" "$A3" "$A4" "$B1"; do
  [ "$(journal_rows "$kept")" = 1 ] || fail "$kept must be kept, journal gone"
  [ "$(run_verify "$kept")" != gced ] || fail "$kept must not be gced"
done

# ---- 6. a second sweep collects nothing -----------------------------------
S=$(sweep '{"dry_run":false}')
printf '%s' "$(row "$S" "$NA")" | row_has 0 0

# ---- 7. enabled=false collects nothing ------------------------------------
set_policy "$IA" '{"keep_days":1,"keep_failures":true,"enabled":false}' >/dev/null
psql_q -q -c "UPDATE runs SET created_at = now() - interval '3 days' WHERE id='$A3'"
S=$(sweep '{"dry_run":false}')
[ "$(row "$S" "$NA")" = "{}" ] || fail "a disabled policy must not sweep"
[ "$(journal_rows "$A3")" = 1 ] || fail "a disabled policy must not delete"

# ---- 8. keep_failures=false makes the failed and unverified runs eligible -
set_policy "$IA" '{"keep_days":1,"keep_failures":false,"enabled":true}' >/dev/null
S=$(sweep '{"dry_run":false}')
python3 -c '
import json, sys
d = json.load(sys.stdin)
r = [x for x in d["swept"] if x["identity"] == sys.argv[1]][0]
assert r["collected"] == 3, r          # A2 failed, A3 verified, A4 unverified
assert r["policy"] == "retention:%s:keep-1d" % sys.argv[1], r' "$S" "$NA"
[ "$(journal_rows "$A2")" = 0 ] || fail "A2 must be collected with keep_failures=false"
[ "$(gced_policy "$A2")" = "retention:$NA:keep-1d" ] || fail "A2 citation"
[ "$(journal_rows "$A4")" = 0 ] || fail "A4 must not be kept with keep_failures=false"
[ "$(journal_rows "$B1")" = 1 ] || fail "another identity's run must be untouched"

set_policy "$IA" '{"keep_days":1,"keep_failures":true,"enabled":false}' >/dev/null
echo "[20] retention policy: off by default, dry-run first, cited tombstones, failures kept"
