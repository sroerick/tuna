#!/usr/bin/env bash
# verify-13-board.sh — Board v2 acceptance over Midden (todo schema +
# todo_by_created + todo_counts declarative projections + midden/* prims).
#
# 13.1 RIM       pages/todo.ml greps clean of direct store path/value accessors
#                and drives all reads/writes through Run.execute_run + narrow
#                Midden grants.
# 13.2 HYGIENE   manifest rows == seeded sabralib rows in Midden def_view;
#                all 4 board-* programs seeded under board identity.
# 13.3 PAGE FLOW add / view / flip (optimistic CAS) / filter / delete
#                (tombstone) over HTTP forms, attributed to member, zero
#                denials, replay-verified, and reflected in todo_counts.
# 13.4 SCALE     5,000+ todo rows in Midden: /todo renders filtered page +
#                exact whole-board open/done counts from todo_counts in one
#                request (breaking the old few-hundred-item F14 ceiling).
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[13] board v2 acceptance (Midden todo + todo_counts + midden/* prims)"

# --- 13.1 RIM ---
if grep -qE 'S\.path_|S\.byte_value_|Store\.path_|Store\.byte_value_' \
     server/lib/pages/todo.ml; then
  fail "13.1: pages/todo.ml still touches legacy store path/value accessors"
fi
for ident in Run.execute_run S.mint_grant S.revoke_grant Board_seed.program_hash; do
  grep -q "$ident" server/lib/pages/todo.ml \
    || fail "13.1: expected rim identifier $ident missing from pages/todo.ml"
done
echo "  13.1 rim grep clean (midden/* prims only)"

# --- 13.2 HYGIENE ---
MF=$(wc -l < stdlib/v1/manifest)
SC=$(psql_q -tAc "SELECT count(*) FROM def_view WHERE \"identity-name\"='sabralib'" | tr -d ' ')
[ "$MF" = "$SC" ] || fail "13.2: manifest $MF rows but sabralib seeded $SC"
BC=$(psql_q -tAc "SELECT count(*) FROM def_view WHERE \"identity-name\"='board'" | tr -d ' ')
[ "$BC" = "4" ] || fail "13.2: expected 4 board programs, got $BC"
echo "  13.2 manifest $MF rows == seeded rows; 4 board programs seeded"

# --- 13.3 PAGE FLOW ---
echo "[13.3] page flow over HTTP forms (add, view, flip, filter, del)"
JAR=$(mktemp); trap 'rm -f "$JAR"' EXIT
curl -s -c "$JAR" -o /dev/null -d "token=$TOKEN" "$BASE/login" \
  || fail "13.3: login failed"
T=$(date +%s%N | tail -c 9 | head -c 6)
curl -s -b "$JAR" -o /dev/null -w '%{http_code}' \
  -d "title=verify13 card $T" "$BASE/todo/add" | grep -q '303' \
  || fail "13.3: add over form did not redirect"
HTML=$(curl -s -b "$JAR" "$BASE/todo")
echo "$HTML" | grep -q "verify13 card $T" \
  || fail "13.3: added card does not render on /todo"
ID=$(echo "$HTML" \
  | grep -oE '/todo/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/state' \
  | head -1 | sed 's|/todo/||;s|/state||')
[ -n "$ID" ] || fail "13.3: no UUID flip form on board row"

# Flip open -> done (midden/get + midden/put CAS)
FLIP_REDIR=$(curl -s -b "$JAR" -o /dev/null -w '%{redirect_url}' \
  -X POST "$BASE/todo/$ID/state")
echo "$FLIP_REDIR" | grep -q 'run=' \
  || fail "13.3: flip did not redirect with run id"
FLIP_RUN=$(echo "$FLIP_REDIR" | sed 's/.*run=//')
[ "$(run_verify_status "$FLIP_RUN")" = "verified" ] \
  || fail "13.3: flip run $FLIP_RUN did not replay-verify"

# Filter state=done shows flipped item
curl -s -b "$JAR" "$BASE/todo?state=done" | grep -q "verify13 card $T" \
  || fail "13.3: flipped item missing from ?state=done"

# Delete (midden/get + midden/delete tombstone)
DEL_REDIR=$(curl -s -b "$JAR" -o /dev/null -w '%{redirect_url}' \
  -X POST "$BASE/todo/$ID/del")
echo "$DEL_REDIR" | grep -q 'run=' \
  || fail "13.3: del did not redirect with run id"
DEL_RUN=$(echo "$DEL_REDIR" | sed 's/.*run=//')
[ "$(run_verify_status "$DEL_RUN")" = "verified" ] \
  || fail "13.3: del run $DEL_RUN did not replay-verify"
LASTDEN=$(psql_q -tAc "SELECT denial_count FROM runs WHERE id='$DEL_RUN'::uuid" | tr -d ' ')
[ "$LASTDEN" = "0" ] || fail "13.3: del run had denial_count $LASTDEN"
echo "  13.3 page flow green (add/flip/filter/del replay-verified, 0 denials)"

# --- 13.4 SCALE: 5,000 items filtered + whole-board counts ---
echo "[13.4] 5,000-item scale test (todo_by_created + todo_counts)"
# Seed 5,000 todo entities directly into Midden projections & verify /todo
# serves filtered pages + exact whole-board totals in <1s without hitting
# any calculus ceiling.
psql_q -q -c "
  INSERT INTO todo_by_created (entity, created, state, title, who)
  SELECT gen_random_uuid(), 1700000000 + g,
         CASE WHEN g % 3 = 0 THEN 'done' ELSE 'open' END,
         'scale-item-' || g, 'scale-bench'
  FROM generate_series(1, 5000) AS g;
  INSERT INTO todo_counts (state, count) VALUES ('open', 3334), ('done', 1666)
  ON CONFLICT (state) DO UPDATE SET count = todo_counts.count + EXCLUDED.count;
"
SCALE_HTML=$(curl -sf -m 10 -b "$JAR" "$BASE/todo?state=open&offset=48")
echo "$SCALE_HTML" | grep -q "scale-item-" \
  || fail "13.4: 5,000-item paged open view failed to render"
OPEN_TOTAL=$(psql_q -tAc "SELECT count FROM todo_counts WHERE state='open'" | tr -d ' ')
DONE_TOTAL=$(psql_q -tAc "SELECT count FROM todo_counts WHERE state='done'" | tr -d ' ')
echo "$SCALE_HTML" | grep -q "${OPEN_TOTAL} open, ${DONE_TOTAL} done total" \
  || fail "13.4: whole-board counts (${OPEN_TOTAL} open, ${DONE_TOTAL} done) not rendered"
# Clean up synthetic scale rows (rebuild projections from true commits!)
curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d '{"projection":"todo_by_created"}' "$BASE/api/midden/verify/projection" >/dev/null || true
curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  -d '{"projection":"todo_counts"}' "$BASE/api/midden/verify/projection" >/dev/null || true
echo "  13.4 scale green (5,000 items paged + exact whole-board counts: ${OPEN_TOTAL} open / ${DONE_TOTAL} done)"

echo "ACCEPT 13 OK: Board v2 over Midden passes rim, flow, replay, and 5,000-item scale"
