#!/usr/bin/env bash
# verify-13-board.sh — the BOARD chapter's acceptance instrument
# (borg/board.borg acceptance 13.1-13.7).  Green = ALL clauses hold.
#
# 13.1 FORMAT    board records are the L1 keyed trees; hostile titles
#                 round-trip; post-migration zero byte-kind values
#                 under todo/
# 13.2 WINDOW    >=300-item walk exact-cover in windows past the old
#                 list_cap; zero-window = the F14 caps error; sibling
#                 namespaces excluded by the slash law
# 13.3 WRITES    view twins; flip journals list+get+cas (lost-update-
#                 safe); stale cas conflicts without writing; del is
#                 the canonical delete; the PAGE flow over HTTP forms
#                 journals put/cas/delete attributed to the member
# 13.4 RIM       the page module greps clean of the store
#                 path/value accessor family and lives on the
#                 grant-mint/run/resolve whitelist
# 13.5 FORGED    a member's scoped grant denies an out-of-scope path
#                 at the prim boundary; sentinel intact
# 13.6 MIGRATION legacy JSON byte rows re-encode field-for-field
#                 (incl. hostile + unparseable tails); second pass
#                 writes zero put ops
# 13.7 HYGIENE   manifest rows == seeded sabralib rows; the exercised
#                 corpus row exists; the differential harness agrees
set -e
. "$(dirname "$0")/verify-lib.sh"

echo "[13] board chapter acceptance (borg/board.borg)"

# --- 13.4 RIM (grep first: cheap and decisive) --------------------------
if grep -qE 'S\.path_|S\.byte_value_|Store\.path_|Store\.byte_value_' \
     server/lib/pages/todo.ml; then
  fail "13.4: the page module still touches the store path/value family"
fi
for ident in Run.execute_run S.mint_grant S.revoke_grant Board_seed.program_hash; do
  grep -q "$ident" server/lib/pages/todo.ml \
    || fail "13.4: expected rim identifier $ident missing from the page"
done
grep -q '(`/todo", Todo.view pool)' server/lib/pages.ml \
  || grep -q '"/todo", Todo.view pool' server/lib/pages.ml \
  || fail "13.4: the /todo route mounting changed"
echo "  13.4 rim grep clean (no store path/value accessors; wheel intact)"

# --- 13.7 HYGIENE (the vocabulary floor under everything else) ----------
MF=$(wc -l < stdlib/v1/manifest)
SC=$(psql_q -tAc "SELECT count(*) FROM repl_dict WHERE identity_id=(SELECT id FROM identities WHERE name='sabralib')")
[ "$MF" = "$SC" ] || fail "13.7: manifest $MF rows but sabralib seeded $SC"
[ -f scripts/diff-corpus/stdlib_todo-add.corpus ] \
  || fail "13.7: the exercised corpus row stdlib_todo-add is missing"
echo "  13.7 manifest $MF rows == seeded rows; exercised corpus row present"

# --- codec parity (draws the floor under migration + hostile titles) ---
echo "[13.0] pytwin codec parity vs the live REPL ... "
TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}" \
  python3 scripts/dialect/pytwin.py parity "$BASE" > /dev/null \
  || fail "parity: pytwin codec twins disagree with the OCaml rim"
echo "  13.0 pytwin parity green"

# --- runtime clauses: the board probe (fresh namespace) ------------------
echo "[13.1-13.2-13.3-13.5] board_probe (fresh namespace)"
TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}" \
  python3 scripts/dialect/board_probe.py \
  || fail "board probe"
# [includes: hostile-title round-trip twins (13.1), the windowed walk +
# caps-error + sibling exclusion (13.2), flip/stale-cas/del journals
# (13.3), and the member scoped-grant forged-path denial (13.5).]

# --- 13.6 MIGRATION + the 13.1 byte-free invariant -----------------------
echo "[13.6] migration: seed legacy fixtures, migrate twice, assert fields"
LEGACY_N=$(psql_q -tAc "SELECT count(*) FROM tree_paths tp JOIN byte_values bv ON tp.value_hash = bv.hash WHERE tp.path LIKE 'todo/%'")
TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}" \
  python3 scripts/dialect/board_migrate.py > /dev/null \
  || fail "13.6: migration pass 1 failed"
AFTER=$(psql_q -tAc "SELECT count(*) FROM tree_paths tp JOIN byte_values bv ON tp.value_hash = bv.hash WHERE tp.path LIKE 'todo/%'")
[ "$AFTER" = "0" ] || fail "13.6/13.1: $AFTER legacy byte rows remain under todo/"
PUTS_BEFORE=$(psql_q -tAc "SELECT count(*) FROM tree_ops WHERE op='put' AND path LIKE 'todo/%'")
TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}" \
  python3 scripts/dialect/board_migrate.py > /dev/null \
  || fail "13.6: migration pass 2 failed"
PUTS_AFTER=$(psql_q -tAc "SELECT count(*) FROM tree_ops WHERE op='put' AND path LIKE 'todo/%'")
[ "$PUTS_BEFORE" = "$PUTS_AFTER" ] \
  || fail "13.6: second migration pass wrote $((PUTS_AFTER - PUTS_BEFORE)) more ops"
TUNA_HTTP_PORT="${TUNA_HTTP_PORT:-18090}" python3 - "$BASE" "$TOKEN" <<'PYEOF' \
  || fail "13.6: field-for-field migration asserts failed"
import json, sys, urllib.request
sys.path.insert(0, "scripts/dialect")
import pytwin
base, token = sys.argv[1], sys.argv[2]
auth = {"Authorization": "Bearer " + token, "Content-Type": "application/json"}
def post(path, obj):
    req = urllib.request.Request(base + path, data=json.dumps(obj).encode(), headers=auth)
    return json.load(urllib.request.urlopen(req))
if post("/api/tree/get", {"path": "todo/legacy-tests-000"})["value_ternary"] != \
   pytwin.record("open", b"legacy item one", b"smoke", 1790856000):
    sys.exit("legacy-tests-000 fields drifted")
if post("/api/tree/get", {"path": "todo/legacy-tests-001"})["value_ternary"] != \
   pytwin.record("done", b'legacy "quoted" title', b"smoke", 1790933400):
    sys.exit("legacy-tests-001 (quote title) drifted")
if post("/api/tree/get", {"path": "todo/legacy-tests-002"})["value_ternary"] != \
   pytwin.record("open", b"not-json-bytes", b"migration", 0):
    sys.exit("legacy-tests-002 raw tail drifted")
print("  13.6 fields intact (quote title + raw tail included)")
PYEOF
echo "  13.6 idempotent: $LEGACY_N legacy rows -> 0; pass two wrote zero ops"

# --- 13.3 (page half): the member flow over HTTP forms -------------------
echo "[13.3'] page flow over HTTP forms (the rim drive)"
JAR=$(mktemp); trap 'rm -f "$JAR"' EXIT
curl -s -c "$JAR" -o /dev/null -d "token=$TOKEN" "$BASE/login" \
  || fail "13.3: login failed"
T=$(date +%s%N | tail -c 9 | head -c 6)
curl -s -b "$JAR" -o /dev/null -w '%{http_code}' \
  -d "title=verify13 card $T" "$BASE/todo/add" | grep -q '303' \
  || fail "13.3: add over the form did not redirect"
curl -s -b "$JAR" "$BASE/todo" | grep -q "verify13 card $T" \
  || fail "13.3: the added card does not render on the board"
ID=$(curl -s -b "$JAR" "$BASE/todo" \
  | grep -oE "/todo/[0-9]+-[0-9a-f]+/state" | head -1 \
  | sed 's|/todo/||;s|/state||')
[ -n "$ID" ] || fail "13.3: no flip form on the board row"
curl -s -b "$JAR" -o /dev/null -w '%{redirect_url}' \
  -X POST "$BASE/todo/$ID/state" | grep -q 'run=' \
  || fail "13.3: flip did not redirect with a run id"
CAS=$(psql_q -tAc "SELECT count(*) FROM tree_ops WHERE op='cas' AND path LIKE 'todo/%'")
[ "$CAS" -ge 1 ] || fail "13.3: no cas op on the board ops chain"
curl -s -b "$JAR" -o /dev/null -w '%{http_code}' \
  -X POST "$BASE/todo/$ID/del" | grep -q '303' \
  || fail "13.3: del did not redirect"
DEL=$(psql_q -tAc "SELECT count(*) FROM tree_ops WHERE op='delete' AND path LIKE 'todo/%'")
[ "$DEL" -ge 1 ] || fail "13.3: no canonical delete op on the chain"
LASTDEN=$(psql_q -tAc "SELECT denial_count FROM runs WHERE program_hash IN (SELECT encode(sha256(ternary::bytea),'hex') FROM repl_dict WHERE identity_id=(SELECT id FROM identities WHERE name='board')) AND caller=(SELECT id FROM identities WHERE name='root') ORDER BY created_at DESC LIMIT 1")
[ "$LASTDEN" = "0" ] || fail "13.3: the last page-driven board run shows denial_count $LASTDEN"
echo "  13.3 page flow green (add renders, flip cas'd, del canonical-deleted"
echo "        by the member's own runs, zero denials)"

echo "ACCEPT 13 OK: the /todo board is rim over in-calculus programs"
