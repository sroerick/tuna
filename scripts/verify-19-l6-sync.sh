#!/bin/bash
# acceptance 19 (borg/local.borg l6-local-peer): the local directory syncs
# as a federation peer over the existing /api/fed routes.
#
#   19.1 OFFLINE, THEN PUSH - a def and a sealed run made with no server,
#        then one push: the def lands under ns/<laptop>/dict/<identity>/ at
#        the SAME value hash, and the pushed window's applied rows report
#        no drop.
#   19.2 RECORD FETCHED BACK - the run's deriv record is pulled by hash,
#        rehashed on receipt, and verifies with tools/deriv-check.
#   19.3 DIVERGENCE, NOT CONFLICT - a different server def of the same name
#        is reported by name with both hashes; neither side is overwritten.
#
# Self-contained: makes its own scratch database from migrations and boots
# its own server (127.0.0.1, an ephemeral port) with TUNA_FED_PEERS=lap.
#
# Env: TUNA_DB_HOST/PORT/USER (as scripts/test-store.sh), TMPDIR.
set -uo pipefail
cd "$(dirname "$0")/.."

fail() { echo "FAIL verify-19: $*" >&2; exit 1; }

T=$(mktemp -d "${TMPDIR:-/tmp}/tuna-l6.XXXXXX") || fail "mktemp"
DB=tuna_l6_e2e_$$
PORT=$(python3 - <<'PY'
import socket
s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()
PY
)
BOOT=$(od -An -N16 -tx1 /dev/urandom | tr -d ' \n')
PEER_TOKEN=$(od -An -N16 -tx1 /dev/urandom | tr -d ' \n')
DB_HOST="${TUNA_DB_HOST:-/tmp}"
DB_PORT="${TUNA_DB_PORT:-5434}"
DB_USER="${TUNA_DB_USER:-tuna}"
PSQL=psql
command -v psql >/dev/null 2>&1 || PSQL="$HOME/pg/bin/psql"
BASE="http://127.0.0.1:$PORT"
case "$DB_HOST" in /*) db_h=(-h "$DB_HOST") ;; *) db_h=(-h "$DB_HOST") ;; esac
psql_admin() { "$PSQL" "${db_h[@]}" -p "$DB_PORT" -U "$DB_USER" -d postgres "$@"; }
psql_db() { "$PSQL" "${db_h[@]}" -p "$DB_PORT" -U "$DB_USER" -d "$DB" "$@"; }

cleanup() {
  [ -n "${SRV_PID:-}" ] && kill "$SRV_PID" 2>/dev/null
  psql_admin -q -c "DROP DATABASE IF EXISTS $DB" >/dev/null 2>&1
  rm -rf "$T"
}
trap cleanup EXIT

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$HOME/pg/lib"
"$PSQL" "${db_h[@]}" -p "$DB_PORT" -U "$DB_USER" -d postgres -tAc "SELECT 1" \
  >/dev/null 2>&1 || fail "postgres is down ($DB_HOST:$DB_PORT)"
psql_admin -q -c "DROP DATABASE IF EXISTS $DB" >/dev/null || fail "drop $DB"
psql_admin -q -c "CREATE DATABASE $DB" >/dev/null || fail "create $DB"
for sql in migrations/*.sql; do
  psql_db -q -v ON_ERROR_STOP=1 -f "$sql" >/dev/null || fail "migration $sql"
done

dune build --root "$PWD" cli/bin/main.exe server/bin/main.exe \
  tools/deriv-check/deriv_check.exe 2>&1 | head -20
CLI=_build/default/cli/bin/main.exe
SRV=_build/default/server/bin/main.exe
DC=_build/default/tools/deriv-check/deriv_check.exe
[ -x "$CLI" ] && [ -x "$SRV" ] && [ -x "$DC" ] || fail "binaries did not build"

jget() { python3 -c "import json,sys;d=json.load(sys.stdin);print(eval(sys.argv[1]))" "$1"; }
auth_boot="Authorization: Bearer $BOOT"

TUNA_DB_HOST="$DB_HOST" TUNA_DB_PORT="$DB_PORT" TUNA_DB_USER="$DB_USER" \
TUNA_DB_NAME="$DB" TUNA_HTTP_PORT="$PORT" TUNA_BOOTSTRAP_TOKEN="$BOOT" \
TUNA_FED_PEERS=lap TUNA_FED_PEER_TOKEN_LAP="$PEER_TOKEN" \
  "$SRV" > "$T/server.log" 2>&1 &
SRV_PID=$!
for _ in $(seq 1 120); do
  code=$(curl -s -m 2 -o /dev/null -w '%{http_code}' -H "$auth_boot" "$BASE/api/whoami")
  [ "$code" = "200" ] && break
  sleep 0.25
done
[ "${code:-000}" = "200" ] || { cat "$T/server.log" >&2; fail "server did not boot"; }

# an ordinary non-admin owner: the laptop pulls this identity's dictionary
curl -sf -m 60 -X POST -H "$auth_boot" --data-binary '{"name":"e2eowner"}' \
  "$BASE/api/identities" > "$T/owner.json" || fail "mint owner"
OWNER=$(jget "d['token']" < "$T/owner.json")
[ -n "$OWNER" ] || fail "owner token"
echo "  server on $BASE, owner e2eowner, peer lap"

# 19.1 offline: one def and one sealed run, no server in sight
LAP=$T/lap; mkdir -p "$LAP"
printf 'e2eowner\n' > "$LAP/identity"
printf 'def greet (lambda (x) x)\n' | "$CLI" repl --dir "$LAP" \
  --fuel 100000 --cap 100000 > "$T/def.out" 2>&1 || { cat "$T/def.out"; fail "offline def"; }
echo '(lambda (x) [(runtime (prim "echo" x)) (runtime (prim "now"))])' > "$T/r.sabra"
"$CLI" run "$T/r.sabra" '"k"' --dir "$LAP" --grant echo --grant now \
  --fuel 100000 --cap 100000 > "$T/run.out" 2>&1 || { cat "$T/run.out"; fail "offline run"; }
REC=$(sed -n 's/^record //p' "$T/run.out")
[ -f "$REC" ] || fail "no sealed record ($T/run.out)"
REC_HASH=$(sha256sum "$REC" | cut -d' ' -f1)
LOCAL_DEF=$(python3 - "$LAP/ops.jsonl" <<'PY'
import json, sys
rows = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
hits = [r for r in rows if r.get("path") == "dict/e2eowner/greet"]
print(hits[-1]["value_hash"] if hits else "")
PY
)
[ -n "$LOCAL_DEF" ] || fail "the offline def is not in the local op log"

printf 'peer=%s\nowner=%s\n' "$PEER_TOKEN" "$OWNER" > "$T/tokens"
chmod 600 "$T/tokens"
"$CLI" fed push --base "$BASE" --peer lap --tokens "$T/tokens" --dir "$LAP" \
  --yes > "$T/push.out" 2>&1 || { cat "$T/push.out"; fail "fed push"; }
grep -q '^push applied ' "$T/push.out" || { cat "$T/push.out"; fail "push report"; }
grep -q '^push dropped ' "$T/push.out" && { cat "$T/push.out"; fail "unexpected drop"; }
PUSHED=$(curl -sf -m 60 -G -H "Authorization: Bearer $OWNER" \
  --data-urlencode 'prefix=ns/lap/dict/e2eowner/' --data-urlencode 'limit=5000' \
  "$BASE/api/fed/ops") || fail "read the pushed window"
GOT=$(python3 - "$LOCAL_DEF" <<PY
import json, sys
d = json.loads('''$PUSHED''')
hits = [o for o in d["ops"] if o.get("path") == "ns/lap/dict/e2eowner/greet"]
print(hits[-1]["value_hash"] if hits else "")
PY
)
[ "$GOT" = "$LOCAL_DEF" ] || fail "pushed def hash $GOT != local $LOCAL_DEF"
echo "  [19.1] offline def pushed to ns/lap/dict/e2eowner/greet at $LOCAL_DEF"

# 19.2 the record comes back by hash and deriv-check accepts it
curl -sf -m 60 -H "Authorization: Bearer $OWNER" \
  "$BASE/api/fed/value/$REC_HASH" > "$T/value.json" || fail "fetch record value"
python3 - "$T/value.json" "$T/record.json" <<'PY' || fail "record payload"
import base64, json, sys
v = json.load(open(sys.argv[1]))
assert v.get("kind") == "bytes", v.get("kind")
open(sys.argv[2], "wb").write(base64.b64decode(v["payload"]))
PY
[ "$(sha256sum "$T/record.json" | cut -d' ' -f1)" = "$REC_HASH" ] || fail "fetched record does not rehash"
"$DC" "$T/record.json" > "$T/deriv.out" 2>&1 || { cat "$T/deriv.out"; fail "deriv-check refused the fetched record"; }
echo "  [19.2] record $REC_HASH fetched back and verified by deriv-check"

# 19.3 a concurrent server def of the same name: reported, not overwritten
curl -sf -m 120 -X POST -H "Authorization: Bearer $OWNER" \
  --data-binary '{"command":"def greet (lambda (a) (lambda (b) a))"}' \
  "$BASE/api/repl" > "$T/server_def.json" || { cat "$T/server_def.json"; fail "server def"; }
"$CLI" fed check --base "$BASE" --peer lap --tokens "$T/tokens" --dir "$LAP" \
  > "$T/check.out" 2>&1 || { cat "$T/check.out"; fail "fed check"; }
grep -q '^divergence greet local ' "$T/check.out" || { cat "$T/check.out"; fail "no divergence for greet"; }
LOCAL_H=$(sed -n 's/^divergence greet local \([^ ]*\) server .*/\1/p' "$T/check.out")
SERV_H=$(sed -n 's/^divergence greet local [^ ]* server \(.*\)/\1/p' "$T/check.out")
[ "$LOCAL_H" = "$LOCAL_DEF" ] || fail "report local hash $LOCAL_H != $LOCAL_DEF"
[ -n "$SERV_H" ] && [ "$SERV_H" != "-" ] && [ "$SERV_H" != "$LOCAL_H" ] \
  || fail "report server hash '$SERV_H'"
# neither side was overwritten: the laptop keeps its binding...
SERV_NOW=$(curl -sf -m 60 -G -H "Authorization: Bearer $OWNER" \
  --data-urlencode 'prefix=ns/lap/dict/e2eowner/' --data-urlencode 'limit=5000' \
  "$BASE/api/fed/ops" | python3 -c "
import json,sys
d=json.load(sys.stdin)
hits=[o for o in d['ops'] if o.get('path')=='ns/lap/dict/e2eowner/greet']
print(hits[-1]['value_hash'] if hits else '')")
[ "$SERV_NOW" = "$LOCAL_DEF" ] || fail "the pushed copy changed: $SERV_NOW"
# ...and the server keeps its own
SRV_COPY=$(curl -sf -m 120 -X POST -H "Authorization: Bearer $OWNER" \
  --data-binary '{"command":"dict"}' "$BASE/api/repl" \
  | python3 -c "
import hashlib, json, sys
rows = json.load(sys.stdin)['round'].get('dictionary') or []
t = [r['ternary'] for r in rows if r.get('name')=='greet'][0] if rows else ''
print(hashlib.sha256(t.encode()).hexdigest() if t else '')")
[ "$SRV_COPY" = "$SERV_H" ] || fail "the server copy changed: $SRV_COPY != $SERV_H"
echo "  [19.3] divergence greet local $LOCAL_H server $SERV_H, neither overwritten"

echo "verify-19: L6 local peer sync OK"
