#!/usr/bin/env bash
# pp-slice acceptance smoke: the public face + accounts tier over a live
# server.  Exit 0 on green.  Honors the instance's isolated slots:
#   TUNA_HTTP_PORT (default 18092), TUNA_DEV_DIR (default /tmp/tuna-pp-dev),
#   TUNA_PUBLIC_DIR (default /tmp/tuna-pp-public).
#
# Runs against a server started by scripts/serve.sh (or dev.sh with the
# same TUNA_* env).  Requires the operator/root bearer token from
# $TUNA_DEV_DIR/bootstrap.token (or TUNA_SMOKE_TOKEN).
set -e

BASE="http://127.0.0.1:${TUNA_HTTP_PORT:-18092}"
DEV_DIR="${TUNA_DEV_DIR:-/tmp/tuna-pp-dev}"
LOG="$DEV_DIR/server.log"

fail() { echo "FAIL: $*" >&2; exit 1; }
code() { curl -s -o /dev/null -w '%{http_code}' "$@"; }
jget() { python3 -c "import json,sys;d=json.load(sys.stdin);print(eval(sys.argv[1]))" "$1"; }

TOKEN="${TUNA_SMOKE_TOKEN:-}"
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/bootstrap.token" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$LOG" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || fail "no bearer token: set TUNA_SMOKE_TOKEN or boot the server"
AUTH="Authorization: Bearer $TOKEN"

echo "[1] public surfaces are anonymous"
for path in /welcome /code /agent.txt /.well-known/agent.json /health; do
  c=$(code "$BASE$path")
  [ "$c" = 200 ] || fail "$path expected 200, got $c"
done

echo "[2] welcome names the calculus"
curl -sf "$BASE/welcome" | grep -q 'Fork of t' || fail "welcome body"
curl -sf "$BASE/welcome" | grep -q '/src.tgz' || fail "welcome src.tgz link"

echo "[3] /code lists programs/runs"
curl -sf "$BASE/code" | grep -qi '<h2>code' || fail "/code heading"

echo "[4] agent.txt is prose with the auth model"
curl -sf "$BASE/agent.txt" | grep -q 'Bearer' || fail "agent.txt bearer"
curl -sf "$BASE/agent.txt" | grep -q '/api/runs' || fail "agent.txt runs endpoint"

echo "[5] agent.json is valid JSON with endpoints + auth"
curl -sf "$BASE/.well-known/agent.json" \
  | python3 -c "import json,sys;d=json.load(sys.stdin);assert 'auth' in d and 'endpoints' in d" \
  || fail "agent.json shape"

echo "[6] src.tgz is served as a gzip tarball"
curl -sf "$BASE/src.tgz" -o /tmp/.tuna-src.tgz || fail "src.tgz fetch"
python3 -c "import tarfile;tarfile.open('/tmp/.tuna-src.tgz','r:gz')" \
  || fail "src.tgz is not a gzipped tar"
rm -f /tmp/.tuna-src.tgz

echo "[7] /api/* still 401 without auth; bearer works"
[ "$(code $BASE/api/runs)" = 401 ] || fail "expected 401"
curl -sf -H "$AUTH" "$BASE/api/runs" >/dev/null || fail "bearer GET /api/runs"

echo "[8] admin mints a NON-ADMIN identity + token (T2)"
AGENT_NAME="smoke-public-$$"
MINT=$(curl -sf -X POST -H "$AUTH" -H 'Content-Type: application/json' \
  --data-binary "{\"name\":\"$AGENT_NAME\",\"password\":\"smoke-public-pw\"}" \
  "$BASE/api/identities") || fail "POST /api/identities"
AGENT_TOKEN=$(echo "$MINT" | jget "d['token']")
[ "$(echo "$MINT" | jget "d['is_admin']")" = False ] || fail "minted identity must be non-admin"
[ -n "$AGENT_TOKEN" ] || fail "mint returned no token"

echo "[9] the minted token authenticates; a non-admin cannot mint identities"
curl -sf -H "Authorization: Bearer $AGENT_TOKEN" "$BASE/api/runs" >/dev/null \
  || fail "minted token GET /api/runs"
[ "$(code -X POST -H "Authorization: Bearer $AGENT_TOKEN" -H 'Content-Type: application/json' \
     --data-binary '{"name":"escalate"}' "$BASE/api/identities")" = 403 ] \
  || fail "non-admin must be 403 on identity mint"

echo "[10] password login mints a session cookie; logout revokes it"
rm -f /tmp/.tuna-jar
[ "$(curl -s -o /dev/null -w '%{http_code}' -c /tmp/.tuna-jar \
     --data-urlencode "username=$AGENT_NAME" \
     --data-urlencode 'password=smoke-public-pw' "$BASE/login")" = 303 ] \
  || fail "password login redirect"
grep -q 'tuna_session' /tmp/.tuna-jar || fail "no session cookie set"
# the session cookie drives /api/* (browser tier fallback)
curl -sf -b /tmp/.tuna-jar "$BASE/api/runs" >/dev/null || fail "session cookie on /api/runs"
curl -s -o /dev/null -b /tmp/.tuna-jar -c /tmp/.tuna-jar "$BASE/logout" || fail "logout"
[ "$(code -b /tmp/.tuna-jar "$BASE/api/runs")" = 401 ] || fail "revoked session must 401"
rm -f /tmp/.tuna-jar

echo "[11] /identities page: admin sees the mint form (session)"
if [ -n "${TUNA_BOOTSTRAP_PASSWORD:-}" ]; then
  rm -f /tmp/.tuna-root-jar
  curl -s -o /dev/null -c /tmp/.tuna-root-jar \
    --data-urlencode 'username=root' \
    --data-urlencode "password=$TUNA_BOOTSTRAP_PASSWORD" "$BASE/login" \
    || fail "root password login"
  curl -sf -b /tmp/.tuna-root-jar "$BASE/identities" | grep -q 'identities/mint' \
    || fail "admin /identities form missing"
  rm -f /tmp/.tuna-root-jar
else
  echo "    (root password not set; skipping admin-page assertion)"
fi

echo "[12] unknown public path is a plain 404"
[ "$(code "$BASE/no-such-route")" = 404 ] || fail "expected 404"

echo "smoke-public: all sections green"
