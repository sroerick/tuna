#!/bin/bash
# M8 acceptance smoke: exercise the whole htmx UI (server-rendered
# pages, session-cookie auth, htmx fragments, no-JS degradation)
# against a live server (scripts/dev.sh start).  Exit 0 on green.
#
# Env:
#   TUNA_HTTP_PORT   server port   (default 18090)
#   TUNA_SMOKE_TOKEN bearer token  (default: from /tmp/tuna-dev/bootstrap.token)
#
# Session auth note: the UI session cookie carries the identity token
# itself (ONE credential store, identities.token_hash); the script logs
# in through POST /login exactly like a browser would.
set -e

BASE="http://127.0.0.1:${TUNA_HTTP_PORT:-18090}"
LOG="${TUNA_DEV_DIR:-/tmp/tuna-dev}/server.log"
JAR=$(mktemp)
trap 'rm -f "$JAR"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }

TOKEN="${TUNA_SMOKE_TOKEN:-}"
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "${TUNA_DEV_DIR:-/tmp/tuna-dev}/bootstrap.token" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$LOG" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || fail "no token: set TUNA_SMOKE_TOKEN or boot the server"

jget() { python3 -c "import json,sys;d=json.load(sys.stdin);print(eval(sys.argv[1]))" "$1"; }
curl_code() { curl -s -o /dev/null -w '%{http_code}' "$@"; }

echo "[1] /health open; static assets served"
curl -sf "$BASE/health" | grep -q '"db":true' || fail "health"
curl_code "$BASE/static/htmx.min.js" | grep -q 200 || fail "htmx.min.js not served"

echo "[2] anonymous page requests redirect to /login"
curl_code "$BASE/" -H 'HX-Request: true' | grep -q 303 || fail "anon / (htmx)"
curl_code "$BASE/grants" | grep -q 303 || fail "anon /grants"
curl_code "$BASE/repl" | grep -q 303 || fail "anon /repl"

echo "[3] login: bad token 401, good token sets session cookie"
curl_code -d 'token=nonsense' "$BASE/login" | grep -q 401 || fail "bad token must 401"
curl -s -c "$JAR" -o /dev/null -w '%{http_code}' -d "token=$TOKEN" "$BASE/login" \
  | grep -q 303 || fail "good token must 303"
grep -q 'tuna_session' "$JAR" || fail "no session cookie set"

echo "[4] dashboard renders with runs table"
DASH=$(curl -sf -b "$JAR" "$BASE/")
echo "$DASH" | grep -q 'id="runs-box"' || fail "no runs table"
echo "$DASH" | grep -q 'hx-get="/frag/runs"' || fail "no htmx tick on runs table"
echo "$DASH" | grep -q 'noscript' || fail "dashboard must degrade to pure HTML"

echo "[5] /frag/runs: htmx fragment and no-JS page variant"
FRAG=$(curl -sf -b "$JAR" -H 'HX-Request: true' "$BASE/frag/runs")
echo "$FRAG" | grep -q 'id="runs-box"' || fail "runs fragment"
curl -sf -b "$JAR" "$BASE/frag/runs" | grep -q '<html' || fail "no-JS frag/runs must be a page"

echo "[6] seed a program + run through the API"
HASH=$(curl -sf -X POST -H "Authorization: Bearer $TOKEN" \
  --data-binary '{"source":"(lambda (x) (%22102000 x))"}' "$BASE/api/programs" \
  | jget "d['hash']") || fail "seed program"
RUN=$(curl -sf -X POST -H "Authorization: Bearer $TOKEN" \
  -d "{\"program_hash\":\"$HASH\",\"inputs\":[\"22102000\"]}" "$BASE/api/runs" \
  | jget "d['run']['id']") || fail "seed run"

echo "[7] program page: ternary, tree, provenance, patch + run forms"
PG=$(curl -sf -b "$JAR" "$BASE/programs/$HASH")
echo "$PG" | grep -q 'canonical ternary' || fail "program page missing ternary"
echo "$PG" | grep -q '<h3>tree</h3>' || fail "program page missing tree render"
echo "$PG" | grep -q 'hx-post="/programs/'"$HASH"'/patch"' || fail "no patch form"
echo "$PG" | grep -q 'hx-post="/programs/'"$HASH"'/run"' || fail "no run form"
curl_code "$BASE/programs/deadbeef" -b "$JAR" | grep -q 404 || fail "unknown program must 404"

echo "[8] program lookup redirect"
curl_code -b "$JAR" "$BASE/programs/lookup?hash=$HASH" | grep -q 303 || fail "lookup must redirect"

echo "[9] run page: badges, journal, verify"
RP=$(curl -sf -b "$JAR" "$BASE/runs/$RUN")
echo "$RP" | grep -q 'badge ok">normal' || fail "run page status badge"
echo "$RP" | grep -q 'badge ok">verified' || fail "auto-verify on fetch"
echo "$RP" | grep -q 'id="journal-box"' || fail "journal table"

echo "[10] journal tick fragment (htmx + no-JS)"
curl -sf -b "$JAR" -H 'HX-Request: true' "$BASE/runs/$RUN/journal" \
  | grep -q 'id="journal-box"' || fail "journal fragment"
curl -sf -b "$JAR" "$BASE/runs/$RUN/journal" | grep -q '<html' \
  || fail "no-JS journal must be a page"

echo "[11] patch form: CAS apply (htmx), no-JS 303, bad path 404, conflict 409"
NEW=$(curl -sf -b "$JAR" -H 'HX-Request: true' \
  --data-urlencode "expected_old_hash=$HASH" --data-urlencode 'path=' \
  --data-urlencode 'new_ternary=21100' "$BASE/programs/$HASH/patch") \
  || fail "patch apply"
echo "$NEW" | grep -q 'applied' || fail "patch apply fragment"
NEW_HASH=$(echo "$NEW" | grep -o '/programs/[0-9a-f]*' | head -1 | sed 's#/programs/##')
curl_code -b "$JAR" --data-urlencode "expected_old_hash=$HASH" \
  --data-urlencode 'path=' --data-urlencode 'new_ternary=21100' \
  "$BASE/programs/$HASH/patch" | grep -q 303 || fail "no-JS patch must 303"
curl -s -b "$JAR" -H 'HX-Request: true' --data-urlencode "expected_old_hash=$NEW_HASH" \
  --data-urlencode 'path=/9' --data-urlencode 'new_ternary=0' \
  "$BASE/programs/$NEW_HASH/patch" | grep -q 'does not address a subtree' \
  || fail "bad path must surface an error fragment"
curl_code -b "$JAR" --data-urlencode "expected_old_hash=$NEW_HASH" \
  --data-urlencode 'path=/9' --data-urlencode 'new_ternary=0' \
  "$BASE/programs/$NEW_HASH/patch" | grep -q 404 || fail "no-JS bad path must 404"
curl -s -b "$JAR" -H 'HX-Request: true' --data-urlencode "expected_old_hash=0000000000000000000000000000000000000000000000000000000000000000" \
  --data-urlencode 'path=' --data-urlencode 'new_ternary=0' \
  "$BASE/programs/$NEW_HASH/patch" | grep -q 'patch conflict' \
  || fail "conflict must surface an error fragment"
curl_code -b "$JAR" --data-urlencode "expected_old_hash=0000000000000000000000000000000000000000000000000000000000000000" \
  --data-urlencode 'path=' --data-urlencode 'new_ternary=0' \
  "$BASE/programs/$NEW_HASH/patch" | grep -q 409 || fail "no-JS conflict must 409"

echo "[12] run form (htmx + no-JS redirect)"
curl -sf -b "$JAR" -H 'HX-Request: true' --data-urlencode 'inputs=22102000' \
  -d 'fuel=1000&size_cap=1000' "$BASE/programs/$HASH/run" \
  | grep -q 'ran' || fail "run form fragment"
curl_code -b "$JAR" --data-urlencode 'inputs=22102000' -d 'fuel=1000&size_cap=1000' \
  "$BASE/programs/$HASH/run" | grep -q 303 || fail "no-JS run must 303"

echo "[13] repl: htmx round-trip, no-JS page, compile errors surfaced"
curl -sf -b "$JAR" -H 'HX-Request: true' \
  --data-urlencode 'source=(lambda (x) (%22102000 x))' --data-urlencode 'inputs=22102000' \
  -d 'fuel=1000&size_cap=1000' "$BASE/repl/eval" | grep -q 'badge ok">normal' \
  || fail "repl htmx round-trip"
curl -sf -b "$JAR" --data-urlencode 'source=(lambda (x) (%22102000 x))' \
  --data-urlencode 'inputs=22102000' -d 'fuel=1000&size_cap=1000' "$BASE/repl/eval" \
  | grep -q '<html' || fail "no-JS repl must be a page"
curl -sf -b "$JAR" -H 'HX-Request: true' --data-urlencode 'source=(q x)' \
  "$BASE/repl/eval" | grep -q 'compile error' || fail "repl must surface compile errors"
curl -sf -b "$JAR" "$BASE/repl" | grep -q 'repl' || fail "repl page"

echo "[14] grants admin: page, mint (htmx), revoke (htmx), unknown prim 400"
curl -sf -b "$JAR" "$BASE/grants" | grep -q 'args_attenuation' || fail "grants page"
curl -sf -b "$JAR" -H 'HX-Request: true' -d 'prim=echo&args_attenuation={}' \
  "$BASE/grants/mint" | grep -q 'minted' || fail "mint fragment"
GID=$(curl -sf -b "$JAR" "$BASE/grants" | grep -o 'grant-[0-9a-f-]*' | head -1 | sed 's/grant-//')
[ -n "$GID" ] || fail "no grant row listed"
curl -sf -b "$JAR" -H 'HX-Request: true' -X POST "$BASE/grants/$GID/revoke" \
  | grep -q 'revoked' || fail "revoke fragment"
curl_code -b "$JAR" -H 'HX-Request: true' -d 'prim=nope&args_attenuation={}' \
  "$BASE/grants/mint" | grep -q 400 || fail "unknown prim must 400"

echo "[15] verify button detects out-of-band journal tamper"
TGRANT=$(curl -sf -X POST -H "Authorization: Bearer $TOKEN" -d '{"prim":"echo","args_attenuation":{}}' \
  "$BASE/api/grants" | jget "d['id']") || fail "tamper-test grant"
THASH=$(curl -sf -X POST -H "Authorization: Bearer $TOKEN" \
  --data-binary '{"source":"(lambda (x) (prim \"echo\" x))"}' "$BASE/api/programs" \
  | jget "d['hash']") || fail "tamper-test program"
TRUN=$(curl -sf -X POST -H "Authorization: Bearer $TOKEN" \
  -d "{\"program_hash\":\"$THASH\",\"inputs\":[\"10\"],\"grants\":[\"$TGRANT\"]}" "$BASE/api/runs" \
  | jget "d['run']['id']") || fail "tamper-test run"
psql -h /tmp -p "${TUNA_DB_PORT:-5434}" -U "${TUNA_DB_USER:-tuna}" -d "${TUNA_DB_NAME:-tuna}" -q \
  -c "UPDATE journals SET result_ternary = '22102000' WHERE run_id='$TRUN' AND seq=0" \
  || fail "out-of-band journal tamper"
curl -sf -b "$JAR" -H 'HX-Request: true' -X POST "$BASE/runs/$TRUN/verify" \
  | grep -q 'badge bad">verify failed' || fail "tampered run must fail UI verify"
curl -sf -b "$JAR" "$BASE/runs/$TRUN" | grep -q 'badge ok">normal' \
  || fail "tampered run row still shows recorded status (history immutable)"

echo "[16] logout drops the session"
curl_code -b "$JAR" "$BASE/logout" | grep -q 303 || fail "logout must redirect"
curl -s -b "$JAR" -c "$JAR" -o /dev/null "$BASE/logout"
grep -q 'tuna_session' "$JAR" && fail "session cookie must be expired by logout" || true

rm -f "$JAR"
echo "smoke-ui: ALL GREEN"
