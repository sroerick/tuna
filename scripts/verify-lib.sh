#!/bin/bash
# Shared preamble for the M10 acceptance verify scripts (one per
# acceptance.criteria item 1-7).  Each verify-N script is callable in
# ANY order and exits 0 on green — so this library must be sourced, not
# executed, and every script must be self-contained on top of it.
#
# Env:
#   TUNA_HTTP_PORT   server port     (default 18090)
#   TUNA_SMOKE_TOKEN bearer token    (default: /tmp/tuna-dev/bootstrap.token,
#                                     then the boot log)
#   TUNA_DB_*        psql connection (socket /tmp, port 5434, db tuna, user tuna)
#
# Requires: a live dev server (scripts/dev.sh start), psql, python3.

BASE="http://127.0.0.1:${TUNA_HTTP_PORT:-18090}"
DEV_DIR="${TUNA_DEV_DIR:-/tmp/tuna-dev}"

fail() { echo "FAIL[$0]: $*" >&2; exit 1; }

TOKEN="${TUNA_SMOKE_TOKEN:-}"
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/bootstrap.token" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/server.log" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || fail "no bearer token: set TUNA_SMOKE_TOKEN or boot the server (scripts/dev.sh start)"
AUTH="Authorization: Bearer $TOKEN"

# every curl carries -m: a hung request must FAIL the script, not wedge
# it (the server compiles synchronously; a pathological program can
# block the runtime — fail loudly, never hang).
curl_code() { curl -s -m 300 -o /dev/null -w '%{http_code}' "$@"; }

# json path helper: jget "d['run']['id']"
jget() { python3 -c "import json,sys;d=json.load(sys.stdin);print(eval(sys.argv[1]))" "$1"; }

# psql shorthand against the dev database (acceptance queries run
# against journal+run rows as DATA — no log grep)
psql_q() {
  psql -h "${TUNA_DB_HOST:-/tmp}" -p "${TUNA_DB_PORT:-5434}" \
       -U "${TUNA_DB_USER:-tuna}" -d "${TUNA_DB_NAME:-tuna}" "$@"
}

# a live, unrevoked grant for prim $1 (freshly minted: scripts run in
# any order, so nothing is assumed about prior state)
mint_grant() {
  curl -sf -m 60 -X POST -H "$AUTH" --data-binary "{\"prim\":\"$1\"}" \
    $BASE/api/grants | jget "d['id']" || fail "mint $1 grant"
}

# compile $1 from source (provenance ir included), echo the hash
post_source() {
  curl -sf -m 300 -X POST -H "$AUTH" --data-binary "{\"source\":$1}" \
    $BASE/api/programs | jget "d['hash']" || fail "POST source program $1"
}

# post a bare ternary program, echo the hash
post_ternary() {
  curl -sf -m 60 -X POST -H "$AUTH" --data-binary "{\"ternary\":\"$1\"}" \
    $BASE/api/programs | jget "d['hash']" || fail "POST ternary $1"
}

# run $1 (program hash) with inputs $2 (json array literal), grants $3
# (json array literal, may be empty) — echoes the response
do_run() {
  curl -sf -m 300 -X POST -H "$AUTH" --data-binary \
    "{\"program_hash\":\"$1\",\"inputs\":$2,\"grants\":$3,\"fuel\":${4:-10000},\"size_cap\":100000}" \
    $BASE/api/runs || fail "POST run of $1"
}

# fetch a run (auto-verifies an unverified finished run) and echo
# verify_status
run_verify_status() {
  curl -sf -H "$AUTH" $BASE/api/runs/$1 | jget "d['run']['verify_status']"
}
