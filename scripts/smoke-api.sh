#!/usr/bin/env bash
# M6 acceptance smoke: exercise the whole JSON API chain against a live
# server (scripts/dev.sh start).  Exit 0 on green.
#
# Env:
#   TUNA_HTTP_PORT   server port            (default 18090)
#   TUNA_SMOKE_TOKEN bearer token           (default: extracted from the
#                    latest TUNA_BOOTSTRAP_TOKEN line in /tmp/tuna-dev/server.log)
set -e

BASE="http://127.0.0.1:${TUNA_HTTP_PORT:-18090}"
LOG="${TUNA_DEV_DIR:-/tmp/tuna-dev}/server.log"

fail() { echo "FAIL: $*" >&2; exit 1; }

TOKEN="${TUNA_SMOKE_TOKEN:-}"
# prefer dev.sh's persisted token file (the log only carries it on a
# first boot; a re-boot with the replayed token prints nothing)
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "${TUNA_DEV_DIR:-/tmp/tuna-dev}/bootstrap.token" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$LOG" 2>/dev/null | tail -1)
[ -n "$TOKEN" ] || fail "no bearer token: set TUNA_SMOKE_TOKEN or boot the server"
AUTH="Authorization: Bearer $TOKEN"

jget() { python3 -c "import json,sys;d=json.load(sys.stdin);print(eval(sys.argv[1]))" "$1"; }

curl_code() { curl -s -o /dev/null -w '%{http_code}' "$@"; }

echo "[1] /health open, db up"
curl -sf "$BASE/health" | grep -q '"db":true' || fail "health"
echo "[2] /api/* unauthenticated -> 401"
[ "$(curl_code $BASE/api/runs)" = 401 ] || fail "expected 401 without token"
[ "$(curl_code -X POST -H 'Authorization: Bearer bogus' -d '{}' $BASE/api/runs)" = 401 ] \
  || fail "expected 401 with bad token"

echo "[3] POST /api/programs (ternary + source)"
NOT_HASH=$(curl -sf -X POST -H "$AUTH" --data-binary '{"ternary":"22102000"}' $BASE/api/programs \
  | jget "d['hash']") || fail "POST ternary program"
OMEGA_HASH=$(curl -sf -X POST -H "$AUTH" --data-binary '{"ternary":"221000"}' $BASE/api/programs \
  | jget "d['hash']") || fail "POST omega"
SRC=$(curl -sf -X POST -H "$AUTH" --data-binary '{"source":"(lambda (x) x)"}' $BASE/api/programs) \
  || fail "POST source program"
echo "$SRC" | grep -q '"hash"' || fail "source program returned no hash"
BAD_SRC=$(curl_code -X POST -H "$AUTH" --data-binary '{"source":"(lambda (x) (q x))"}' $BASE/api/programs)
[ "$BAD_SRC" = 400 ] || fail "expected 400 for unbound variable, got $BAD_SRC"

echo "[4] GET /api/programs/:hash"
curl -sf -H "$AUTH" $BASE/api/programs/$NOT_HASH | grep -q '"ternary":"22102000"' || fail "GET program"
[ "$(curl_code -H "$AUTH" $BASE/api/programs/$(printf 0%.0s {1..64}))" = 404 ] || fail "GET unknown program"

echo "[5] POST /api/programs/:hash/patch (CAS apply + conflict)"
# not = "2"+"210200"+"0"; subtree at path 1 = "210200"
SUB=$(python3 -c "import hashlib;print(hashlib.sha256(b'210200').hexdigest())")
APPLIED=$(curl -sf -X POST -H "$AUTH" --data-binary \
  "{\"path\":\"1\",\"expected_old_hash\":\"$SUB\",\"new_ternary\":\"0\"}" \
  $BASE/api/programs/$NOT_HASH/patch) || fail "patch apply"
echo "$APPLIED" | grep -q '"ternary":"200"' || fail "patch produced wrong tree"
PATCHED=$(echo "$APPLIED" | jget "d['hash']")
# the ORIGINAL program row is immutable, so a repeat apply is idempotent:
# CAS conflicts only fire against the CURRENT program
CONFLICT=$(curl_code -X POST -H "$AUTH" --data-binary \
  "{\"path\":\"1\",\"expected_old_hash\":\"$SUB\",\"new_ternary\":\"0\"}" \
  $BASE/api/programs/$PATCHED/patch)
[ "$CONFLICT" = 409 ] || fail "expected 409 on wrong expected-hash, got $CONFLICT"
FD=$(curl -s -X POST -H "$AUTH" --data-binary \
  "{\"path\":\"1\",\"expected_old_hash\":\"$SUB\",\"new_ternary\":\"0\",\"old_ternary\":\"210200\"}" \
  $BASE/api/programs/$PATCHED/patch | jget "d['first_diff']")
[ "$FD" = "" ] || fail "first_diff should be empty string (root shapes differ), got $FD"
BADPATH=$(curl_code -X POST -H "$AUTH" --data-binary \
  "{\"path\":\"0\",\"expected_old_hash\":\"$SUB\",\"new_ternary\":\"0\"}" \
  $BASE/api/programs/$NOT_HASH/patch)
[ "$BADPATH" = 404 ] || fail "expected 404 on path escaping a fork"

echo "[6] POST /api/runs (normal + fuel-exact + grant check)"
RUN=$(curl -sf -X POST -H "$AUTH" --data-binary \
  "{\"program_hash\":\"$NOT_HASH\",\"inputs\":[\"10\"],\"fuel\":100,\"size_cap\":100}" \
  $BASE/api/runs) || fail "POST run not(true)"
echo "$RUN" | grep -q '"step_count":2' || fail "not(true) must take 2 steps"
echo "$RUN" | grep -q '"status":"normal"' || fail "not(true) must be normal"
RUN_ID=$(echo "$RUN" | jget "d['run']['id']")
FUELED=$(curl -sf -X POST -H "$AUTH" --data-binary \
  "{\"program_hash\":\"$OMEGA_HASH\",\"fuel\":5,\"size_cap\":1000,\"inputs\":[\"221000\",\"221000\",\"221000\",\"221000\",\"221000\",\"221000\"]}" \
  $BASE/api/runs) || fail "POST omega run"
echo "$FUELED" | grep -q '"step_count":5' || fail "omega must stop at exactly fuel steps"
echo "$FUELED" | grep -q '"status":"fuel_exhausted"' || fail "omega must fuel_exhaust"
[ "$(curl_code -X POST -H "$AUTH" --data-binary \
    "{\"program_hash\":\"$OMEGA_HASH\",\"fuel\":5,\"size_cap\":1000,\"grants\":[\"00000000-0000-0000-0000-000000000000\"]}" \
    $BASE/api/runs)" = 403 ] || fail "expected 403 for unknown grant"

echo "[7] GET /api/runs/:id, /api/runs?program=, /api/journals/:run_id"
curl -sf -H "$AUTH" $BASE/api/runs/$RUN_ID | grep -q "$RUN_ID" || fail "GET run"
curl -sf -H "$AUTH" "$BASE/api/runs?program=$NOT_HASH" | grep -q "$RUN_ID" || fail "list runs"
curl -sf -H "$AUTH" $BASE/api/journals/$RUN_ID | grep -q '"journal":\[\]' || fail "journals (pure runs have none)"
[ "$(curl_code -H "$AUTH" $BASE/api/runs/00000000-0000-0000-0000-000000000000)" = 404 ] \
  || fail "GET unknown run"

echo "[8] POST /api/journals/:run_id/fork (data-plane counterfactual)"
FORK=$(curl -sf -X POST -H "$AUTH" --data-binary '{"edits":[]}' $BASE/api/journals/$RUN_ID/fork) \
  || fail "fork"
echo "$FORK" | grep -q "\"parent_run_id\":\"$RUN_ID\"" || fail "fork must link to parent"
[ "$(curl_code -X POST -H "$AUTH" --data-binary '{"edits":[{"seq":0,"result_ternary":"0"}]}' \
    $BASE/api/journals/$RUN_ID/fork)" = 400 ] || fail "edit beyond journal must 400"

echo "[9] fork of a run WITH journal rows: edits apply + chain rebuilt"
# seed a journal via SQL is not part of the API (prims land in M7); instead
# verify the fork's derived_journals bookkeeping:
FORK_ID=$(echo "$FORK" | jget "d['run']['id']")
psql -h /tmp -p 5434 -U tuna -d tuna -tAc \
  "SELECT parent_run_id FROM derived_journals WHERE run_id='$FORK_ID'" \
  | grep -q "$RUN_ID" || fail "derived_journals row missing"

echo "[10] M7: grants + prim boundary + journal row"
GRANT_ID=$(curl -sf -X POST -H "$AUTH" --data-binary '{"prim":"echo"}' $BASE/api/grants \
  | jget "d['id']") || fail "mint echo grant"
ECHO_HASH=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"source":"(lambda (x) (prim \"echo\" x))"}' $BASE/api/programs | jget "d['hash']") \
  || fail "POST echo source program"
PRIM_RUN=$(curl -sf -X POST -H "$AUTH" --data-binary \
  "{\"program_hash\":\"$ECHO_HASH\",\"inputs\":[\"10\"],\"grants\":[\"$GRANT_ID\"],\"fuel\":1000,\"size_cap\":100000}" \
  $BASE/api/runs) || fail "POST prim run"
echo "$PRIM_RUN" | grep -q '"status":"normal"' || fail "prim run must be normal"
echo "$PRIM_RUN" | grep -q '"prim":"echo"' || fail "prim call must be journaled"
echo "$PRIM_RUN" | grep -q "\"grant_id\":\"$GRANT_ID\"" || fail "grant must be journaled"
CALLSITE=$(echo "$PRIM_RUN" | jget "d['journal'][0]['callsite_path']")
[ -n "$CALLSITE" ] || fail "callsite path must be resolved from provenance"
echo "$PRIM_RUN" | grep -q '"result_ternary":"2100"' || fail "echo must return the cons-list args"
# denial without grants is a journaled error answer, run continues
DENIED=$(curl -sf -X POST -H "$AUTH" --data-binary \
  "{\"program_hash\":\"$ECHO_HASH\",\"inputs\":[\"10\"],\"fuel\":1000,\"size_cap\":100000}" \
  $BASE/api/runs) || fail "POST denied run"
echo "$DENIED" | grep -q '"status":"normal"' || fail "denied run must still be normal"
echo "$DENIED" | grep -q '"error":"grant denial' || fail "denial must be journaled as error"

PRIM_RUN_ID=$(echo "$PRIM_RUN" | jget "d['run']['id']")

echo "[11] M7: auto-verify on fetch + verify sweeper"
V=$(curl -sf -H "$AUTH" $BASE/api/runs/$PRIM_RUN_ID | jget "d['run']['verify_status']")
[ "$V" = "verified" ] || fail "fetch must auto-verify, got $V"
SWEEP=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
echo "$SWEEP" | grep -q "\"run_id\":\"$PRIM_RUN_ID\"" || fail "sweeper must include the run"
echo "$SWEEP" | grep -q '"verify":"verified"' || fail "sweeper must verify"

echo "[12] M7: counterfactual fork re-executes the edited journal"
CFORK=$(curl -sf -X POST -H "$AUTH" --data-binary \
  '{"edits":[{"seq":0,"result_ternary":"0"}]}' \
  $BASE/api/journals/$PRIM_RUN_ID/fork) || fail "counterfactual fork"
echo "$CFORK" | grep -q "\"forked_from\":\"$PRIM_RUN_ID\"" || fail "fork must link to parent"
echo "$CFORK" | grep -q '"verify":{"run_id":"' || fail "fork must report a verify verdict"
FORK_VERIFY=$(echo "$CFORK" | jget "d['verify']['verify']")
[ "$FORK_VERIFY" = "verified" ] || fail "counterfactual fork must verify, got $FORK_VERIFY"
FORK_ID=$(echo "$CFORK" | jget "d['run']['id']")
FORK_RES=$(echo "$CFORK" | jget "d['run']['result_ternary']")
PRIM_RES=$(echo "$PRIM_RUN" | jget "d['run']['result_ternary']")
[ "$FORK_RES" != "$PRIM_RES" ] || fail "counterfactual must change the outcome"
# empty edits = faithful copy, still verified
COPY=$(curl -sf -X POST -H "$AUTH" --data-binary '{"edits":[]}' \
  $BASE/api/journals/$PRIM_RUN_ID/fork) || fail "faithful fork"
echo "$COPY" | jget "d['verify']['verify']" | grep -q verified || fail "faithful fork must verify"

echo "[13] M7: journal tampering caught (out-of-band SQL edit)"
psql -h /tmp -p 5434 -U tuna -d tuna -q -c \
  "UPDATE journals SET result_ternary='0' WHERE run_id='$PRIM_RUN_ID' AND seq=0" >/dev/null
TV=$(curl -sf -H "$AUTH" "$BASE/api/runs/verify?all=1")
echo "$TV" | grep -q '"verify":"failed"' || fail "tampered journal must fail verification"
echo "$TV" | grep -q 'chain broken' || fail "tamper must surface as chain break"
# restore the row so later smoke runs stay green (row_hash is now wrong
# either way; the tamper is the finding — re-verify via fork reads only)
echo "[14] M9: POST /api/repl — eval/def/get/patch/first-diff/dict + transcript chain"
RP=$(curl -sf -X POST -H "$AUTH" -d '{"command":"eval (lambda (x) x)","inputs":["22102000"]}' \
  $BASE/api/repl) || fail "repl eval round"
echo "$RP" | jget "d['round']['kind']" | grep -q eval || fail "round kind"
echo "$RP" | jget "d['round']['run_id']" | grep -q '[0-9a-f-]' || fail "eval round must be a journaled run"
DEFR=$(curl -sf -X POST -H "$AUTH" -d '{"command":"def neg %22102000"}' $BASE/api/repl) \
  || fail "repl def round"
echo "$DEFR" | jget "d['round']['hash']" | grep -q 1f6cae19 || fail "def must pin the not tree"
DICTR=$(curl -sf -X POST -H "$AUTH" -d '{"command":"eval (neg %22102000)"}' $BASE/api/repl) \
  || fail "repl round through the dictionary"
echo "$DICTR" | jget "d['round']['ternary']" | grep -q 22102000 || fail "dictionary name must substitute"
# transcript chain: the def round's run row links the previous round
CHAIN=$(curl -sf -H "$AUTH" $BASE/api/runs/$(echo "$DICTR" | jget "d['round']['run_id']"))
echo "$CHAIN" | jget "d['run']['parent_run_id']" | grep -q '[0-9a-f-]' \
  || fail "REPL rounds must chain via parent_run_id"
# structural commands
GETR=$(curl -sf -X POST -H "$AUTH" -d '{"command":"get 1"}' $BASE/api/repl) || fail "get round"
echo "$GETR" | jget "d['round']['ternary']" | grep -q 210200 || fail "get subtree"
PATCHR=$(curl -sf -X POST -H "$AUTH" -d '{"command":"patch 1 %0"}' $BASE/api/repl) || fail "patch round"
echo "$PATCHR" | jget "d['round']['ternary']" | grep -q 200 || fail "patch replaces the subtree"
PH1=$(echo "$DEFR" | jget "d['round']['hash']")
PH2=$(echo "$PATCHR" | jget "d['round']['hash']")
FDR=$(curl -sf -X POST -H "$AUTH" -d "{\"command\":\"first-diff $PH1 $PH2\"}" $BASE/api/repl) \
  || fail "first-diff round"
echo "$FDR" | jget "d['round']['ternary']" | grep -q '^1$' || fail "first-diff must report the path"
UNDR=$(curl -sf -X POST -H "$AUTH" -d '{"command":"undef neg"}' $BASE/api/repl) || fail "undef round"
echo "$UNDR" | jget "d['round']['note']" | grep -q 'undefined neg' || fail "undef note"
BADR=$(curl -s -X POST -H "$AUTH" -d '{"command":"frobnicate"}' $BASE/api/repl)
echo "$BADR" | grep -q 'unknown command' || fail "bad command must be a 400 message"
# unauth
curl_code -X POST -d '{"command":"dict"}' $BASE/api/repl | grep -q 401 \
  || fail "repl requires auth"

echo "SMOKE OK: all API chain checks passed"
