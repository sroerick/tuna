#!/usr/bin/env bash
# gate-receipt.sh - run one gate command and mint a verifiable receipt
# (borg/gate-receipts.borg, step G1).
#
#   TUNA_BASE=http://127.0.0.1:18190 TUNA_TOKEN=... \
#     scripts/gate-receipt.sh <gate-slug> [--adapter NAME] -- <command...>
#
# The wrapper does the host half:
#   1. record rev / tree_clean / diff_hash from the CURRENT work tree
#      (run it from the tree under test; a worktree is its own tree),
#   2. run the command with stdout+stderr captured to one log,
#   3. hash the log and store the bytes as a byte value,
#   4. run the harness adapter to get counts [run passed failed skipped],
#   5. encode the observation record and the skip-allowed list, store
#      both as tree values, and POST the verdict run (the prim-free
#      source is uploaded by hash on every call; dedup makes it cheap),
#   6. mint the run with claims.sh at
#        u.<runner>.claim.<date>.gate.<project>.<rev12>.<gate-slug>
#      and print the verdict and claim path.
#
# Adapters: alcotest | differential | make-test | unittest | none (default
# make-test).  A gate whose log no adapter can parse is recorded as
# counts=unparsed, and the verdict is unverified - never pass.
#
# Env:
#   TUNA_BASE                 server base (default http://127.0.0.1:18090)
#   TUNA_TOKEN                bearer (required)
#   TUNA_GATE_PROJECT         owning project (default: gate-slug prefix,
#                             else this script's project directory)
#   TUNA_GATE_ADAPTER         default adapter (same names as --adapter)
#   TUNA_GATE_SKIP_ALLOWED    comma-separated gate slugs whose skips pass
#   TUNA_GATE_FUEL            verdict run fuel (default 10000000)
#   TUNA_GATE_SIZE_CAP        verdict run size cap (default 1000000)
#   TUNA_GATE_KEEP_LOG=1      keep the captured log and print its path
#
# Exit: 0 when a receipt was minted (whatever the verdict says), 2 on an
# operational failure.  A fail/unverified verdict is data for the
# reviewer, not this script's exit code.
#
# The log is hashed exactly as captured.  Redaction is the runner's job
# (a redacted log is a different log hash, stated, never silent), so a
# gate that prints secrets must be wrapped/redacted by the caller.

set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"        # tuna/scripts
TUNA_ROOT="$(cd "$ROOT/.." && pwd)"          # tuna/
BASE="${TUNA_BASE:-http://127.0.0.1:18090}"
TOKEN="${TUNA_TOKEN:-}"

die() { echo "gate-receipt: $*" >&2; exit 2; }
usage() {
  echo "usage: gate-receipt.sh <gate-slug> [--adapter alcotest|differential|make-test|unittest|none] -- <command...>" >&2
  exit 2
}

[ -n "$TOKEN" ] || die "set TUNA_TOKEN to a bearer"
command -v curl >/dev/null 2>&1 || die "curl not found"
AUTH="Authorization: Bearer $TOKEN"
CURL="curl -s -m 300"

sha256hex() {
  if   command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1
  elif command -v sha256    >/dev/null 2>&1; then sha256 -q
  elif command -v openssl   >/dev/null 2>&1; then openssl sha256 | sed 's/.*= *//'
  else die "no sha256 tool on PATH"; fi
}
b64() {
  if   command -v base64  >/dev/null 2>&1; then base64 | tr -d '\n'
  elif command -v openssl >/dev/null 2>&1; then openssl base64 -A
  else die "no base64 tool on PATH"; fi
}
jstr() { sed -n "s/.*\"$1\":\"\([^\"]*\)\".*/\1/p" | head -n 1; }
jint() { sed -n "s/.*\"$1\":\([0-9]*\).*/\1/p"         | head -n 1; }
fetch() { $CURL -H "$AUTH" "$BASE$1"; }
post()  { $CURL -H "$AUTH" -H 'Content-Type: application/json' --data-binary "$2" "$BASE$1"; }
put_tree() { post /api/value/put "{\"ternary\":\"$1\"}" | jstr hash; }

# the verdict source has no quotes or backslashes, but keep the escaper
# honest for future edits: JSON string body on stdout, one line.
json_escape() {
  sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/\t/\\t/g' -e 's/\r/\\r/g' \
    | awk 'BEGIN { ORS = "" } { if (NR > 1) printf "\\n"; printf "%s", $0 }'
}

# -- arguments --------------------------------------------------------

gate="${1:-}"
[ -n "$gate" ] || usage
shift
adapter="${TUNA_GATE_ADAPTER:-make-test}"
while [ $# -gt 0 ]; do
  case "$1" in
    --adapter) [ $# -ge 2 ] || usage; adapter="$2"; shift 2 ;;
    --) shift; break ;;
    -*) die "unknown option $1 (put options before --)" ;;
    *) die "expected -- before the command" ;;
  esac
done
[ $# -ge 1 ] || usage
case "$adapter" in
  alcotest|differential|make-test|unittest|none) : ;;
  *) die "unknown adapter '$adapter' (alcotest|differential|make-test|unittest|none)" ;;
esac

# -- the tree under test ----------------------------------------------

git rev-parse --show-toplevel >/dev/null 2>&1 || die "not inside a git work tree"
rev=$(git rev-parse HEAD 2>/dev/null) || die "git rev-parse HEAD failed"
if [ -z "$(git status --porcelain)" ]; then
  tree_clean=1
  diff_hash=""
else
  tree_clean=0
  diff_hash=$(git diff HEAD | sha256hex)
fi

if [ -n "${TUNA_GATE_PROJECT:-}" ]; then
  project="$TUNA_GATE_PROJECT"
elif [ "${gate#*/}" != "$gate" ]; then
  project="${gate%%/*}"
else
  project="$(basename "$TUNA_ROOT")"
fi

# -- run the command, capture stdout+stderr in order ------------------

log=$(mktemp "${TMPDIR:-/tmp}/tuna-gate.XXXXXX.log")
keep_log="${TUNA_GATE_KEEP_LOG:-}"
cleanup() { [ -n "$keep_log" ] || rm -f "$log"; }
trap cleanup EXIT

command_str=$(printf '%q ' "$@")
command_str=${command_str% }

set +e
( "$@" ) >"$log" 2>&1
exit_status=$?
set -e

# -- the log is a content-addressed byte value ------------------------

log_hash=$(sha256hex <"$log")
payload=$(b64 <"$log")
resp=$(post /api/value/put "{\"bytes_b64\":\"$payload\"}") ||
  die "value/put of the log failed"
log_hash_server=$(printf '%s' "$resp" | jstr hash)
[ "$log_hash_server" = "$log_hash" ] ||
  die "server log hash $log_hash_server != local $log_hash (payload too large, or a non-sha256 store?)"

# -- adapter: counts [run passed failed skipped] or unparsed ----------

if [ "$adapter" = none ]; then
  counts=unparsed
  adapter_label="none"
else
  counts=$(bash "$ROOT/gate-receipts/adapt.sh" "$adapter" "$log" 2>/dev/null) || counts=unparsed
  adapter_label="adapt.sh/$adapter/v1"
  if [ "$counts" != unparsed ]; then
    valid=$(printf '%s\n' "$counts" |
      awk '{ if ($0 ~ /^[0-9]+ [0-9]+ [0-9]+ [0-9]+$/) print "yes" }')
    [ "$valid" = yes ] || { counts=unparsed; adapter_label="$adapter_label (invalid output)"; }
  fi
fi

# -- who is asserting, and the verdict program ------------------------

runner=$(fetch /api/whoami | jstr name)
[ -n "$runner" ] || die "cannot resolve the runner identity (GET /api/whoami)"

src=$(cat "$ROOT/gate-receipts/verdict.sabra")
src_json=$(printf '%s' "$src" | json_escape)
resp=$(post /api/programs "{\"source\":\"$src_json\"}") ||
  die "program upload of verdict.sabra failed"
program_hash=$(printf '%s' "$resp" | jstr hash)
[ -n "$program_hash" ] || die "program upload returned no hash: $resp"

# -- observation + skip-allowed as tree values ------------------------

obs_ternary=$(
  G_GATE="$gate" \
  G_PROJECT="$project" \
  G_COMMAND="$command_str" \
  G_REV="$rev" \
  G_TREE_CLEAN="$tree_clean" \
  G_DIFF_HASH="$diff_hash" \
  G_EXIT="$exit_status" \
  G_LOG_HASH="$log_hash" \
  G_COUNTS="$counts" \
  G_ADAPTER="$adapter_label" \
  G_RUNNER="$runner" \
  G_STARTED_AT="$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  bash "$ROOT/gate-receipts/encode.sh" observation
)
[ -n "$obs_ternary" ] || die "observation encoding produced nothing"

skip_env="${TUNA_GATE_SKIP_ALLOWED:-}"
skip_list=$(printf '%s' "$skip_env" | tr ',' '\n')
skip_ternary=$(G_SKIP="$skip_list" bash "$ROOT/gate-receipts/encode.sh" skip)
[ -n "$skip_ternary" ] || die "skip-allowed encoding produced nothing"

tree_sha256hex() {
  if command -v python3 >/dev/null 2>&1; then
    python3 -c '
import hashlib, sys
s = sys.stdin.read().strip()
out = bytearray()
for i in range(0, len(s), 5):
    c = s[i:i+5].ljust(5, "0")
    v = 0
    for ch in c: v = v * 3 + (ord(ch) - 48)
    out.append(v)
print(hashlib.sha256(bytes(out)).hexdigest())
'
  else
    LC_ALL=C awk '
      { all = all $0 }
      END {
        n = length(all)
        for (i = 1; i <= n; i += 5) {
          v = 0
          for (j = 0; j < 5; j++) {
            c = (i + j <= n) ? substr(all, i + j, 1) : "0"
            v = v * 3 + (c + 0)
          }
          printf "%c", v
        }
      }' | sha256hex
  fi
}

obs_hash=$(put_tree "$obs_ternary")
skip_hash=$(put_tree "$skip_ternary")
[ "$obs_hash" = "$(printf '%s' "$obs_ternary" | tree_sha256hex)" ] ||
  die "observation value hash disagrees with its ternary"
[ "$skip_hash" = "$(printf '%s' "$skip_ternary" | tree_sha256hex)" ] ||
  die "skip-allowed value hash disagrees with its ternary"

# -- the verdict run (prim-free, pinned caps) -------------------------

fuel="${TUNA_GATE_FUEL:-10000000}"
size_cap="${TUNA_GATE_SIZE_CAP:-1000000}"
resp=$(post /api/runs "{\"program_hash\":\"$program_hash\",\"inputs\":[\"$obs_ternary\",\"$skip_ternary\"],\"fuel\":$fuel,\"size_cap\":$size_cap}") ||
  die "POST /api/runs of the verdict failed"
run_id=$(printf '%s' "$resp" | sed -n 's/.*"run":{"id":"\([^"]*\)".*/\1/p')
result_ternary=$(printf '%s' "$resp" | jstr result_ternary)
result_hash=$(printf '%s' "$resp" | jstr result_hash)
steps=$(printf '%s' "$resp" | jint step_count)
run_status=$(printf '%s' "$resp" | jstr status)
[ -n "$run_id" ] || die "verdict run response carries no run id: $resp"
[ "$run_status" = normal ] || die "verdict run finished $run_status, nothing to cite"

case "$result_ternary" in
  10)   verdict=pass ;;
  110)  verdict=fail ;;
  1110) verdict=unverified ;;
  *)    die "verdict returned an unexpected tree '$result_ternary'" ;;
esac

# -- mint --------------------------------------------------------------

sanitize() { printf '%s' "$1" | sed 's/[^A-Za-z0-9_-]/-/g'; }
date_utc=$(date -u +%Y-%m-%d)
rev12=$(printf '%s' "$rev" | cut -c1-12)
base="u.$(sanitize "$runner").claim.$date_utc.gate.$(sanitize "$project").$rev12.$(sanitize "$gate")"
mint=$(TUNA_BASE="$BASE" TUNA_TOKEN="$TOKEN" bash "$ROOT/claims.sh" mint "$base" "$run_id") ||
  die "claims.sh mint failed"
claim=$(printf '%s\n' "$mint" | sed -n 's/^audit=//p')
citation=$(printf '%s\n' "$mint" | sed -n 's/^projection=//p')
[ -n "$claim" ] || die "claims.sh mint printed no claim path"

# -- the receipt -------------------------------------------------------

echo "gate-receipt: gate=$gate project=$project"
echo "gate-receipt: rev=$rev tree_clean=$tree_clean"
[ "$tree_clean" = 1 ] || echo "gate-receipt: diff_hash=$diff_hash"
echo "gate-receipt: command=$command_str"
echo "gate-receipt: exit=$exit_status counts=$counts adapter=$adapter_label"
echo "gate-receipt: log_hash=$log_hash"
if [ -n "$keep_log" ]; then echo "gate-receipt: log=$log"; fi
echo "gate-receipt: observation=$obs_hash skip_allowed=$skip_hash"
echo "gate-receipt: run=$run_id status=$run_status steps=$steps result=$result_hash"
echo "gate-receipt: verdict=$verdict"
echo "gate-receipt: claim=$claim"
echo "gate-receipt: citation=$citation"
