#!/usr/local/bin/env bash
# claims.sh - mint and verify a claim against a running tuna instance.
#
# A claim is a tree path (the assertion) plus the value at that path (the
# citation).  The citation is line-oriented text, readable as-is and parseable
# without a JSON library:
#
#   tuna-claim-citation-v1
#   assertion=<the path's assertion slug>
#   program=<sha256 of the program ternary>
#   inputs=<sha256,...>      hashes of the input ternaries (empty is legal)
#   fuel=<n>
#   size_cap=<n>
#   semantics=<v0|v1>
#   result=<sha256 of the result ternary>
#   steps=<n>
#   status=<normal|fuel_exhausted|size_exhausted|error|deadline_exceeded>
#   claimant=<identity id>
#   run=<run id>             metadata: answerable to its owner and admins only
#   minted_at=<UTC timestamp>
#
# VERIFIABLE.  A claim is verifiable when its citation is self-contained over
# content-addressed, world-readable artifacts.  GET /api/programs/:hash and
# GET /api/value/:hash?kind=tree check authentication and nothing else, so any
# identity can resolve the cited program and inputs, and POST /api/runs
# re-executes a prim-free computation with no grants.  Verification is
# therefore: resolve the citation, re-execute under the same caps, compare
# result hash and step count.  The claimant's run row is never needed - it
# stays caller-scoped.
#
# PUBLISHED.  The claim path is gated by the tree-op floor, but the citation
# text is content-addressed and readable by hash.  Naming that hash in the
# path (a %h in the path becomes citation-<hash12>) publishes the citation to
# any peer with no grant over the claimant's garden.
#
# LIMIT.  A computation that calls host prims is NOT re-executable from the
# citation alone: replay answers prim calls from the run's journal, and
# journal reads are caller-scoped.  Such a claim is verifiable only by its
# owner, an admin, or a verifier handed the journal some other way.
#
# Usage:
#   TUNA_TOKEN=<bearer> claims.sh mint    <base> <run-id> [--narrative F|--narrative-hash H]
#                                         [--target CLAIM-PATH]
#   TUNA_TOKEN=<bearer> claims.sh cite    <claim-path | value-hash>
#   TUNA_TOKEN=<bearer> claims.sh verify  <claim-path | value-hash>
#   TUNA_TOKEN=<bearer> claims.sh verdict <claim-path> <confirmed|stale|refuted> [note]
#   TUNA_TOKEN=<bearer> claims.sh list    [glob]
#
# Env: TUNA_BASE (default http://127.0.0.1:18090), TUNA_TOKEN (required).
# Needs curl, awk, sed and a sha256 tool; a production box ships no jq/python3.

set -eu

BASE="${TUNA_BASE:-http://127.0.0.1:18090}"
TOKEN="${TUNA_TOKEN:-}"
[ -n "$TOKEN" ] || { echo "claims.sh: set TUNA_TOKEN to a bearer" >&2; exit 2; }
AUTH="Authorization: Bearer $TOKEN"
CURL="curl -s -m 300"
command -v curl >/dev/null 2>&1 || { echo "claims.sh: curl not found" >&2; exit 2; }

say() { echo "claims.sh: $*" >&2; }
die() { say "$*"; exit 2; }

sha256hex() {
  if   command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1
  elif command -v sha256    >/dev/null 2>&1; then sha256 -q
  elif command -v openssl   >/dev/null 2>&1; then openssl sha256 | sed 's/.*= *//'
  else die "no sha256 tool on PATH"; fi
}

# --- the Cstr string codec (common/lib/cstr.ml, the book's marshal convention):
# leaf=0, stem=1<child>, fork=2<left><right>; a byte is a little-endian bit list
# (LSB outermost, 1 = stem, 0 = leaf); a string is a right-nested list of bytes.

cstr_encode() { LC_ALL=C awk '
  BEGIN { for (i=0;i<256;i++) ORD[sprintf("%c",i)] = i }
  function ctree(v,   arr,n,j,out) {
    n = 0
    while (v > 0) { arr[n++] = (v%2==1 ? "10" : "0"); v = int(v/2) }
    out = "0"
    for (j = n-1; j >= 0; j--) out = "2" arr[j] out
    return out }
  { all = all $0 "\n" }
  END { for (i=1;i<=length(all);i++) printf "2%s", ctree(ORD[substr(all,i,1)])
        printf "0" }'; }

cstr_decode() { LC_ALL=C awk '
  function nxt() { return substr(T, i++, 1) }
  function ctree(   c,b,v) {
    c = nxt()
    if (c == "0") return 0
    if (c != "2") { bad = 1; return 0 }
    b = nxt()
    if (b == "0") v = 0
    else if (b == "1") { if (nxt() != "0") { bad = 1; return 0 }; v = 1 }
    else { bad = 1; return 0 }
    return v + 2*ctree() }
  { T = T $0 }
  END { i = 1; out = ""
    while (i <= length(T)) {
      c = nxt()
      if (c == "0") break
      if (c != "2") { bad = 1; break }
      v = ctree()
      if (bad || v > 255) { bad = 1; break }
      out = out sprintf("%c", v) }
    if (bad) { print "claims.sh: not a Cstr string tree" > "/dev/stderr"; exit 3 }
    printf "%s", out }'; }

# --- server records are flat and every key is unique, so a greedy key match
# is enough: no JSON library on a production box.  Each helper takes the key
# and reads the JSON record on stdin.
jstr()  { sed -n "s/.*\"$1\":\"\([^\"]*\)\".*/\1/p" | head -n 1; }
jint()  { sed -n "s/.*\"$1\":\([0-9]*\).*/\1/p"          | head -n 1; }
jlist() { sed -n "s/.*\"$1\":\[\([^]]*\)\].*/\1/p" | tr -d '"' | tr ',' '\n' | sed '/^$/d'; }

fetch()    { $CURL -H "$AUTH" "$BASE$1"; }
post()     { $CURL -H "$AUTH" -H 'Content-Type: application/json' --data-binary "$2" "$BASE$1"; }
# base64 for publishing byte artifacts (OpenBSD ships openssl, not base64)
b64() {
  if   command -v base64  >/dev/null 2>&1; then base64 | tr -d '\n'
  elif command -v openssl >/dev/null 2>&1; then openssl base64 -A
  else die "no base64 tool on PATH"; fi
}
putbytes() {   # payload on stdin -> content-addressed byte-value hash on stdout
  local b
  b=$(b64) || die "base64 of the payload failed"
  post /api/value/put "{\"bytes_b64\":\"$b\"}" | jstr hash
}
postcode() { $CURL -o /dev/null -w '%{http_code}' -H "$AUTH" -H 'Content-Type: application/json' --data-binary "$2" "$BASE$1"; }
getcode()  { $CURL -o /dev/null -w '%{http_code}' -H "$AUTH" "$BASE$1"; }

# value hash at an in-tree path, read through the entry (so the floor applies)
path_value_hash() { post /api/tree/get "{\"path\":\"$1\"}" | jstr value_hash; }

# citation text for a claim path or a citation hash
citation_text() {
  local arg="$1" hash ternary
  case "$arg" in
    *[!0-9a-f]*) hash=$(path_value_hash "$arg" || true) ;;
    *)           hash="$arg" ;;
  esac
  [ -n "${hash:-}" ] || die "cannot read $arg (missing, or no covering grant for it)"
  ternary=$(fetch "/api/value/$hash?kind=tree" || true)
  [ -n "$ternary" ] || die "value $hash is not fetchable"
  printf '%s' "$ternary" | cstr_decode
}

cmd_cite() { citation_text "$1"; echo; }

cmd_mint() {
  local path="$1" runid="$2" resp ph res st fuel cap sem status caller inputs h
  local enc hash p code back
  resp=$(fetch "/api/runs/$runid" || true)
  [ -n "$resp" ] || die "cannot fetch run $runid"
  ph=$(printf '%s' "$resp" | jstr program_hash)
  res=$(printf '%s' "$resp" | jstr result_hash)
  st=$(printf '%s' "$resp" | jint step_count)
  fuel=$(printf '%s' "$resp" | jint fuel)
  cap=$(printf '%s' "$resp" | jint size_cap)
  sem=$(printf '%s' "$resp" | jstr semantics)
  status=$(printf '%s' "$resp" | jstr status)
  caller=$(printf '%s' "$resp" | jstr caller)
  inputs=$(printf '%s' "$resp" | jlist input_hashes)
  [ -n "$ph" ] || die "run $runid carries no program_hash (unknown run?)"
  [ -n "$status" ] || die "run $runid carries no status"
  case "$status" in
    normal) [ -n "$res" ] || die "run $runid is normal but has no result_hash" ;;
    *)      die "run $runid finished $status - nothing to cite" ;;
  esac
  for h in $inputs; do
    [ "$(getcode "/api/value/$h?kind=tree")" = 200 ] ||
      die "input $h is not published as a value: POST /api/value/put {\"ternary\":...} first, or no peer can re-execute this citation"
  done
  # publish the run record and its journal as content-addressed artifacts, so
  # the computation a claim links to is inspectable by any peer (claim paths are
  # floor-gated; values are readable by hash by any authenticated identity)
  run_art=$(printf '%s' "$resp" | putbytes)
  jrn=$(fetch "/api/journals/$runid" || true)
  [ -n "$jrn" ] || jrn='{"journal":[]}'
  jrn_art=$(printf '%s' "$jrn" | putbytes)
  { [ -n "$run_art" ] && [ -n "$jrn_art" ]; } || die "could not publish the run record or its journal as values"
  enc=$( { printf 'tuna-claim-citation-v1\n'
          printf 'assertion=%s\n' "$(printf '%s' "$path" | sed 's|\.citation-%h$||; s|%h||g')"
          printf 'program=%s\n' "$ph"
          printf 'inputs=%s\n' "$(printf '%s' "$inputs" | tr '\n' ',' | sed 's/,$//')"
          printf 'run-record=%s\n' "$run_art"
          printf 'journal=%s\n' "$jrn_art"
          printf 'fuel=%s\n' "$fuel"
          printf 'size_cap=%s\n' "$cap"
          printf 'semantics=%s\n' "$sem"
          printf 'result=%s\n' "$res"
          printf 'steps=%s\n' "$st"
          printf 'status=%s\n' "$status"
          printf 'claimant=%s\n' "$caller"
          printf 'run=%s\n' "$runid"
          printf 'minted_at=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" ; } | cstr_encode )
  hash=$(printf '%s' "$enc" | sha256hex)
  [ "${#enc}" -le 14000 ] || die "citation encodes to ${#enc} ternary digits; the PUT surface 500s above ~15000, so keep the citation short (gregor's chunked-claim convention slices at 600 chars)"
  p=$(printf '%s' "$path" | sed "s/%h/${hash%??????????????????????????????????????????????}/")
  # write-once: a claim path is written once; a correction is a new claim or a
  # retraction verdict, never an in-place bump, because a bump silently orphans
  # the verdicts and citations that pinned the previous revision
  prev=$(post /api/tree/get "{\"path\":\"$p\"}" | jstr value_hash)
  if [ -n "$prev" ] && [ "$prev" != "5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9" ]; then
    die "$p already holds a claim ($prev); write-once: mint a new claim path, or a retraction verdict"
  fi
  case "$p" in
    *"${hash%??????????????????????????????????????????????}"*) : ;;
    *) say "warning: $p does not name the citation hash ${hash%??????????????????????????????????????????????} - a peer cannot find the citation from the path alone" ;;
  esac
  code=$(postcode /api/tree/put "{\"path\":\"$p\",\"value_ternary\":\"$enc\"}")
  [ "$code" = 201 ] || die "tree/put of $p answered $code"
  back=$(post /api/tree/get "{\"path\":\"$p\"}" | jstr value_hash)
  [ "$back" = "$hash" ] || die "stored value hash $back != locally computed $hash"
  echo "path=$p"
  echo "citation=$hash"
  printf '%s' "$enc" | cstr_decode
}

cmd_verify() {
  local cit ph res st status fuel cap sem inputs h ternary body resp
  local got_res got_st got_status
  cit=$(citation_text "$1" || true)
  [ -n "$cit" ] || die "cannot read a citation from $1"
  ph=$(printf '%s\n' "$cit" | sed -n 's/^program=//p')
  res=$(printf '%s\n' "$cit" | sed -n 's/^result=//p')
  st=$(printf '%s\n' "$cit" | sed -n 's/^steps=//p')
  status=$(printf '%s\n' "$cit" | sed -n 's/^status=//p')
  fuel=$(printf '%s\n' "$cit" | sed -n 's/^fuel=//p')
  cap=$(printf '%s\n' "$cit" | sed -n 's/^size_cap=//p')
  sem=$(printf '%s\n' "$cit" | sed -n 's/^semantics=//p')
  inputs=$(printf '%s\n' "$cit" | sed -n 's/^inputs=//p' | tr ',' '\n' | sed '/^$/d')
  { [ -n "$ph" ] && [ -n "$res" ] && [ -n "$st" ]; } || die "citation lacks program=, result= or steps="
  [ "$(getcode "/api/programs/$ph")" = 200 ] || die "cited program $ph is not published"
  body=""
  for h in $inputs; do
    ternary=$(fetch "/api/value/$h?kind=tree" || true)
    [ -n "$ternary" ] || die "cited input $h is not published"
    if [ -z "$body" ]; then body="\"$ternary\""; else body="$body,\"$ternary\""; fi
  done
  resp=$(post /api/runs "{\"program_hash\":\"$ph\",\"inputs\":[$body],\"fuel\":$fuel,\"size_cap\":$cap,\"semantics\":\"$sem\"}" || true)
  [ -n "$resp" ] || die "the re-run was rejected"
  got_res=$(printf '%s' "$resp" | jstr result_hash)
  got_st=$(printf '%s' "$resp" | jint step_count)
  got_status=$(printf '%s' "$resp" | jstr status)
  rr=$(printf '%s\n' "$cit" | sed -n 's/^run-record=//p')
  if [ -n "$rr" ]; then
    rec=$(fetch "/api/value/$rr?kind=bytes" || true)
    rp=$(printf '%s' "$rec" | jstr program_hash)
    rres=$(printf '%s' "$rec" | jstr result_hash)
    rst=$(printf '%s' "$rec" | jint step_count)
    rstat=$(printf '%s' "$rec" | jstr status)
    echo "record  program=$rp result=$rres steps=$rst status=$rstat"
    if [ "$rp" != "$ph" ] || [ "$rres" != "$res" ] || [ "$rst" != "$st" ] || [ "$rstat" != "$status" ]; then
      echo "MISMATCH the projection disagrees with the run record it names"
      return 1
    fi
  fi
  echo "claim   result=$res steps=$st status=$status"
  echo "re-run  result=$got_res steps=$got_st status=$got_status"
  if [ "$got_res" = "$res" ] && [ "$got_st" = "$st" ] && [ "$got_status" = "$status" ]; then
    echo "VERIFIED result hash, step count and status agree"
    return 0
  fi
  echo "MISMATCH the cited computation does not reproduce"
  return 1
}

# --- the split mint: an audit projection, plus optional provenance ----------
#
# A claim is two content-addressed values under one base path:
#
#   <base>.audit-<p12>   the projection: only DERIVED facts, each one checkable
#                        against the run record named inside it
#   <base>.note-<n12>    the narrative: authored prose, named BY the projection
#                        so a reader reaches it from the audit side
#
# <base> must follow the agreed grammar
#   u.<identity>.claim.<YYYY-MM-DD>.<kind>.<subject>.<assertion-slug>
# The kind is read out of the path and checked against the frozen vocabulary, so
# the path and the projection cannot drift apart.  Kinds a human has to read
# (interpreted, asserted) require a narrative; the other kinds may omit it.
#
# The narrative is a Cstr string tree, the house convention for prose, and it is
# the one part of a claim that is authored rather than derived: context, never
# evidence, and nothing in the projection depends on its content.

# Rev observability: every projection records server_rev=<rev> fetched
# from /health, so an observation claim names the server rev it observed
# and can go stale against a rev instead of silently.  --target <path>
# adds target=<path> for verdict claims (kind=verdict) that judge a
# specific earlier claim.  A verdict correction can also be minted with
# `claims.sh verdict` (a small standalone record beside its target).
CLAIM_KINDS=' observed computed cited interpreted asserted axiom verdict '
LEAF_HASH=5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9

seg() { printf '%s' "$1" | cut -d. -f"$2"; }

claim_base_check() {
  local base="$1" dots
  dots=$(printf '%s' "$base" | tr -cd '.' | wc -c | tr -d ' ')
  [ "$dots" = 6 ] || die "claim path needs 7 dot segments (u.<identity>.claim.<date>.<kind>.<subject>.<slug>), got $((dots+1)): $base"
  [ "$(seg "$base" 1)" = u ] || die "claim path must start u.<identity>: $base"
  [ "$(seg "$base" 3)" = claim ] || die "claim path must carry the claim segment: $base"
  case "$(seg "$base" 4)" in
    [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]) : ;;
    *) die "claim date segment must be YYYY-MM-DD: $base" ;;
  esac
  KIND=$(seg "$base" 5)
  SUBJECT=$(seg "$base" 6)
  ASSERTION=$(seg "$base" 7)
  case "$CLAIM_KINDS" in *" $KIND "*) : ;; *) die "unknown claim kind '$KIND'; vocabulary:$CLAIM_KINDS" ;; esac
  case "$KIND" in
    interpreted|asserted) [ -n "$NARR_HASH" ] || die "kind $KIND needs a narrative (--narrative FILE or --narrative-hash HASH): prose is context, not evidence, so it cannot stand in for one" ;;
  esac
}

claim_write() {   # $1 path, $2 wanted value hash, $3 ternary (idempotent write-once)
  local path="$1" want="$2" tern="$3" prev code back
  prev=$(post /api/tree/get "{\"path\":\"$path\"}" | jstr value_hash)
  if [ -n "$prev" ] && [ "$prev" != "$want" ] && [ "$prev" != "$LEAF_HASH" ]; then
    die "$path already holds a different claim ($prev); write-once: mint a new claim path, or a retraction verdict"
  fi
  code=$(postcode /api/tree/put "{\"path\":\"$path\",\"value_ternary\":\"$tern\"}")
  [ "$code" = 201 ] || die "tree/put of $path answered $code"
  back=$(post /api/tree/get "{\"path\":\"$path\"}" | jstr value_hash)
  [ "$back" = "$want" ] || die "stored value hash $back != expected $want at $path"
}

cmd_mint() {
  local base="" runid="" target="" resp ph res st fuel cap sem status caller inputs h jrn
  local enc hash run_art jrn_art narr_enc p12 n12 rev
  NARR_HASH=''

  [ $# -ge 2 ] || die "usage: claims.sh mint <claim-base-path> <run-id> [--narrative FILE|--narrative-hash HASH] [--target PATH]"
  base="$1"
  runid="$2"
  shift 2
  while [ $# -gt 0 ]; do
    case "$1" in
        --narrative)      [ $# -ge 2 ] || die "--narrative needs a file"; narr_file="$2"; shift 2 ;;
        --narrative-hash) [ $# -ge 2 ] || die "--narrative-hash needs a hash"; NARR_HASH="$2"; shift 2 ;;
        --target)         [ $# -ge 2 ] || die "--target needs a claim path"; target="$2"; shift 2 ;;
        *) die "unknown option: $1" ;;
    esac
  done

  resp=$(fetch "/api/runs/$runid" || true)
  [ -n "$resp" ] || die "cannot fetch run $runid"
  ph=$(printf '%s' "$resp" | jstr program_hash)
  res=$(printf '%s' "$resp" | jstr result_hash)
  st=$(printf '%s' "$resp" | jint step_count)
  fuel=$(printf '%s' "$resp" | jint fuel)
  cap=$(printf '%s' "$resp" | jint size_cap)
  sem=$(printf '%s' "$resp" | jstr semantics)
  status=$(printf '%s' "$resp" | jstr status)
  caller=$(printf '%s' "$resp" | jstr caller)
  inputs=$(printf '%s' "$resp" | jlist input_hashes)
  [ -n "$ph" ] || die "run $runid carries no program_hash (unknown run?)"
  [ "$status" = normal ] || die "run $runid finished $status; only a normal run can be cited"
  [ -n "$res" ] || die "run $runid is normal but carries no result_hash"
  for h in $inputs; do
    [ "$(getcode "/api/value/$h?kind=tree")" = 200 ] ||
      die "input $h is not published as a value: POST /api/value/put {\"ternary\":...} first"
  done
    # rev observability: the projection names the server rev it observed
    rev=$(fetch /health | jstr rev)
    [ -n "$rev" ] || rev=unknown

  # the cited computation becomes a public artifact
  run_art=$(printf '%s' "$resp" | putbytes)
  jrn=$(fetch "/api/journals/$runid" || true)
  [ -n "$jrn" ] || jrn='{"journal":[]}'
  jrn_art=$(printf '%s' "$jrn" | putbytes)
  { [ -n "$run_art" ] && [ -n "$jrn_art" ]; } || die "could not publish the run record or its journal"

  # provenance, resolved before the projection so the projection can name it
  if [ -n "${narr_file:-}" ]; then
    [ -f "$narr_file" ] || die "no such narrative file: $narr_file"
    narr_enc=$(cstr_encode < "$narr_file")
    [ "${#narr_enc}" -le 14000 ] || die "narrative encodes to ${#narr_enc} ternary digits; the PUT surface 500s above ~15000, so split it (gregor's chunked-claim convention slices at 600 chars)"
    NARR_HASH=$(printf '%s' "$narr_enc" | sha256hex)
  elif [ -n "$NARR_HASH" ]; then
    narr_enc=$(fetch "/api/value/$NARR_HASH?kind=tree" || true)
    [ -n "$narr_enc" ] || die "narrative $NARR_HASH is not published as a tree"
  fi

  claim_base_check "$base"

  proj() {
    printf 'tuna-claim-projection-v1\n'
    printf 'kind=%s\n' "$KIND"
    printf 'subject=%s\n' "$SUBJECT"
    printf 'assertion=%s\n' "$ASSERTION"
    printf 'server_rev=%s\n' "$rev"
    [ -n "$target" ] && printf 'target=%s\n' "$target"
    printf 'program=%s\n' "$ph"
    printf 'inputs=%s\n' "$(printf '%s' "$inputs" | tr '\n' ',' | sed 's/,$//')"
    printf 'run-record=%s\n' "$run_art"
    printf 'journal=%s\n' "$jrn_art"
    [ -n "$NARR_HASH" ] && printf 'narrative=%s\n' "$NARR_HASH"
    printf 'fuel=%s\n' "$fuel"
    printf 'size_cap=%s\n' "$cap"
    printf 'semantics=%s\n' "$sem"
    printf 'result=%s\n' "$res"
    printf 'steps=%s\n' "$st"
    printf 'status=%s\n' "$status"
    printf 'claimant=%s\n' "$caller"
    printf 'run=%s\n' "$runid"
    printf 'minted_at=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  }
  enc=$(proj | cstr_encode)
  [ "${#enc}" -le 14000 ] || die "projection encodes to ${#enc} ternary digits; the PUT surface 500s above ~15000"
  hash=$(printf '%s' "$enc" | sha256hex)
  p12=${hash%${hash#????????????}}

  # note first, so a peer never finds a projection whose prose is missing
  if [ -n "$NARR_HASH" ]; then
    n12=${NARR_HASH%${NARR_HASH#????????????}}
    claim_write "$base.note-$n12" "$NARR_HASH" "$narr_enc"
  fi
  claim_write "$base.audit-$p12" "$hash" "$enc"

  echo "audit=$base.audit-$p12"
  echo "projection=$hash"
  [ -n "$NARR_HASH" ] && echo "note=$base.note-$n12"
  printf '%s' "$enc" | cstr_decode
}
# --- the verdict: a claim's correction lives beside it ----------------------
#
# A claim path is write-once, so a correction is a new claim or a verdict.
# A verdict is a small record stored at <target>.verdict-<hash12> — the
# same write-once discipline, listed by the registry census, gated by the
# floor: you need a covering grant over the target's prefix, so you speak
# about a garden from inside it (or hold adjudicator grants).
# Word choice: confirmed | stale | refuted.

cmd_verdict() {
  local target="$1" word="$2" note="${3:-}" rev p hash enc h12
  case "$word" in
    confirmed|stale|refuted) : ;;
    *) die "verdict must be confirmed, stale or refuted (got '$word')" ;;
  esac
  rev=$(fetch /health | jstr rev)
  [ -n "$rev" ] || rev=unknown
  enc=$( { printf 'tuna-claim-verdict-v1\n'
           printf 'target=%s\n' "$target"
           printf 'verdict=%s\n' "$word"
           printf 'server_rev=%s\n' "$rev"
           printf 'note=%s\n' "$note"
           printf 'minted_at=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" ; } | cstr_encode )
  [ "${#enc}" -le 14000 ] || die "verdict encodes to ${#enc} ternary digits; the PUT surface 500s above ~15000"
  hash=$(printf '%s' "$enc" | sha256hex)
  h12=${hash%${hash#????????????}}
  p="$target.verdict-$h12"
  claim_write "$p" "$hash" "$enc"
  echo "verdict=$p"
  printf '%s' "$enc" | cstr_decode
}

# discovery: the registry page (GET /claims, open) is the feed; list pulls
# the paths out of it and matches each against an optional shell glob.
cmd_list() {
  local pat="${1:-*}" p
  $CURL "$BASE/claims" \
    | sed -n 's#^<tr><td><code>\([^<]*\)</code></td><td>.*#\1#p' \
    | while read -r p; do
        [ -n "$p" ] || continue
        case "$p" in
          $pat) printf '%s\n' "$p" ;;
        esac
      done
}

case "${1:-}" in
  mint)    shift; [ $# -ge 2 ] || die "usage: claims.sh mint <claim-base-path> <run-id> [--narrative FILE|--narrative-hash HASH] [--target PATH]"; cmd_mint "$@" ;;
  cite)    shift; [ $# -eq 1 ] || die "usage: claims.sh cite <claim-path|value-hash>";   cmd_cite "$1" ;;
  verify)  shift; [ $# -eq 1 ] || die "usage: claims.sh verify <claim-path|value-hash>"; cmd_verify "$1" ;;
  verdict) shift; [ $# -ge 2 ] || die "usage: claims.sh verdict <claim-path> <confirmed|stale|refuted> [note]"; cmd_verdict "$@" ;;
  list)    shift; cmd_list "$@" ;;
  *) echo "usage: claims.sh mint|cite|verify|verdict|list ..." >&2; exit 2 ;;
esac
