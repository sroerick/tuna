#!/bin/bash
# local chapter L1 (borg/local.borg): the CLI binary itself resolves std
# names offline, prints the lock, and eval-source runs program + arg
# terms.  L3: tuna run seals a record deriv-check verifies, and a
# tampered record exits 1.  L4: --in/--out take the tool-language T4
# type grammar, and the gate verdict through the CLI with a JSON
# observation answers the same JSON as POST /api/tool/run.
# Usage: cli_local.sh <cli/bin/main.exe> <deriv_check.exe> <verdict.sabra>
set -u
CLI=$1
DC=$2
VERDICT=$3
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
fail() { echo "FAIL cli_local: $*" >&2; exit 1; }
LF=3a4225df7bd383bc2f1a6b3d0fa0a0bb84632f90e1415825a2be683503fabc4f

echo '(not true)' > "$T/a.sabra"
out=$("$CLI" compile "$T/a.sabra") || fail "(not true) did not compile"
grep -qx 'ternary 0' <<<"$out" || fail "(not true) is not the leaf: $out"
grep -qx 'steps 2' <<<"$out" || fail "(not true) not 2 steps: $out"
grep -qx 'lock not 1f6cae19aff40a81b0f8fcf17ea936269fcd2eaf27bf85148dbd8750a61ce320' <<<"$out" || fail "lock not missing: $out"
grep -qx 'lock true 4a44dc15364204a80fe80e9039455cc1608281820fe2b24f1e5233ade6af1dd5' <<<"$out" || fail "lock true missing: $out"

echo 'list-fold@3a4225df' > "$T/p.sabra"
out=$("$CLI" compile "$T/p.sabra") || fail "holding pin refused"
grep -qx "hash $LF" <<<"$out" || fail "holding pin: wrong hash: $out"
echo 'list-fold@00000000' > "$T/q.sabra"
err=$("$CLI" compile "$T/q.sabra" 2>&1 >/dev/null); rc=$?
[ $rc -eq 1 ] || fail "moved pin exit $rc, want 1"
grep -q "expected 00000000, found $LF" <<<"$err" || fail "moved pin message: $err"

echo '(lambda (xs) (list-length xs))' > "$T/c.sabra"
out=$("$CLI" eval-source "$T/c.sabra" '[1 2 3]' --fuel 100000 --cap 100000) || fail "eval-source failed"
grep -qx 'normal 1110 517' <<<"$out" || fail "eval-source [1 2 3]: $out"
raw=$("$CLI" eval-source "$T/c.sabra" '%220210022020210022021021000' --fuel 100000 --cap 100000 | tail -1)
[ "$raw" = "$(tail -1 <<<"$out")" ] || fail "%ternary arg differs from source arg: $raw"
echo '(lambda (f) f)' > "$T/i.sabra"
out=$("$CLI" eval-source "$T/i.sabra" 'not') || fail "eval-source with std arg failed"
grep -q '^lock not ' <<<"$out" || fail "arg-term lock missing: $out"
echo '(lambda (x) [(runtime (prim "echo" x)) (runtime (prim "now"))])' > "$T/r.sabra"
out=$("$CLI" run "$T/r.sabra" '"k"' --dir "$T/local" --grant echo --grant now --fuel 100000 --cap 100000) \
  || fail "tuna run failed"
grep -q '^journal 0 echo ' <<<"$out" || fail "run journal: $out"
grep -q '^normal ' <<<"$out" || fail "run status: $out"
rec=$(sed -n 's/^record //p' <<<"$out")
[ -f "$rec" ] || fail "no sealed record at $rec"
"$DC" "$rec" >/dev/null || fail "deriv-check refused a local record"
sed 's/"prim":"now"/"prim":"uuid"/' "$rec" > "$T/bad.json"
"$DC" "$T/bad.json" >/dev/null 2>&1; rc=$?
[ $rc -eq 1 ] || fail "tampered record: deriv-check exit $rc, want 1"
out=$("$CLI" run "$T/r.sabra" '"k"' --dir "$T/local" --fuel 100000 --cap 100000) || fail "ungranted run"
grep -q 'error grant denial: no live grant for prim echo' <<<"$out" || fail "denial not journaled: $out"

# L3 in the repl: eval rounds answer prims, and --grant reaches the host
RD=$T/repl; mkdir -p "$RD"
printf '(lambda (x) (runtime (prim "store/put" "k" x)))\n' \
  | "$CLI" repl --dir "$RD" --input 42 --grant store/put --fuel 100000 --cap 100000 >/dev/null \
  || fail "repl with a granted prim failed"
[ -d "$RD/kv" ] && [ -n "$(ls -A "$RD/kv")" ] || fail "granted repl prim did not run"
# L4: typed I/O.  The observation type and the verdict result type are
# written with the same JSON grammar POST /api/tool/run takes, and the
# decoded answer is the JSON value that endpoint answers in "result".
OBS_TYPE='{"record": {"gate": [1, "string"], "project": [2, "string"],
  "command": [3, "string"], "rev": [4, "string"], "tree_clean": [5, "int"],
  "diff_hash": [6, "string"], "exit": [7, "int"], "log_hash": [8, "string"],
  "counts": [9, {"list": "int"}], "adapter": [10, "string"],
  "runner": [11, "string"], "started_at": [12, "string"]}}'
VERDICT_TYPE='{"enum": {"pass": 1, "fail": 2, "unverified": 3}}'
obs() {
  printf '{"gate": "tuna/runtest", "project": "tuna", "command": "make -C tuna test", "rev": "%s", "tree_clean": 1, "diff_hash": "", "exit": %d, "log_hash": "%s", "counts": %s, "adapter": "alcotest-v1", "runner": "verify-15", "started_at": "2026-11-25T00:00:00Z"}' \
    "$(printf 'a%.0s' $(seq 40))" "$1" "$(printf 'b%.0s' $(seq 64))" "$2"
}
for want in pass fail unverified; do
  case $want in
    pass) o=$(obs 0 '[93, 93, 0, 0]') ;;
    fail) o=$(obs 1 '[93, 93, 0, 0]') ;;
    unverified) o=$(obs 0 '[0, 0, 0, 0]') ;;
  esac
  out=$("$CLI" run "$VERDICT" --in "$OBS_TYPE" "$o" --in '{"list": "string"}' '[]' \
    --out "$VERDICT_TYPE" --dir "$T/l4" --fuel 50000000 --cap 1000000) \
    || fail "typed run ($want) failed"
  grep -qx "result \"$want\"" <<<"$out" \
    || fail "typed verdict ($want) must answer the server's JSON: $out"
done
# without --out the CLI keeps printing ternary: the verdict is one tag
out=$("$CLI" run "$VERDICT" --in "$OBS_TYPE" "$(obs 0 '[93, 93, 0, 0]')" \
  --in '{"list": "string"}' '[]' --dir "$T/l4" --fuel 50000000 --cap 1000000) \
  || fail "untyped run failed"
grep -qx 'normal 10 [0-9]*' <<<"$out" || fail "no --out must stay ternary: $out"
# a JSON value of the wrong type is refused with the flag and the path
err=$("$CLI" run "$VERDICT" --in "$OBS_TYPE" '{"exit": "zero"}' --in '{"list": "string"}' '[]' \
  --dir "$T/l4" 2>&1 >/dev/null); rc=$?
[ $rc -eq 2 ] || fail "mistyped --in exit $rc, want 2"
grep -q -- '--in: \$\.exit' <<<"$err" || fail "mistyped --in message: $err"

RD2=$T/repl2; mkdir -p "$RD2"
printf '(lambda (x) (runtime (prim "store/put" "k" x)))\n' \
  | "$CLI" repl --dir "$RD2" --input 42 --fuel 100000 --cap 100000 >/dev/null \
  || fail "repl with an ungranted prim failed"
[ ! -d "$RD2/kv" ] || fail "ungranted repl prim ran"
echo "cli_local: ok"
