#!/bin/bash
# acceptance 17 (borg/typing.borg TY3): typed inputs at the tool
# boundary, live.
#   17.1 A TYPED INT IS CHECKED AND RUNS - an input {"jay": "int"} is
#        encoded as TyBin's tagged Int, Jay's infer over O_bin types it
#        in a check run (semantics fast3) that the answer cites and that
#        replay-verifies; the tool run answers the int back through
#        result_type {"jay": "int"}.
#   17.2 A FORGED CLAIM IS REFUSED - a tree claimed to be a jay int that
#        is not one (a v1 int; a Bin; junk) is a 400 citing its check
#        run, which exists, answered a refusal, and replay-verifies.
#   17.3 JAY BOOL - true is typed and answered back.
#   17.5 NESTED - a jay type inside a list, option or record field is
#        checked at its own path (review finding 2026-11-25: it was not).
#   17.4 UNTYPED IS UNCHANGED - the same program with a plain "int"
#        input answers as before, with no type checks.
#
# Env: as scripts/verify-lib.sh (a live dev server; TUNA_HTTP_PORT,
# TUNA_SMOKE_TOKEN or the dev boot token).
set -uo pipefail
cd "$(dirname "$0")/.."
. scripts/verify-lib.sh
python3 - "$BASE" "$TOKEN" <<'PY' || fail "see the line above"
import json, sys, urllib.request, urllib.error
base, tok = sys.argv[1], sys.argv[2]
def call(method, path, body=None):
    req = urllib.request.Request(base + path, method=method,
        data=None if body is None else json.dumps(body).encode(),
        headers={"Authorization": "Bearer " + tok,
                 "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=300) as r:
            return r.status, json.load(r)
    except urllib.error.HTTPError as e:
        return e.code, json.load(e)

def die(msg):
    print("FAIL: " + msg); sys.exit(1)

JINT = {"jay": "int"}
JBOOL = {"jay": "bool"}

code, j = call("POST", "/api/programs", {"source": "(lambda (x) x)"})
if code != 201:
    die("identity program must compile: %s %r" % (code, j))
ident = j["hash"]

def tool(inputs, result_type, sem="v0"):
    return call("POST", "/api/tool/run", {
        "program_hash": ident, "inputs": inputs, "result_type": result_type,
        "semantics": sem, "fuel": 1000000, "size_cap": 1000000})

def verified(run_id):
    code, full = call("GET", "/api/runs/" + run_id)
    return code == 200 and full["run"].get("verify_status") == "verified", full["run"]

print("[17.1] a typed int is checked and runs")
n = 1048579
code, j = tool([{"type": JINT, "value": n}], JINT)
if code != 201 or j.get("result") != n:
    die("17.1 want %d back: %s %r" % (n, code, {k: j.get(k) for k in ("result", "result_error", "error")}))
checks = j.get("type_checks") or []
if len(checks) != 1 or checks[0]["input"] != "inputs[0]":
    die("17.1 the answer must cite one type check: %r" % checks)
ok, row = verified(checks[0]["check_run"])
if not ok or row.get("semantics") != "fast3":
    die("17.1 the check run must be a fast3 run that replay-verifies: %r" % {k: row.get(k) for k in ("semantics", "verify_status", "status")})
print("  %d -> %d; check run %s (%s steps, fast3, replay verified), checker %s"
      % (n, j["result"], checks[0]["check_run"][:8], checks[0]["check_steps"], checks[0]["checker"][:12]))

print("[17.2] a forged claim is refused with the check run cited")
code, j = call("POST", "/api/tool/run", {
    "program_hash": ident, "inputs": [{"type": "int", "value": 5}], "result_type": "tree",
    "fuel": 1000000, "size_cap": 1000000})
v1_five = j.get("result")
# the typed 5 as a raw tree (result_type "tree")
code, j = tool([{"type": JINT, "value": 5}], "tree")
jay_five = j.get("result")
# the Int tag's name int_nm = Fork(Leaf, Stem Leaf) ends the ternary as
# "2010"; renamed to Bin's name Fork(Leaf, Leaf) = "200" it is a
# well-formed tag the Bin guard refuses
if not (jay_five or "").endswith("2010"):
    die("17.2 the typed 5 must end with the Int name: %r" % (jay_five or "")[-8:])
claims = [("a v1 int", v1_five), ("junk", "21010"), ("a leaf", "0"),
          ("a jay int retagged as Bin", jay_five[:-4] + "200")]
for what, tern in claims:
    if tern is None:
        die("17.2 could not build the %s claim" % what)
    code, j = tool([{"type": JINT, "value": {"ternary": tern}}], JINT)
    tc = j.get("type_check") or {}
    if code != 400 or not tc.get("check_run"):
        die("17.2 %s claimed as jay int must be a 400 citing a check run: %s %r" % (what, code, j))
    ok, row = verified(tc["check_run"])
    if not ok:
        die("17.2 the refusing check run must replay-verify: %r" % row.get("verify_status"))
    print("  %-38s -> 400, check run %s (%s, result %s), replay verified"
          % (what, tc["check_run"][:8], j.get("check_status"), (j.get("check_result") or "")[:12]))

print("[17.5] a jay type nested in a list, option or record is checked too")
junk = {"ternary": "21010"}
nested = [
    ("a list element", {"list": JINT}, [5, junk], "inputs[0][1]"),
    ("an option", {"option": JINT}, junk, "inputs[0]"),
    ("a record field", {"record": {"amount": [0, JINT]}}, {"amount": junk}, "inputs[0].amount"),
]
for what, ty, val, path in nested:
    code, j = tool([{"type": ty, "value": val}], "tree")
    tc = j.get("type_check") or {}
    if code != 400 or tc.get("input") != path or not tc.get("check_run"):
        die("17.5 junk in %s must be a 400 citing %s: %s %r" % (what, path, code, j))
    print("  junk in %-15s -> 400 at %s, check run %s" % (what, path, tc["check_run"][:8]))
code, j = tool([{"type": {"list": JINT}, "value": [5, -6, 1048579]}], {"list": JINT})
if code != 201 or j.get("result") != [5, -6, 1048579] or len(j.get("type_checks") or []) != 3:
    die("17.5 a list of three jay ints must run with three checks: %s %r" % (code, j))
print("  [5, -6, 1048579] as a list of jay ints -> 201, three check runs")

print("[17.3] jay bool")
code, j = tool([{"type": JBOOL, "value": True}], JBOOL)
if code != 201 or j.get("result") is not True or len(j.get("type_checks") or []) != 1:
    die("17.3 want true back with one check: %s %r" % (code, j))
print("  true -> true, check run %s" % j["type_checks"][0]["check_run"][:8])

print("[17.4] untyped is unchanged")
code, j = tool([{"type": "int", "value": 5}], "int")
if code != 201 or j.get("result") != 5 or "type_checks" in j:
    die("17.4 a plain int input must answer 5 with no type checks: %s %r" % (code, j))
print("  plain int 5 -> 5, no type checks")
PY

echo "ACCEPT 17 OK: jay-typed tool inputs are checked by Jay's infer in a cited, replayable run; forged claims are refused; untyped runs unchanged"
