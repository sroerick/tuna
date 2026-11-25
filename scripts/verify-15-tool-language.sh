#!/bin/bash
# acceptance 15 (borg/tool-language.borg): the tool language, live.
#   15.1 T1 COMBINATORS - comb-i/s/b/c/w resolve from the shared stdlib
#        and their trees carry agora's published hashes.
#   15.2 T2 PINNED NAMES over POST /api/programs - stdlib names resolve
#        (the caller's effective dictionary), the response reports the
#        lock, a holding pin compiles to the unpinned hash, a moved pin
#        and a pinned lambda parameter are 400s.
#   15.3 THE VERDICT THREE WAYS - scripts/gate-receipts/verdict.sabra
#        (helpers inline) and scripts/dialect/verdict-cond.sabra (T3
#        sugar) compile to ONE program hash; scripts/dialect/verdict-std.sabra
#        (pinned stdlib names, no inline helpers) compiles over
#        /api/programs with every name it pins in its lock.  Its tree
#        differs (dictionary literals vs let-bound helpers are different
#        programs for one function); tests/tool_language_tests.ml holds
#        the extensional proof - the same answer on every policy case.
#   15.4 REPL LOCK - an eval round reports the names it resolved.
#
# Env: as scripts/verify-lib.sh (a live dev server; TUNA_HTTP_PORT,
# TUNA_SMOKE_TOKEN or the dev boot token).
set -uo pipefail
cd "$(dirname "$0")/.."
. scripts/verify-lib.sh

python3 - "$BASE" "$TOKEN" <<'PY' || fail "see the line above"
import json, sys, urllib.request, urllib.error
base, tok = sys.argv[1], sys.argv[2]

def post(path, body):
    req = urllib.request.Request(base + path, data=json.dumps(body).encode(),
        headers={"Authorization": "Bearer " + tok,
                 "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=300) as r:
            return r.status, json.load(r)
    except urllib.error.HTTPError as e:
        return e.code, json.load(e)

def program(src):
    return post("/api/programs", {"source": src})

def die(msg):
    print("FAIL: " + msg); sys.exit(1)

print("[15.1] T1 combinators carry agora's hashes")
agora = {"comb-i": "75bae64a", "comb-s": "6ab5ad37", "comb-b": "e4b1abc3",
         "comb-c": "a9d528d5", "comb-w": "a24e4471"}
for name, want in agora.items():
    code, j = program(name)
    if code != 201 or not j["hash"].startswith(want):
        die("15.1 %s: %s %r, want hash %s..." % (name, code, j, want))
print("  5/5 combinators: program hash == agora's published hash")

print("[15.2] T2 pinned names over /api/programs")
code, j = program("(lambda (x) (list-length x))")
if code != 201:
    die("15.2 a stdlib name must resolve over /api/programs: %s %r" % (code, j))
lock = {e["name"]: e["hash"] for e in j.get("lock", [])}
if "list-length" not in lock:
    die("15.2 the lock must name list-length: %r" % j)
unpinned = j["hash"]
code, j = program("(lambda (x) (list-length@%s x))" % lock["list-length"][:12])
if code != 201 or j["hash"] != unpinned:
    die("15.2 a holding pin must compile to the unpinned hash: %s %r" % (code, j))
moved = ("1" if lock["list-length"][0] == "0" else "0") + lock["list-length"][1:12]
code, j = program("(lambda (x) (list-length@%s x))" % moved)
err = j.get("error", "")
if code != 400 or moved not in err or lock["list-length"] not in err:
    die("15.2 a moved pin must be a 400 naming both hashes: %s %r" % (code, j))
code, j = program("(lambda (x) x@12345678)")
if code != 400 or "lambda parameter" not in j.get("error", ""):
    die("15.2 a pinned lambda parameter must be a 400: %s %r" % (code, j))
print("  resolve + lock + holding pin + moved pin + parameter pin: green")

print("[15.3] the verdict three ways: one hash for the sugar, pins hold for std")
hashes = {}
for path in ("scripts/gate-receipts/verdict.sabra",
             "scripts/dialect/verdict-cond.sabra",
             "scripts/dialect/verdict-std.sabra"):
    code, j = program(open(path).read())
    if code != 201:
        die("15.3 %s: %s %r" % (path, code, j))
    hashes[path] = j["hash"]
    print("  %-40s %s (lock: %d names)" % (path, j["hash"][:16], len(j.get("lock", []))))
orig = hashes["scripts/gate-receipts/verdict.sabra"]
if hashes["scripts/dialect/verdict-cond.sabra"] != orig:
    die("15.3 verdict-cond must compile to verdict.sabra's hash: %r" % hashes)
import re
src = open("scripts/dialect/verdict-std.sabra").read()
pins = dict(re.findall(r"([a-z][a-z0-9-]*)@([0-9a-f]{8,64})", src))
code, j = program(src)
lock = {e["name"]: e["hash"] for e in j.get("lock", [])}
for name, pin in pins.items():
    if not lock.get(name, "").startswith(pin):
        die("15.3 verdict-std pins %s@%s but the lock says %r" % (name, pin, lock.get(name)))
print("  verdict-cond == verdict; verdict-std's %d pins all hold in its lock" % len(pins))

print("[15.4] REPL eval reports its lock")
code, j = post("/api/repl", {"term": "(comb-s true true %200)"})
r = j.get("round", {})
names = sorted(e["name"] for e in r.get("lock", []))
if code != 200 or r.get("ternary") != "200" or names != ["comb-s", "true"]:
    die("15.4 REPL eval must answer 200 with lock [comb-s, true]: %s %r" % (code, j))
print("  (comb-s true true %200) -> 200, lock [comb-s, true]")
PY

echo "ACCEPT 15 OK: combinators shared, names pin and lock, the verdict reads three ways"
