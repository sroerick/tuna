#!/bin/bash
# acceptance 16 (borg/eval-modes.borg): staged templates, live.
#   16.1 THE RECORD IS A RUN - tmpl-compile (stdlib v1.3) run over a
#        template string through POST /api/tool/run answers the
#        specialised renderer as a run row that replay-verifies.
#   16.2 COMPILE == RUN - the specialised renderer's hash equals the
#        program hash of the source (tmpl-compile "<template>"): the
#        compile-time specialisation and the recorded run are one tree.
#   16.3 THE SPECIALISED TREE IS A TOOL - called through POST
#        /api/tool/run with a JSON record it answers the rendered,
#        escaped string.
#   16.4 E2 FAST1 - the same tool call under semantics "fast1" answers
#        the same string in fewer steps with native calls, replay-
#        verifies under its own law, and re-run under v0 (tier 2: the
#        reference re-verification) gives the identical result hash.
#        An unknown semantics is refused.
#   16.5 E2 FAST2 - records, the higher-order list family and
#        compile-time partials: the same call answers the same string in
#        fewer steps than fast1, and replay-verifies under fast2.
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

item = "<li>{2} <small>by {3}</small></li>"
todo = {"record": {"title": [2, "string"], "who": [3, "string"]}}

print("[16.1] the record is a run")
code, j = call("POST", "/api/programs", {"source": "tmpl-compile"})
if code != 201:
    die("16.1 tmpl-compile must resolve from the stdlib: %s %r" % (code, j))
tc = j["hash"]
code, j = call("POST", "/api/tool/run", {
    "program_hash": tc, "inputs": [{"type": "string", "value": item}],
    "result_type": "tree", "fuel": 50000000, "size_cap": 10000000})
if code != 201 or j["run"]["status"] != "normal":
    die("16.1 tmpl-compile over the template must finish: %s %r" % (code, j.get("error", j.get("run"))))
run = j["run"]
code, full = call("GET", "/api/runs/" + run["id"])
if full["run"].get("verify_status") != "verified":
    die("16.1 the specialisation run must replay-verify: %r" % full["run"].get("verify_status"))
print("  tmpl-compile %s over the template -> %s (%d steps, replay verified)"
      % (tc[:12], run["result_hash"][:12], run["step_count"]))

print("[16.2] compile-time specialisation == the recorded run")
code, j = call("POST", "/api/programs", {"source": "(tmpl-compile %s)" % json.dumps(item)})
if code != 201 or j["hash"] != run["result_hash"]:
    die("16.2 (tmpl-compile \"...\") must compile to the run's result: %s %r vs %s"
        % (code, j, run["result_hash"]))
spec = j["hash"]
print("  /api/programs (tmpl-compile \"...\") = %s = run result" % spec[:12])

print("[16.3] the specialised renderer as a JSON tool")
code, j = call("POST", "/api/tool/run", {
    "program_hash": spec,
    "inputs": [{"type": todo, "value": {"title": "ship <E1> & test", "who": "o'neil"}}],
    "result_type": "string", "fuel": 50000000, "size_cap": 10000000})
want = "<li>ship &lt;E1&gt; &amp; test <small>by o&#39;neil</small></li>"
if code != 201 or j.get("result") != want:
    die("16.3 want %r: %s %r" % (want, code, {k: j.get(k) for k in ("result", "result_error", "error")}))
print("  %r (%d steps)" % (j["result"], j["run"]["step_count"]))
v0_run = j["run"]

print("[16.4] E2 fast1: accelerators, replay, v0 re-verification")
def tool(sem):
    return call("POST", "/api/tool/run", {
        "program_hash": spec, "semantics": sem,
        "inputs": [{"type": todo, "value": {"title": "ship <E1> & test", "who": "o'neil"}}],
        "result_type": "string", "fuel": 50000000, "size_cap": 10000000})
code, j = tool("fast1")
if code != 201 or j.get("result") != want or j["run"]["semantics"] != "fast1":
    die("16.4 fast1 must answer %r: %s %r" % (want, code, {k: j.get(k) for k in ("result", "error")}))
fast = j["run"]
if not fast["step_count"] < v0_run["step_count"]:
    die("16.4 fast1 must take fewer steps: %d vs v0 %d" % (fast["step_count"], v0_run["step_count"]))
code, full = call("GET", "/api/runs/" + fast["id"])
if full["run"].get("verify_status") != "verified":
    die("16.4 a fast1 run must replay-verify under its own law: %r" % full["run"].get("verify_status"))
code, j = tool("v0")
if j["run"]["result_hash"] != fast["result_hash"]:
    die("16.4 v0 re-verification must give the fast1 result hash: %s vs %s"
        % (j["run"]["result_hash"], fast["result_hash"]))
print("  fast1 %d steps vs v0 %d, replay verified, v0 re-run result hash identical (%s)"
      % (fast["step_count"], v0_run["step_count"], fast["result_hash"][:12]))
code, j = tool("fast9")
if code != 400:
    die("16.4 an unknown semantics must be a 400: %s %r" % (code, j))
print("  unknown semantics fast9 -> 400")

print("[16.5] E2 fast2: records, higher-order lists, compile-time partials")
code, j = tool("fast2")
if code != 201 or j.get("result") != want or j["run"]["semantics"] != "fast2":
    die("16.5 fast2 must answer %r: %s %r" % (want, code, {k: j.get(k) for k in ("result", "error")}))
f2 = j["run"]
if not f2["step_count"] < fast["step_count"]:
    die("16.5 fast2 must take fewer steps than fast1: %d vs %d" % (f2["step_count"], fast["step_count"]))
code, full = call("GET", "/api/runs/" + f2["id"])
if full["run"].get("verify_status") != "verified" or f2["result_hash"] != fast["result_hash"]:
    die("16.5 fast2 must replay-verify with the same result: %r %s"
        % (full["run"].get("verify_status"), f2["result_hash"]))
print("  fast2 %d steps (fast1 %d, v0 %d), replay verified, same result hash"
      % (f2["step_count"], fast["step_count"], v0_run["step_count"]))
PY

echo "ACCEPT 16 OK: a staged template is a recorded, replayable run and a callable tool; fast1 and fast2 agree with v0"
