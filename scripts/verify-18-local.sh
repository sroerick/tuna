#!/bin/bash
# acceptance 18 (borg/local.borg): local, offline front ends.
#   18.1 L1 STDLIB OFFLINE - every in-tree .sabra source and a set of
#        std-name probes compile through the CLI (no server, frozen
#        stdlib) to the SAME program hash POST /api/programs answers
#        for the caller, and the CLI lock equals the server lock.  The
#        caller must have no defs of its own (checked: the REPL dict
#        command answers no rows), so its effective dictionary is std.
#        Run against an instance seeded from stdlib/v1/manifest
#        (verify-8 checks that); a mismatch is LO1 drift.
#
# Env: as scripts/verify-lib.sh (a live dev server; TUNA_HTTP_PORT,
# TUNA_SMOKE_TOKEN or the dev boot token).
set -uo pipefail
cd "$(dirname "$0")/.."
. scripts/verify-lib.sh

dune build --root "$PWD" cli/bin/main.exe 2>&1 | head -20
CLI=_build/default/cli/bin/main.exe
[ -x "$CLI" ] || fail "cli did not build"

echo "[18.1] L1: CLI compile == POST /api/programs (frozen std vs seeded std)"
python3 - "$BASE" "$TOKEN" "$CLI" scripts/*/*.sabra <<'PY' || fail "see the line above"
import json, subprocess, sys, tempfile, urllib.request, urllib.error
base, tok, cli, files = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4:]

def api(path, body):
    req = urllib.request.Request(base + path, data=json.dumps(body).encode(),
        headers={"Authorization": "Bearer " + tok,
                 "Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=300) as r:
        return json.load(r)

own = api("/api/repl", {"command": "dict"})["round"].get("dictionary") or []
if own:
    raise SystemExit("18.1: caller has %d defs of its own; use a fresh "
                     "identity so its effective dictionary is std" % len(own))

probes = ["(not true)", "(list-length [1 2 3])",
          "(lambda (xs) (list-map (lambda (x) (int-add x 1)) xs))",
          "(lambda (not) (not 0))", "(comb-s true true)",
          "(rec-get #1 [[#1 42]])"]
sources = [(p, p) for p in probes] + [(f, open(f).read()) for f in files]

def cli_compile(src):
    with tempfile.NamedTemporaryFile("w", suffix=".sabra") as t:
        t.write(src); t.flush()
        r = subprocess.run([cli, "compile", t.name], capture_output=True, text=True)
    if r.returncode != 0:
        return None, None, r.stderr.strip()
    h, lock = None, {}
    for line in r.stdout.splitlines():
        parts = line.split()
        if parts[0] == "hash": h = parts[1]
        if parts[0] == "lock": lock[parts[1]] = parts[2]
    return h, lock, None

n = 0
for label, src in sources:
    ch, clock, cerr = cli_compile(src)
    try:
        d = api("/api/programs", {"source": src})
        sh, serr = d["hash"], None
        slock = {e["name"]: e["hash"] for e in d.get("lock", [])}
    except urllib.error.HTTPError as e:
        sh, slock, serr = None, None, e.read().decode()[:200]
    if (ch is None) != (sh is None):
        raise SystemExit("18.1: %s compiles on one side only (cli: %s / server: %s)"
                         % (label, cerr or "ok", serr or "ok"))
    if ch is None:
        print("  %-40s both refuse" % label[:40]); n += 1; continue
    if ch != sh:
        raise SystemExit("18.1 LO1 DRIFT: %s cli %s != server %s" % (label, ch, sh))
    if clock != slock:
        raise SystemExit("18.1: %s lock differs: cli %s server %s" % (label, clock, slock))
    n += 1
print("  %d/%d sources: identical program hash and lock" % (n, len(sources)))
PY

echo "ACCEPT 18 OK"
