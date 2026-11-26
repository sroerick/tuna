#!/bin/bash
# acceptance 18 (borg/local.borg): local, offline front ends.
#   18.1 L1 STDLIB OFFLINE - every in-tree .sabra source and a set of
#        std-name probes compile through the CLI (no server, frozen
#        stdlib) to the SAME program hash POST /api/programs answers
#        for the caller, and the CLI lock equals the server lock.  The
#        caller must have no defs of its own (checked: the REPL dict
#        command answers no rows), so its effective dictionary is std.
#        Run against an instance seeded from stdlib/v1/manifest
#        (verify-8 checks that); a mismatch is LO1 drift.  A source
#        both sides refuse (a moved pin, a non-std name) must be
#        refused with the same compile message.
#   18.2 L2 LOCAL REPL - one transcript (def, eval with and without
#        inputs, get, a shadowing def of a std name, undef, errors,
#        dict) run through tuna repl over a fresh directory and through
#        /api/repl: every round has the same kind, status, result hash,
#        steps, lock and dict rows, and failed rounds fail with the same
#        message.  The transcript undefines what it defined.
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
          "(rec-get #1 [[#1 42]])",
          "list-fold@3a4225df", "list-fold@00000000"]
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
        body = e.read().decode()
        try: serr = json.loads(body).get("error", body)
        except ValueError: serr = body
        sh, slock = None, None
    if (ch is None) != (sh is None):
        raise SystemExit("18.1: %s compiles on one side only (cli: %s / server: %s)"
                         % (label, cerr or "ok", serr or "ok"))
    if ch is None:
        # same refusal: the CLI's compile message appears in the 400 body
        if cerr not in serr:
            raise SystemExit("18.1: %s refused differently: cli %r server %r"
                             % (label, cerr, serr))
        print("  %-40s both refuse, same message" % label[:40]); n += 1; continue
    if ch != sh:
        raise SystemExit("18.1 LO1 DRIFT: %s cli %s != server %s" % (label, ch, sh))
    if clock != slock:
        raise SystemExit("18.1: %s lock differs: cli %s server %s" % (label, clock, slock))
    n += 1
print("  %d/%d sources: identical program hash and lock" % (n, len(sources)))
PY

echo "[18.2] L2: a REPL transcript answers alike in tuna repl and /api/repl"
LDIR=$(mktemp -d); trap 'rm -rf "$LDIR"' EXIT
python3 - "$BASE" "$TOKEN" "$CLI" "$LDIR" <<'PY' || fail "see the line above"
import json, os, subprocess, sys, urllib.request, urllib.error
base, tok, cli, ldir = sys.argv[1:5]
sfx = os.urandom(3).hex()
inc = "v18inc" + sfx
three = None  # ternary of [1 2 3], filled from round 1

def server(cmd, inputs):
    req = urllib.request.Request(base + "/api/repl",
        data=json.dumps({"command": cmd, "inputs": inputs}).encode(),
        headers={"Authorization": "Bearer " + tok,
                 "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=300) as r:
            o = json.load(r)["round"]
    except urllib.error.HTTPError as e:
        body = e.read().decode()
        try: return ("error", json.loads(body).get("error", body))
        except ValueError: return ("error", body)
    return ("ok", {"kind": o["kind"], "status": o["status"] or None,
                   "hash": o["hash"], "steps": o["steps"] if o["status"] else None,
                   "lock": sorted((l["name"], l["hash"]) for l in o["lock"]),
                   "dict": sorted((d["name"], d["ternary"]) for d in o["dictionary"])})

def local(cmd, inputs):
    argv = [cli, "repl", "--dir", ldir]
    for t in inputs: argv += ["--input", "%" + t]
    r = subprocess.run(argv, input=cmd + "\n", capture_output=True, text=True)
    f = {"lock": [], "dict": []}
    for line in r.stdout.splitlines():
        if not line: continue
        k, _, v = line.partition(" ")
        if k == "error": return ("error", v)
        if k == "lock": f["lock"].append(tuple(v.split(" ", 1)))
        elif k == "def": f["dict"].append(tuple(v.split(" ", 1)))
        else: f[k] = v
    return ("ok", {"kind": f.get("kind"), "status": f.get("status"),
                   "hash": f.get("hash", ""),
                   "steps": int(f["steps"]) if "steps" in f else None,
                   "lock": sorted(f["lock"]), "dict": sorted(f["dict"])})

script = [
  ("[1 2 3]", []),
  ("def %s (lambda (x) (int-add x 1))" % inc, []),
  ("(%s 41)" % inc, []),
  ("(lambda (xs) (list-length xs))", "THREE"),
  ("get 0", []),
  ("get 9", []),
  ("def not (lambda (b) b)", []),
  ("(not true)", []),
  ("undef not", []),
  ("(not true)", []),
  ("eval nosuchname", []),
  ("bogus command", []),
  ("dict", []),
  ("undef " + inc, []),
  ("dict", []),
]
for i, (cmd, inputs) in enumerate(script):
    if inputs == "THREE": inputs = [three]
    s, l = server(cmd, inputs), local(cmd, inputs)
    if s[0] != l[0]:
        raise SystemExit("18.2 round %d %r: server %s vs local %s" % (i, cmd, s, l))
    if s[0] == "error":
        if l[1] not in s[1] and s[1] not in l[1]:
            raise SystemExit("18.2 round %d %r: errors differ: %r vs %r" % (i, cmd, s[1], l[1]))
    elif s[1] != l[1]:
        raise SystemExit("18.2 round %d %r:\n server %s\n local  %s" % (i, cmd, s[1], l[1]))
    if i == 0:
        # the round's result tree, for the inputs of round 3
        with open(os.path.join(ldir, "values", l[1]["hash"])) as f: three = f.read().strip()
    print("  %-44s %s" % (cmd[:44], "same error" if s[0] == "error" else
          "same %s %s steps=%s" % (s[1]["kind"], s[1]["hash"][:8], s[1]["steps"])))
print("  %d/%d rounds identical (kind, status, hash, steps, lock, dict)" % (len(script), len(script)))
PY

echo "ACCEPT 18 OK"
