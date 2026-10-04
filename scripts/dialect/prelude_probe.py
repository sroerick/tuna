#!/usr/bin/env python3
"""prelude_probe.py — the v0.2 todo retry (borg/dialect.borg v0.2).

The owner's todo probe once bounced with "language not mature enough"
and left no artifact; the chapter's own bridge (bridge_probe.py) became
the citation.  This is the retry on the shipped prelude: items are
seeded through keyed-literal call sites, the create / board / flip /
open-query halves run as sabra programs over granted namespaces, and
every keyed-literal result is asserted against a REPL-computed twin
(differential-pinned, never hand-ternary).  Exits 0 on green; prints
one failure line and exits 1 otherwise.

Env: TUNA_HTTP_PORT (18090), TUNA_SMOKE_TOKEN or /tmp/tuna-dev/bootstrap.token.
Usage: prelude_probe.py [--prefix todo-cal-v02]
"""
import json
import os
import sys
import urllib.error
import urllib.request

PORT = os.environ.get("TUNA_HTTP_PORT", "18090")
BASE = f"http://127.0.0.1:{PORT}"
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))


def src(name):
    with open(os.path.join(HERE, name)) as f:
        return f.read()


def token():
    if os.environ.get("TUNA_SMOKE_TOKEN"):
        return os.environ["TUNA_SMOKE_TOKEN"]
    with open("/tmp/tuna-dev/bootstrap.token") as f:
        return f.read().strip()


TOKEN = token()


def post(path, obj):
    req = urllib.request.Request(
        BASE + path,
        data=json.dumps(obj).encode(),
        headers={"Authorization": "Bearer " + TOKEN,
                 "Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(req) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        raise SystemExit(f"prelude_probe: POST {path} -> {e.code}: {e.read().decode()}")


def repl(command, **kw):
    r = post("/api/repl", dict(command=command, **kw))
    if "round" not in r:
        raise SystemExit(f"prelude_probe: repl {command!r} -> {r}")
    return r["round"]


def tern(term):
    return repl("eval " + term)["ternary"]


def get(path):
    req = urllib.request.Request(BASE + path,
                                 headers={"Authorization": "Bearer " + TOKEN})
    with urllib.request.urlopen(req) as r:
        return json.load(r)


def run_checked(name, program_hash, inputs, grants, fuel=1_000_000):
    run = post("/api/runs", {"program_hash": program_hash, "inputs": inputs,
                             "grants": grants, "fuel": fuel,
                             "size_cap": 100_000})["run"]
    if run["status"] != "normal":
        raise SystemExit(f"prelude_probe: {name} status {run['status']}")
    # the journal and the replay verdict live on the GET (verification
    # is async; the POST row has neither).
    full = get(f"/api/runs/{run['id']}")
    run = full["run"]
    if run["verify_status"] != "verified":
        raise SystemExit(f"prelude_probe: {name} replay "
                         f"{run['verify_status']} != verified")
    # 0014 run-denial surfacing: a green probe run must show ZERO grant
    # denials on the run row (the v1-defect-1 law: a denied prim denies
    # the run — surfaced as denial_count, never silent).
    if run.get("denial_count") != 0:
        raise SystemExit(f"prelude_probe: {name} denial_count "
                         f"{run.get('denial_count')} != 0 "
                         "(a prim denial poisoned this run)")
    return run, [e["prim"] for e in full.get("journal", [])]


def grant(prim, prefix=None):
    obj = {"prim": prim}
    if prefix is not None:
        obj["path_prefix"] = prefix
    return post("/api/grants", obj)["id"]


def main():
    prefix = "todo-cal-v02"
    if "--prefix" in sys.argv:
        prefix = sys.argv[sys.argv.index("--prefix") + 1]

    # 0. server sanity: the v0.2 reader must be live on this binary.
    if tern('{:state %10}') != tern('[[key-state %10]]'):
        raise SystemExit("prelude_probe: server predates v0.2 "
                         "(keyed literal != hand twin)")

    # 1. the schema is data: the probe's own keys are ordinary defs.
    repl("def key-open %111110")
    repl("def key-total %1111110")

    # 2. seed a(open) b(done) c(open) through keyed-literal call sites.
    items = [("a", "todo-open", "buy milk", 1727770000),
             ("b", "todo-done", "ship", 1727770100),
             ("c", "todo-open", "write", 1727770200)]
    for id_, state, title, when in items:
        cmd = "{:state %s :title %s :who \"me\" :when %d}" % (
            state, json.dumps(title), when)
        post("/api/tree/put", {"path": f"{prefix}/{id_}",
                               "value_ternary": tern(cmd)})

    # 3. the CREATE call site: pure vocabulary, one journaled tree/put.
    cph = repl("def todo-create-v02 "
               '(lambda (p) (todo-created p "call the dentist" "me"))'
               )["program_hash"]
    path_d = tern(json.dumps(f"{prefix}/d"))
    _, cprims = run_checked("create", cph, [path_d], [grant("tree/put", prefix)])
    if cprims != ["tree/put"]:
        raise SystemExit(f"prelude_probe: create journal {cprims} != [tree/put]")

    # 4. the BOARD summary: a keyed literal, twin-pinned via the REPL.
    bph = repl("def todo-board-v02 " + src("todo-v02-board.sabra"))["program_hash"]
    pfx = tern(json.dumps(prefix))
    gl, gm = grant("tree/list", prefix), grant("math/add")
    brd = tern("{:open 3 :total 4}")  # a c d open, b done, d created
    run1, bprims = run_checked("board", bph, [pfx], [gl, gm], fuel=10_000_000)
    if "tree/list" not in bprims or "math/add" not in bprims:
        raise SystemExit(f"prelude_probe: board journal lacks prims {bprims}")
    if run1["result_ternary"] != brd:
        raise SystemExit(f"prelude_probe: board mismatch "
                         f"(got {run1['result_ternary']}, want {brd})")

    # 5. the WRITE half: todo-flip, then read the state back by name.
    fph = repl("def todo-flip-v02 " + src("todo-v02-flip.sabra"))["program_hash"]
    path_a = tern(json.dumps(f"{prefix}/a"))
    gg, gp = grant("tree/get", prefix), grant("tree/put", prefix)
    _, fprims = run_checked("flip", fph, [path_a], [gg, gp])
    if "tree/get" not in fprims or "tree/put" not in fprims:
        raise SystemExit(f"prelude_probe: flip journal lacks effects {fprims}")
    rph = repl("def todo-read-v02 "
               '(lambda (p) (todo-state (prim "tree/get" p)))')["program_hash"]
    rrd, _ = run_checked("read-back", rph, [path_a], [gg])
    if rrd["result_ternary"] != tern("todo-done"):
        raise SystemExit("prelude_probe: flip did not set todo-done "
                         f"(got {rrd['result_ternary']})")

    # 6. the FILTER query: exactly the open titles, in board order.
    oph = repl("def todo-open-v02 " + src("todo-v02-open.sabra"))["program_hash"]
    orun, _ = run_checked("open-query", oph, [pfx], [gl], fuel=10_000_000)
    # list-fold traverses right-to-left, so the inline prepend-fold
    # preserves board order: c and d survive the flip.
    want = tern('["write" "call the dentist"]')
    if orun["result_ternary"] != want:
        raise SystemExit(f"prelude_probe: open-query mismatch "
                         f"(got {orun['result_ternary']}, want {want})")

    # 7. the board moves with the tree: same program, live state.
    run2, _ = run_checked("board-2", bph, [pfx], [gl, gm], fuel=10_000_000)
    brd2 = tern("{:open 2 :total 4}")
    if run2["result_ternary"] != brd2:
        raise SystemExit(f"prelude_probe: board-2 mismatch "
                         f"(got {run2['result_ternary']}, want {brd2})")

    print(f"  create: todo-created call site, journal {cprims}, replay verified")
    print("  board: {open 3, total 4} as a keyed literal, replay verified")
    print("  flip: a -> todo-done, read back by name (todo-state), replay verified")
    print("  open-query: [write; call the dentist] exact, replay verified")
    print("  board-2: {open 2, total 4} — the summary moves with the tree")


if __name__ == "__main__":
    main()
