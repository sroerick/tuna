#!/usr/bin/env python3
"""bridge_probe.py — acceptance 12.5 driver (borg/dialect.borg).

Seeds a granted namespace with record items, compiles
scripts/dialect/todo-board.sabra through a REPL def round, runs it with
tree/list + math/add grants, and asserts the board summary, the journal
and replay verification.  Exits 0 on green; prints one failure line and
exits 1 otherwise.

Env: TUNA_HTTP_PORT (18090), TUNA_SMOKE_TOKEN or /tmp/tuna-dev/bootstrap.token.
Usage: bridge_probe.py [--prefix todo-cal-v12]
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
BOARD = os.path.join(ROOT, "scripts", "dialect", "todo-board.sabra")

# the summary record [[1 2] [2 1]] = 2 open, 1 done, canonical ternary
EXPECTED_BOARD = "2220210022020210002220202100220210000"


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
        raise SystemExit(f"bridge_probe: POST {path} -> {e.code}: {e.read().decode()}")


def repl(command, **kw):
    r = post("/api/repl", dict(command=command, **kw))
    if "round" not in r:
        raise SystemExit(f"bridge_probe: repl {command!r} -> {r}")
    return r["round"]


def get(path):
    req = urllib.request.Request(BASE + path,
                                 headers={"Authorization": "Bearer " + TOKEN})
    with urllib.request.urlopen(req) as r:
        return json.load(r)


def main():
    prefix = "todo-cal-v12"
    if "--prefix" in sys.argv:
        prefix = sys.argv[sys.argv.index("--prefix") + 1]

    # seed: a=open, b=done, c=open; item record [[1 state][2 title][4 when]]
    items = [("a", "%10", "buy milk", 1727770000),
             ("b", "%0", "ship", 1727770100),
             ("c", "%10", "write", 1727770200)]
    for id_, state, title, when in items:
        cmd = "[[1 %s] [2 %s] [4 %d]]" % (state, json.dumps(title), when)
        ternary = repl(cmd)["ternary"]
        post("/api/tree/put", {"path": f"{prefix}/{id_}", "value_ternary": ternary})

    pfx = repl(json.dumps(prefix))["ternary"]  # prefix as a string tree
    with open(BOARD) as f:
        source = f.read()
    ph = repl("def todo-board-v12 " + source)["program_hash"]

    gl = post("/api/grants", {"prim": "tree/list", "path_prefix": prefix})["id"]
    gm = post("/api/grants", {"prim": "math/add"})["id"]
    run = post("/api/runs", {"program_hash": ph, "inputs": [pfx],
                             "grants": [gl, gm], "fuel": 10_000_000,
                             "size_cap": 100_000})
    r = run["run"]
    if r["status"] != "normal":
        raise SystemExit(f"bridge_probe: status {r['status']}")
    if r["result_ternary"] != EXPECTED_BOARD:
        raise SystemExit(f"bridge_probe: board mismatch (got {r['result_ternary']})")
    journal = run.get("journal", [])
    prims = [e["prim"] for e in journal]
    if "tree/list" not in prims or "math/add" not in prims:
        raise SystemExit(f"bridge_probe: journal lacks prims {prims}")
    v = get(f"/api/runs/{r['id']}")["run"]["verify_status"]
    if v != "verified":
        raise SystemExit(f"bridge_probe: replay {v} != verified")

    # --- the WRITE half: transition an item open -> done, in-calculus ---
    with open(os.path.join(ROOT, "scripts", "dialect",
                           "todo-transition.sabra")) as f:
        trans = f.read()
    tph = repl("def todo-trans-v12 " + trans)["program_hash"]
    item_path = repl(json.dumps(f"{prefix}/a"))["ternary"]
    gg = post("/api/grants", {"prim": "tree/get", "path_prefix": prefix})["id"]
    gp = post("/api/grants", {"prim": "tree/put", "path_prefix": prefix})["id"]
    trun = post("/api/runs", {"program_hash": tph, "inputs": [item_path],
                              "grants": [gg, gp], "fuel": 1_000_000,
                              "size_cap": 100_000})
    tr = trun["run"]
    if tr["status"] != "normal":
        raise SystemExit(f"bridge_probe: transition status {tr['status']}")
    tprims = [e["prim"] for e in trun.get("journal", [])]
    if "tree/get" not in tprims or "tree/put" not in tprims:
        raise SystemExit(f"bridge_probe: transition journal lacks effects {tprims}")
    # read the state back in-calculus: (rec-val 1 (tree/get path)) == %0
    readph = repl("def todo-read-v12 "
                  "(lambda (p) (rec-val 1 (prim \"tree/get\" p)))")["program_hash"]
    rrun = post("/api/runs", {"program_hash": readph, "inputs": [item_path],
                              "grants": [gg], "fuel": 1_000_000,
                              "size_cap": 100_000})
    if rrun["run"]["result_ternary"] != "0":
        raise SystemExit("bridge_probe: transition did not set state done "
                         f"(got {rrun['run']['result_ternary']})")
    tv = get(f"/api/runs/{tr['id']}")["run"]["verify_status"]
    if tv != "verified":
        raise SystemExit(f"bridge_probe: transition replay {tv} != verified")

    print(f"  bridge: board 2 open / 1 done, {len(journal)} journal rows, "
          f"replay verified")
    print(f"  transition: state open->done via tree/get+tree/put, replay {tv}")


if __name__ == "__main__":
    main()
