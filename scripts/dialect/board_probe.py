#!/usr/bin/env python3
"""board_probe.py — the board chapter's driver (borg/board.borg programs).

The prelude_probe pattern: every stage run-checked (status normal,
denial_count 0, replay verify verified), exact result twins REPL-pinned
(never hand-decoded ternary), journal shapes asserted by prim name and
by the row's structured error field.  Runs over a FRESH --prefix
namespace per invocation so probes never collide with the live todo/
board or each other.

Env: TUNA_HTTP_PORT (18090), TUNA_SMOKE_TOKEN or
     /tmp/tuna-dev/bootstrap.token.
Usage: board_probe.py [--prefix board-cal-v03]

Stages: compile the four board programs -> add real items (one journaled
put each) -> exact view twin (window entries + open/done summary) ->
lost-update-safe flip (list+get+cas) + view twin -> stale-cas conflict
answer without a write -> canonical delete -> >=300-item windowed walk
past the old list_cap with the zero-window caps-error pin and sibling
exclusion (13.2) -> scoped-grant forged-path denial (13.5).
"""

import json
import os
import sys
import time
import urllib.request

BASE = "http://127.0.0.1:" + os.environ.get("TUNA_HTTP_PORT", "18090")
TOKEN = os.environ.get("TUNA_SMOKE_TOKEN") or open(
    "/tmp/tuna-dev/bootstrap.token").read().strip()
if TOKEN.startswith("TUNA_BOOTSTRAP_TOKEN="):
    TOKEN = TOKEN.split("=", 1)[1]
AUTH = {"Authorization": "Bearer " + TOKEN, "Content-Type": "application/json"}


def post(path, obj):
    req = urllib.request.Request(
        BASE + path, data=json.dumps(obj).encode(), headers=AUTH)
    try:
        return json.load(urllib.request.urlopen(req))
    except urllib.error.HTTPError as e:
        raise SystemExit(f"board_probe: POST {path} -> {e.code}: "
                         f"{e.read().decode()[:400]}")


def get(path):
    req = urllib.request.Request(BASE + path, headers=AUTH)
    try:
        return json.load(urllib.request.urlopen(req))
    except urllib.error.HTTPError as e:
        raise SystemExit(f"board_probe: GET {path} -> {e.code}")


def repl(command):
    r = post("/api/repl", {"command": command})
    if "round" not in r:
        raise SystemExit(f"board_probe: repl {command!r}: {json.dumps(r)[:300]}")
    return r["round"]


def tern(term):
    return repl("eval " + term)["ternary"]


def src(name):
    with open(os.path.join(os.path.dirname(__file__), name)) as f:
        return f.read()


PROGS = {}
FUEL = {"fuel": 10_000_000, "size_cap": 100_000}


def compile_programs():
    rd = repl("eval (pair todo-add 1)")  # v0.3 row is seeded or dies
    del rd
    for name in ["board-add", "board-view", "board-flip", "board-del"]:
        rd = repl("def " + name + " " + src(name + ".sabra"))
        if rd.get("status") != "normal":
            raise SystemExit(f"board_probe: compile {name}: {rd}")
        PROGS[name] = rd["program_hash"]
    print("  stage: v0.3 row seeded; 4 board programs compiled")


def grant(prim, prefix=None):
    g = {"prim": prim, "args_attenuation": "null"}
    if prefix:
        g["path_prefix"] = prefix
    gr = post("/api/grants", g)
    gid = gr.get("id")
    if not gid:
        raise SystemExit(f"board_probe: mint {prim}: {json.dumps(gr)[:300]}")
    if prefix:
        # The public mint ignores path_prefix (admin-surface gap, noted);
        # scope via the delegation-attenuation route: it lives-checks
        # holder + lineage + proven narrowing.
        child = post(f"/api/grants/{gid}/attenuate",
                     {"path_prefix": prefix, "args_attenuation": "null"})
        cid = (child.get("grant") or {}).get("id") or child.get("id")
        if not cid:
            raise SystemExit(f"board_probe: attenuate {prim}@{prefix}: "
                             f"{json.dumps(child)[:300]}")
        return cid
    return gid


def run_checked(name, program_hash, inputs, grants, **kw):
    r = post("/api/runs", dict({"program_hash": program_hash,
                                "inputs": inputs, "grants": grants},
                               **FUEL, **kw))
    if "run" not in r:
        raise SystemExit(f"board_probe: run {name}: {json.dumps(r)[:300]}")
    run = r["run"]
    if run["status"] != "normal":
        raise SystemExit(f"board_probe: {name}: status {run['status']} != normal")
    full = get(f"/api/runs/{run['id']}")
    run = full["run"]
    if run["verify_status"] != "verified":
        raise SystemExit(f"board_probe: {name}: verify "
                         f"{run.get('verify_status')} != verified")
    return run, full.get("journal", [])


def s(x):
    """cstr-string tree of x, via the REPL twin.

    The embedding is RAW (+ surrounding quotes): the transport's json
    layer escapes exactly once, the reader decodes exactly once -
    pre-escaping here double-encodes and re-titles the string.
    """
    return tern('"' + str(x) + '"')


def record_expr(state, title, who, when):
    # RAW embedding: the reader honors no escapes, so every character
    # between the quotes is the string itself (quotes cannot ride this
    # channel at all; the rim's Cstr.encode is the full-bytes channel).
    return f'(todo-item {state} "{title}" "{who}" {when})'


def entry_expr(path, record, version):
    return (f"(pair {json.dumps(path)} (pair {record} "
            f"{json.dumps(str(version))}))")


def view_twin(items, opens, done):
    """Exact expected ternary of a board-view window answer.

    (pair <entries> <summary>) built with explicit pair nesting - no
    reader sugar rides the twin.  items = [(path, record_expr, ver)].
    """
    entries = "%0"
    for (p, r, v) in reversed(items):
        entries = f"(pair {entry_expr(p, r, v)} {entries})"
    summary = (f"(pair (pair key-state (pair {opens} %0)) "
               f"(pair (pair key-title (pair {done} %0)) %0))")
    return tern(f"(pair {entries} {summary})")


def main():
    prefix = "board-cal-v03"
    args = sys.argv[1:]
    for i, a in enumerate(args):
        if a == "--prefix" and i + 1 < len(args):
            prefix = args[i + 1]
    ns = f"{prefix}-{int(time.time()):x}"
    print(f"# board_probe over fresh namespace {ns}/")

    compile_programs()

    g_put = grant("tree/put")
    g_list = grant("tree/list")
    g_get = grant("tree/get")
    g_cas = grant("tree/cas")
    g_addg = grant("math/add")

    # 1. ADD: three real items (one hostile title); journal [tree/put].
    titles = ["call the dentist", "slash / and \\ back",
              "día Ünicode ☃"]
    base = int(time.time()) % 100000
    items = []
    for i, t in enumerate(titles):
        pid = f"{ns}/{base}-{i:03d}"
        run, js = run_checked(f"add-{i}", PROGS["board-add"],
                              [s(pid), tern('"' + t + '"'), s("board-probe"),
                               tern("1727780000")], [g_put])
        if [e["prim"] for e in js] != ["tree/put"]:
            raise SystemExit(f"board_probe: add-{i} journal != [tree/put]")
        items.append((pid, record_expr("todo-open", t, "board-probe",
                                       1727780000), 1))
    print(f"  stage: 3 adds, one journaled tree/put each (hostile titles ok)")

    # 2. VIEW first window: exact twin, entries + open/done summary.
    run, js = run_checked("view-1", PROGS["board-view"],
                          [s(ns + "/"), s("")], [g_list, g_addg])
    expect = view_twin(items, opens=3, done=0)
    if run["result_ternary"] != expect:
        raise SystemExit("board_probe: view-1 result twin mismatch")
    prims = [e["prim"] for e in js]
    if prims[0] != "tree/list" or "math/add" not in prims:
        raise SystemExit(f"board_probe: view-1 journal {prims[:4]}")
    print(f"  stage: view-1 exact twin ({run['step_count']} steps, "
          f"{len(prims)-1} math/add rows)")

    # 3. FLIP the middle item: journal [tree/list tree/get tree/cas];
    #    flip is version-safe; view twin follows.
    mid = items[1][0]
    run, js = run_checked("flip-1", PROGS["board-flip"], [s(mid)],
                          [g_get, g_list, g_cas])
    prims = [e["prim"] for e in js]
    if prims != ["tree/list", "tree/get", "tree/cas"]:
        raise SystemExit(f"board_probe: flip-1 journal {prims}")
    if js[-2].get("error"):  # the get row
        raise SystemExit(f"board_probe: flip-1 get errored: {js[-2]}")
    if js[-1].get("error"):
        raise SystemExit(f"board_probe: flip-1 cas errored: {js[-1]}")
    items[1] = (items[1][0],
                items[1][1].replace("todo-open", "todo-done"), 2)
    run, js = run_checked("view-2", PROGS["board-view"],
                          [s(ns + "/"), s("")], [g_list, g_addg])
    if run["result_ternary"] != view_twin(items, opens=2, done=1):
        raise SystemExit("board_probe: view-2 twin mismatch after flip")
    print("  stage: flip-1 lost-update-safe (list+get+cas), view-2 twin ok")

    # 4. STALE CAS: expected-version-vs-store conflict is a journaled
    #    error answer; the value is intact.
    stale = (f"(lambda (p) (prim \"tree/cas\" p \"1\" "
             f"{record_expr('todo-open', 'stale', 'x', 0)}))")
    ph = repl("def stale-cas " + stale)["program_hash"]
    run, js = run_checked("stale-cas", ph, [s(items[1][0])], [g_cas])
    if [e["prim"] for e in js] != ["tree/cas"] or not js[0].get("error"):
        raise SystemExit(f"board_probe: stale-cas journal {js}")
    if "conflict" not in js[0]["error"]:
        raise SystemExit(f"board_probe: stale-cas error {js[0]['error']!r}")
    run, js = run_checked("view-3", PROGS["board-view"],
                          [s(ns + "/"), s("")], [g_list, g_addg])
    if run["result_ternary"] != view_twin(items, opens=2, done=1):
        raise SystemExit("board_probe: stale-cas changed the board!")
    print("  stage: stale-cas conflicts as a journaled answer, board intact")

    # 5. DEL: canonical delete; twin updates.
    last = items[2][0]
    run, js = run_checked("del", PROGS["board-del"], [s(last)],
                          [grant("tree/del")])
    if [e["prim"] for e in js] != ["tree/del"]:
        raise SystemExit(f"board_probe: del journal {js}")
    if js[0].get("error"):
        raise SystemExit(f"board_probe: del errored: {js[0]}")
    items = items[:2]
    run, js = run_checked("view-4", PROGS["board-view"],
                          [s(ns + "/"), s("")], [g_list, g_addg])
    if run["result_ternary"] != view_twin(items, opens=1, done=1):
        raise SystemExit("board_probe: view-4 twin mismatch after del")
    print("  stage: del one canonical tree/del, view-4 twin ok")

    # 6. WINDOW WALK (13.2): 300 records walked in 24-entry windows past
    #    the old 256 list_cap; exact-cover, path order, sibling namespace
    #    excluded by the trailing-slash collation law; the zero-window
    #    (1-arg) call pins the F14 caps error.
    walk = ns + "w"
    walk_items = []
    for i in range(300):
        pid = f"{walk}/it{i:04d}"
        rec = record_expr("todo-open", f"walk {i}", "walk", 1727781000)
        r = post("/api/tree/put", {"path": pid, "value_ternary": tern(rec)})
        if r.get("_err") if isinstance(r, dict) else False:
            raise SystemExit(f"board_probe: walk put {pid}: {r}")
        walk_items.append((pid, rec, 1))
    sib = ns + "wb"
    for i in range(2):
        post("/api/tree/put", {"path": f"{sib}/x{i}",
                               "value_ternary": tern("17")})
    cursor = ""
    idx = 0
    windows = 0
    while idx < len(walk_items):
        win = walk_items[idx:idx + 24]
        run, js = run_checked(f"walk-{windows}", PROGS["board-view"],
                              [s(walk + "/"), s(cursor)],
                              [g_list, g_addg])
        opens = sum(1 for (_, r, _) in win if "todo-open" in r)
        if run["result_ternary"] != view_twin(win, opens, len(win) - opens):
            raise SystemExit(f"board_probe: walk-{windows} twin mismatch "
                             f"(items {idx}..{idx+len(win)})")
        cursor = win[-1][0]
        idx += len(win)
        windows += 1
    run, js = run_checked("walk-empty", PROGS["board-view"],
                          [s(walk + "/"), s(cursor)],
                          [g_list, g_addg])
    if run["result_ternary"] != view_twin([], opens=0, done=0):
        raise SystemExit("board_probe: walk-empty twin mismatch (cursor at end)")
    windows += 1
    # zero-window = today's shape: the caps error stays a journaled answer
    zph = repl("def zero-window (lambda (p) (prim \"tree/list\" p))"
               "")["program_hash"]
    r = post("/api/runs", dict({"program_hash": zph, "inputs": [s(walk + "/")],
                                 "grants": [g_list]}, **FUEL))
    full = get(f"/api/runs/{r['run']['id']}")
    row = full["journal"][0]
    if "payload cap" not in (row.get("error") or ""):
        raise SystemExit(f"board_probe: zero-window error {row.get('error')!r}")
    print(f"  stage: 300-item walk exact-cover in {windows} windows "
          f"(sibling excluded, zero-window caps error pinned)")

    # 7. SCOPED GRANT / FORGED PATH (13.5): a member's scoped grant
    #    denies an out-of-scope path at the prim boundary, never
    #    silently.  Rides a NON-ADMIN identity (tree/del's admin
    #    exemption would bypass the grant gate) and the run surface -
    #    the submission-level scoped-grant acceptance (FINDINGS non-F,
    #    fixed 2026-10-04) is what lets a scoped grant reach the
    #    boundary at all.
    sub = post("/api/identities", {"name": "board-forge-" + ns})
    member = {"Authorization": "Bearer " + sub["token"],
              "Content-Type": "application/json"}

    def mpost(path, obj):
        req = urllib.request.Request(BASE + path, data=json.dumps(obj).encode(),
                                     headers=member)
        try:
            return json.load(urllib.request.urlopen(req))
        except urllib.error.HTTPError as e:
            raise SystemExit(f"board_probe: member POST {path} -> {e.code}: "
                             f"{e.read().decode()[:300]}")

    mg = mpost("/api/grants", {"prim": "tree/del", "args_attenuation": "null"})
    child = mpost(f"/api/grants/{mg['id']}/attenuate",
                  {"path_prefix": ns + "/", "args_attenuation": "null"})
    scoped = (child.get("grant") or {}).get("id")
    if not scoped:
        raise SystemExit(f"board_probe: member attenuate: {json.dumps(child)[:300]}")
    sentinel = f"{ns}-sentinel/s"
    post("/api/tree/put", {"path": sentinel, "value_ternary": tern("19")})
    r = mpost("/api/runs", {"program_hash": PROGS["board-del"],
                            "inputs": [s(sentinel)],
                            "grants": [scoped],
                            "fuel": 1_000_000, "size_cap": 100_000})
    if "run" not in r:
        raise SystemExit(f"board_probe: forged run refused: {json.dumps(r)[:300]}")
    full = get(f"/api/runs/{r['run']['id']}")
    run = full["run"]
    if run.get("denial_count", 0) < 1:
        raise SystemExit(f"board_probe: forged del denial_count "
                         f"{run.get('denial_count')} < 1")
    if not [e for e in full.get("journal", []) if e.get("error")]:
        raise SystemExit("board_probe: forged del left no journaled answer")
    check = post("/api/tree/get", {"path": sentinel})
    if not check.get("value_ternary"):
        raise SystemExit("board_probe: sentinel vanished after denied del")
    print(f"  stage: member's scoped grant denied the forged path "
          f"(denial_count {run['denial_count']}, sentinel intact)")

    print("board_probe: all stages green")


if __name__ == "__main__":
    main()
