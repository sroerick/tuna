#!/usr/bin/env python3
"""Forensics corpus driver (borg/forensics.borg §fault-corpus).

Deliberate-fault corpus over stdlib-v1-bound programs.  Every fault is
produced by SCRIPTED MUTATION (never hand-edited) and lands as a
journaled CAS patch (`POST /api/programs/:hash/patch`), so the corpus is
auditable history like everything else in tuna.

Four fault classes (chapter vocabulary):

  class-1 STRUCTURAL   a flipped ternary digit / dropped arm in a pinned
                       subtree; expected answer = the exact mutation path,
                       found by first-diff alone.
  class-2 SEMANTIC     a wrong dictionary def consumed by an unchanged
                       caller; first-diff lands at the mutated def's
                       boundary and provenance names its IR span.
  class-3 WORLD        the prim answer in a recorded run is wrong, the
                       trees are identical; only a journal fork localizes
                       it (a different terminal status is required, and
                       both the parent and the fork replay-verify).
  class-4 DIVERGENCE   omega-shaped fault; expected answer is the run's
                       Loop closure pair, never a fuel burn.

The driver talks to a live server over HTTP (TUNA_HTTP_PORT,
TUNA_SMOKE_TOKEN via env, same defaults as verify-lib.sh), executes the
whole SEAL -> ... -> HANDOFF loop, prints one JSON object with the
measured numbers, and its exit status mirrors deriv-check: 0 green, 1
any-miss, 2 malformed (bad corpus / unreachable server).

Gate 3 COLLABORATION is the driver re-running the localization protocol
from the same starting hashes a second time and requiring every answer
to match exactly.
"""

import json
import os
import sys
import urllib.error
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tree import (  # noqa: E402
    at,
    decode,
    encode,
    replace,
    tree_hash,
)

BASE = "http://127.0.0.1:%s" % os.environ.get("TUNA_HTTP_PORT", "18090")
DEV_DIR = os.environ.get("TUNA_DEV_DIR", "/tmp/tuna-dev")


def _token():
    tok = os.environ.get("TUNA_SMOKE_TOKEN", "")
    if tok:
        return tok
    for path in (
        os.path.join(DEV_DIR, "bootstrap.token"),
        os.path.join(DEV_DIR, "server.log"),
    ):
        try:
            with open(path) as f:
                for line in f:
                    line = line.strip()
                    if line.startswith("TUNA_BOOTSTRAP_TOKEN="):
                        tok = line.split("=", 1)[1]
        except OSError:
            pass
    if not tok:
        raise SystemExit("forensics: no bearer token (set TUNA_SMOKE_TOKEN)")
    return tok


TOKEN = _token()

_QUERIES = 0


class ForensicsError(Exception):
    pass


def api(path, data=None, method=None, allow=None):
    """One API query.  Counts toward the query budget.  Raises on
    unexpected status; `allow` is a set of statuses returned as values."""
    global _QUERIES
    _QUERIES += 1
    body = None if data is None else json.dumps(data).encode()
    req = urllib.request.Request(
        BASE + path,
        data=body,
        headers={
            "Authorization": "Bearer " + TOKEN,
            "Content-Type": "application/json",
        },
        method=method or ("POST" if data is not None else "GET"),
    )
    try:
        with urllib.request.urlopen(req, timeout=300) as f:
            return f.status, json.load(f)
    except urllib.error.HTTPError as e:
        payload = json.loads(e.read().decode() or "{}")
        if allow and e.code in allow:
            return e.code, payload
        raise ForensicsError("HTTP %d %s: %s" % (e.code, path, payload))


COMPILE_FUEL = 50_000_000


def repl(command):
    _, r = api("/api/repl", {"command": command, "compile_fuel": COMPILE_FUEL})
    if "round" not in r:
        raise ForensicsError("repl %r: %s" % (command, r.get("error")))
    return r["round"]


def def_(name, src):
    return repl("def %s %s" % (name, src))


def compile_hash(src):
    r = repl("eval " + src)
    if "program_hash" not in r:
        raise ForensicsError("compile %r: %s" % (src, r))
    return r["program_hash"]


def get_program(h):
    _, r = api("/api/programs/" + h)
    return r


def first_diff(a, b):
    return repl("first-diff %s %s" % (a, b))["ternary"]


def run(program_hash, inputs, grants=None, fuel=100000, semantics="v0",
        trace=False):
    _, r = api(
        "/api/runs",
        {
            "program_hash": program_hash,
            "inputs": inputs,
            "grants": grants or [],
            "fuel": fuel,
            "size_cap": 1000000,
            "semantics": semantics,
            **({"trace": True} if trace else {}),
        },
    )
    if "run" not in r:
        raise ForensicsError("run %s: %s" % (program_hash, r))
    return r


def run_verify(run_id):
    _, r = api("/api/runs/" + run_id)
    return r["run"]["verify_status"]


def fork(run_id, edits):
    _, r = api("/api/journals/%s/fork" % run_id, {"edits": edits})
    return r


def patch(program_hash, path, expected_old_hash, new_ternary):
    _, r = api(
        "/api/programs/%s/patch" % program_hash,
        {
            "path": path,
            "expected_old_hash": expected_old_hash,
            "new_ternary": new_ternary,
        },
    )
    return r


def grant(prim):
    _, r = api("/api/grants", {"prim": prim})
    return r["id"]


# -- mutation operators -------------------------------------------------
# All operators are total on the declared shape and return the whole
# rewritten tree; `inject` re-addresses it to submit only the replacement
# subtree (the patch API's CAS unit).


def mutate_drop_left(t, path):
    sub = at(t, path)
    if sub[0] != "F":
        raise ForensicsError("drop_left: %r is not a fork" % path)
    return replace(t, path, sub[1]), "drop-left"


def mutate_drop_right(t, path):
    sub = at(t, path)
    if sub[0] != "F":
        raise ForensicsError("drop_right: %r is not a fork" % path)
    return replace(t, path, sub[2]), "drop-right"


def mutate_stem_to_leaf(t, path):
    sub = at(t, path)
    if sub[0] != "S":
        raise ForensicsError("stem_to_leaf: %r is not a stem" % path)
    return replace(t, path, ("L",)), "stem->leaf"


def mutate_swap_arms(t, path):
    sub = at(t, path)
    if sub[0] != "F":
        raise ForensicsError("swap_arms: %r is not a fork" % path)
    return replace(t, path, ("F", sub[2], sub[1])), "swap-arms"


MUTATORS = {
    "drop-left": mutate_drop_left,
    "drop-right": mutate_drop_right,
    "stem->leaf": mutate_stem_to_leaf,
    "swap-arms": mutate_swap_arms,
}


def inject(program_hash, path, mutation):
    """Apply a scripted mutation to a stored program, land it as a CAS
    patch, and return (faulty_hash, mutation_name)."""
    ternary = get_program(program_hash)["ternary"]
    t = decode(ternary)
    new_t, name = MUTATORS[mutation](t, path)
    sub_hash = tree_hash(at(t, path))
    new_ternary = encode(at(new_t, path))
    r = patch(program_hash, path, sub_hash, new_ternary)
    if r.get("hash") != tree_hash(new_t):
        raise ForensicsError("patch hash mismatch: %s vs %s" % (r, tree_hash(new_t)))
    return r["hash"], name


# -- corpus definition --------------------------------------------------

FAULTS = []


def fault(**kw):
    FAULTS.append(kw)
    return kw


# ---- class 1: structural -------------------------------------------
# Every `path` below was checked against the compiled tree: the mutated
# node's shape matches the operator, the fault changes observable
# behavior on `inputs`, and `first_diff` lands exactly on `expect`.
# `expect` differs from `path` only for swap-arms, where the mutated
# fork keeps its shape and first-diff descends into the swapped arm
# (path+"1") - that descent is the localization answer.

fault(id="c1-not-stem", klass=1, base="(lambda (w) (not w))",
      mutation="stem->leaf", path="11", expect="11", inputs=["0"])

fault(id="c1-not-fork", klass=1, base="(lambda (w) (not w))",
      mutation="drop-left", path="12", expect="12", inputs=["10"])

fault(id="c1-and-swap", klass=1, base="(lambda (w) (bool-and (not w) %10))",
      mutation="swap-arms", path="1010", expect="10101", inputs=["0"])

fault(id="c1-and-drop", klass=1, base="(lambda (w) (bool-and (not w) %10))",
      mutation="drop-left", path="10", expect="10", inputs=["0"])

fault(id="c1-length-drop", klass=1, base="(lambda (w) (list-length w))",
      mutation="drop-left", path="1010", expect="1010", inputs=["10"])

fault(id="c1-intneg-stem", klass=1, base="(lambda (w) (int-neg w))",
      mutation="stem->leaf", path="1", expect="1", inputs=["0"])

fault(id="c1-xor-drop", klass=1, base="(lambda (w) (bool-xor (not w) %10))",
      mutation="drop-right", path="10102", expect="10102", inputs=["0"])

fault(id="c1-or-stem", klass=1, base="(lambda (w) (bool-or (not w) %0))",
      mutation="stem->leaf", path="1", expect="1", inputs=["0"])

# ---- class 2: semantic (wrong def consumed by unchanged caller) ----
# Good caller compiled against the stock def; faulty caller compiled
# after the def is redefined (a wrong dictionary def).  The caller
# source is byte-identical in both, so the program-hash first-diff
# isolates the def boundary and provenance names its IR span.

fault(id="c2-first", klass=2, def_name="c2-first", good_def="(lambda (x) (first (pair %0 x)))",
      bad_def="(lambda (x) (first (pair x %0)))",
      caller="(lambda (w) (c2-first w))", inputs=["10"])

fault(id="c2-isleaf", klass=2, def_name="c2-isleaf", good_def="(lambda (x) (is-leaf x))",
      bad_def="(lambda (x) (is-stem x))",
      caller="(lambda (w) (pair (c2-isleaf w) w))", inputs=["10"])

fault(id="c2-natpred", klass=2, def_name="c2-natpred", good_def="(lambda (x) (nat-pred x))",
      bad_def="(lambda (x) (nat-sum x))",
      caller="(lambda (w) (c2-natpred w))", inputs=["2100"])

# ---- class 3: world-via-journal (wrong prim answer) ----------------
# Program tree identical in all cases; the branch chosen by the echoed
# answer decides whether the run closes as `loop` (non-leaf answer) or
# diverges to a different terminal status (leaf answer).  Only a journal
# fork can localize the difference; no tree query sees it.

WORLD_SRC = (
    '(lambda (x) ((if (is-leaf (prim "echo" x)) '
    '(lambda (z) z) (lambda (z) (z z))) (lambda (r) (r r))))'
)

fault(id="c3-echo-leaf", klass=3, world=True, src=WORLD_SRC, prim="echo",
      input="10", edit="0", expect_terminal=["fuel_exhausted"])

fault(id="c3-echo-leaf2", klass=3, world=True, src=WORLD_SRC, prim="echo",
      input="2100", edit="0", expect_terminal=["fuel_exhausted"])

# ---- class 4: divergence (omega-shaped fault; Loop closure pair) ----
# The fault is injected as a scripted CAS patch at the root, exactly
# like the structural classes: compile a well-behaved base, land the
# omega tree as a replacement subtree, and observe the Loop closure.
OMEGA_SRC = "(lambda (x) ((rec-fix (lambda (r) (r r))) x))"
OMEGA_SRC2 = "(lambda (x) ((rec-fix (lambda (r) (r (r r)))) x))"

fault(id="c4-omega-fix", klass=4, good="(lambda (x) (pair x x))",
      omega=OMEGA_SRC, inputs=["10"], semantics="v1")

fault(id="c4-omega-fix2", klass=4, good="(lambda (x) (pair x %0))",
      omega=OMEGA_SRC2, inputs=["10"], semantics="v1")

# ---- count-preservation counterexample (chapter CLAIMS NOT MADE) ----
# step count is NOT a behavior-equivalence oracle: the two programs
# below have different behavior and identical step counts.
COUNTEREXAMPLE = {
    "a": "(lambda (x) (pair x %0))",
    "b": "(lambda (x) (pair x %10))",
    "inputs": ["10"],
}


# -- execution ----------------------------------------------------------


def run_class_1(f):
    h_good = compile_hash(f["base"])
    h_faulty, mname = inject(h_good, f["path"], f["mutation"])
    global _QUERIES
    start = _QUERIES
    fd = first_diff(h_good, h_faulty)
    queries = _QUERIES - start
    localized = fd == f["expect"]
    good = run(h_good, f["inputs"], semantics="v0")
    faulty = run(h_faulty, f["inputs"], semantics="v0")
    behavioral = (
        good["run"]["status"] != faulty["run"]["status"]
        or good["run"].get("result_ternary") != faulty["run"].get("result_ternary")
    )
    gv = run_verify(good["run"]["id"])
    fv = run_verify(faulty["run"]["id"])
    return {
        "id": f["id"], "class": 1, "mutation": mname,
        "mutation_path": f["path"], "expected_path": f["expect"],
        "localized_path": fd, "localized": localized, "queries": queries,
        "good_hash": h_good, "faulty_hash": h_faulty,
        "behavioral_change": behavioral,
        "good_status": good["run"]["status"],
        "faulty_status": faulty["run"]["status"],
        "good_verify": gv, "faulty_verify": fv,
    }


def run_class_2(f):
    def_(f["def_name"], f["good_def"])
    h_good = compile_hash(f["caller"])
    def_(f["def_name"], f["bad_def"])
    h_faulty = compile_hash(f["caller"])
    global _QUERIES
    start = _QUERIES
    fd = first_diff(h_good, h_faulty)
    prog = get_program(h_faulty)
    queries = _QUERIES - start
    tags = {t["path"]: t for t in (prog.get("ir") or {}).get("tags", [])}
    spans = [
        (p, t)
        for p, t in tags.items()
        if fd == p or (fd and p and (fd.startswith(p) or p.startswith(fd)))
    ]
    named = any(t.get("span") for _, t in spans)
    good = run(h_good, f["inputs"], semantics="v0")
    faulty = run(h_faulty, f["inputs"], semantics="v0")
    gv = run_verify(good["run"]["id"])
    fv = run_verify(faulty["run"]["id"])
    return {
        "id": f["id"], "class": 2, "caller": f["caller"],
        "expected_boundary": fd, "first_diff_path": fd,
        "boundary_ok": fd != "", "provenance_named": named,
        "queries": queries, "good_hash": h_good, "faulty_hash": h_faulty,
        "behavioral_change": (
            good["run"]["status"] != faulty["run"]["status"]
            or good["run"].get("result_ternary")
            != faulty["run"].get("result_ternary")
        ),
        "good_status": good["run"]["status"],
        "faulty_status": faulty["run"]["status"],
        "good_verify": gv, "faulty_verify": fv,
    }


def run_class_3(f):
    h = compile_hash(f["src"])
    g = grant(f["prim"])
    good = run(h, [f["input"]], grants=[g], fuel=20000, semantics="v1")
    rid = good["run"]["id"]
    if good["run"]["status"] == "normal":
        raise ForensicsError("c3 %s: base run must be non-normal" % f["id"])
    global _QUERIES
    start = _QUERIES
    journal = [j for j in good["journal"] if j["prim"] == f["prim"]]
    if not journal:
        raise ForensicsError("c3 %s: no %s journal row" % (f["id"], f["prim"]))
    callsite = journal[0]["callsite_path"]
    edit = {"seq": journal[0]["seq"], "result_ternary": f["edit"]}
    forked = fork(rid, [edit])
    queries = _QUERIES - start
    fv = run_verify(forked["run"]["id"])
    terminal = forked["run"]["status"]
    verdict = forked["verify"].get("verify")
    return {
        "id": f["id"], "class": 3, "prim": f["prim"],
        "callsite_path": callsite, "fork_status": terminal,
        "expected_terminal": f["expect_terminal"],
        "different_terminal": terminal in f["expect_terminal"]
        and terminal != good["run"]["status"],
        "verify": verdict, "fork_verify_status": fv,
        "parent_verify_status": run_verify(rid),
        "queries": queries, "good_status": good["run"]["status"],
        "good_hash": h,
    }


def run_class_4(f):
    h_good = compile_hash(f["good"])
    h_omega = compile_hash(f["omega"])
    # the fault itself is a scripted CAS patch at the root: the omega
    # tree replaces the root of the well-behaved base, yielding exactly
    # the pattern whose Loop closure names the offending (fun, arg).
    gt = decode(get_program(h_good)["ternary"])
    ot = decode(get_program(h_omega)["ternary"])
    r = patch(h_good, "", tree_hash(gt), encode(ot))
    if r.get("hash") != h_omega:
        raise ForensicsError("c4 %s: root patch != compiled omega" % f["id"])
    # recorded as a live run over the patched program, trace on.
    excl = run(h_omega, f["inputs"], fuel=20000,
               semantics=f["semantics"], trace=True)
    rid = excl["run"]["id"]
    st = excl["run"]["status"]
    _, tr = api("/api/runs/%s/trace" % rid)
    loop_events = [e for e in tr["events"] if e["kind"] == "loop"]
    pair = None
    if loop_events:
        pair = (loop_events[-1]["fun"], loop_events[-1]["arg"])
    vec = run_verify(rid)
    return {
        "id": f["id"], "class": 4, "status": st,
        "loop_closure_pair": pair, "loop_not_fuel": st == "loop",
        "has_pair": pair is not None and all(pair),
        "trace_loop_detected": tr["trace"]["loop_detected"],
        "steps": excl["run"].get("step_count"),
        "program_hash": h_omega, "base_hash": h_good,
        "patch_path": "", "verify_status": vec,
    }


def run_counterexample():
    ha = compile_hash(COUNTEREXAMPLE["a"])
    hb = compile_hash(COUNTEREXAMPLE["b"])
    out = []
    for inp in COUNTEREXAMPLE["inputs"]:
        a = run(ha, [inp], semantics="v0")["run"]
        b = run(hb, [inp], semantics="v0")["run"]
        out.append({
            "input": inp,
            "a_steps": a.get("step_count"), "b_steps": b.get("step_count"),
            "a_result": a.get("result_ternary"), "b_result": b.get("result_ternary"),
        })
    count_preserved = all(r["a_steps"] == r["b_steps"] for r in out)
    behavior_differs = all(r["a_result"] != r["b_result"] for r in out)
    return {
        "a": COUNTEREXAMPLE["a"], "b": COUNTEREXAMPLE["b"],
        "cases": out, "count_preserved": count_preserved,
        "behavior_differs": behavior_differs,
        "pinned": count_preserved and behavior_differs,
    }


def execute():
    results = []
    for f in FAULTS:
        try:
            if f["klass"] == 1:
                results.append(run_class_1(f))
            elif f["klass"] == 2:
                results.append(run_class_2(f))
            elif f["klass"] == 3:
                results.append(run_class_3(f))
            elif f["klass"] == 4:
                results.append(run_class_4(f))
            else:
                raise ForensicsError("unknown class %r" % f["klass"])
        except ForensicsError as e:
            results.append({"id": f["id"], "class": f["klass"], "error": str(e)})
    try:
        counterexample = run_counterexample()
    except ForensicsError as e:
        counterexample = {"error": str(e)}
    return results, counterexample


def answer_of(r):
    """The localization answer a transcript reaches, per class."""
    if "error" in r:
        return ("error", r["error"])
    c = r["class"]
    if c == 1:
        return ("path", r["localized_path"], r["mutation"])
    if c == 2:
        return ("boundary", r["first_diff_path"], r["provenance_named"])
    if c == 3:
        return ("fork", r["fork_status"], r["callsite_path"])
    return ("loop", r["status"], tuple(r["loop_closure_pair"] or ()))


def aggregate(results, counterexample, second=None):
    c1 = [r for r in results if r["class"] == 1 and "error" not in r]
    c2 = [r for r in results if r["class"] == 2 and "error" not in r]
    c3 = [r for r in results if r["class"] == 3 and "error" not in r]
    c4 = [r for r in results if r["class"] == 4 and "error" not in r]
    errs = [r for r in results if "error" in r]
    n1, n2 = len(c1), len(c2)
    loc1 = sum(1 for r in c1 if r["localized"])
    loc2 = sum(1 for r in c2 if r["boundary_ok"] and r["provenance_named"])
    gate1 = (
        n1 >= 3 and n2 >= 2
        and (100.0 * loc1 / n1 if n1 else 0) >= 90.0
        and (100.0 * loc2 / n2 if n2 else 0) >= 80.0
    )
    q12 = [r["queries"] for r in c1 + c2]
    q3 = [r["queries"] for r in c3]
    gate2 = (not q12 or max(q12) <= 8) and (not q3 or max(q3) <= 12)
    # collaboration: every localization answer reproduces exactly
    if second is not None:
        first_ans = {r["id"]: answer_of(r) for r in results}
        second_ans = {r["id"]: answer_of(r) for r in second}
        gate3 = first_ans == second_ans and len(first_ans) == len(FAULTS)
    else:
        gate3 = False
    # every class-1/2 fault must be an OBSERVED behavior change (a
    # deliberate fault, not a semantically inert edit), and both the
    # good and faulty records must replay-verify.
    gate4 = (
        len(c3) >= 2
        and all(
            r["different_terminal"]
            and r["fork_verify_status"] == "verified"
            and r["parent_verify_status"] == "verified"
            for r in c3
        )
        and len(c4) >= 2
        and all(
            r["loop_not_fuel"] and r["has_pair"]
            and r["trace_loop_detected"] and r["verify_status"] == "verified"
            for r in c4
        )
        and all(
            r["behavioral_change"]
            and r["good_verify"] == "verified"
            and r["faulty_verify"] == "verified"
            for r in c1 + c2
        )
    )
    gate5 = (not errs and len(results) == len(FAULTS)
             and counterexample.get("pinned", False))
    return {
        "class1_total": n1, "class1_localized": loc1,
        "class2_total": n2, "class2_localized": loc2,
        "class3_total": len(c3), "class4_total": len(c4),
        "localization_class1_pct": round(100.0 * loc1 / n1, 1) if n1 else 0.0,
        "localization_class2_pct": round(100.0 * loc2 / n2, 1) if n2 else 0.0,
        "max_queries_class12": max(q12) if q12 else 0,
        "max_queries_class3": max(q3) if q3 else 0,
        "gate1_localization": gate1,
        "gate2_query_budget": gate2,
        "gate3_collaboration": gate3,
        "gate4_counterfactual": gate4,
        "gate5_unattended": gate5,
        "counterexample_pinned": counterexample.get("pinned", False),
        "errors": errs,
    }


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "--json"
    try:
        results, counterexample = execute()
        results2, _ = execute()
    except ForensicsError as e:
        print("MALFORMED: %s" % e, file=sys.stderr)
        return 2
    agg = aggregate(results, counterexample, second=results2)
    out = {"aggregate": agg, "results": results,
           "counterexample": counterexample, "collaboration": results2}
    print(json.dumps(out, indent=2))
    gates = [
        agg["gate1_localization"],
        agg["gate2_query_budget"],
        agg["gate3_collaboration"],
        agg["gate4_counterfactual"],
        agg["gate5_unattended"],
    ]
    return 0 if all(gates) else 1


if __name__ == "__main__":
    sys.exit(main())
