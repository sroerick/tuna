# Forensics corpus — SEAL → HANDOFF over hash-addressed artifacts

Acceptance for `borg/forensics.borg`. A deliberate-fault corpus over
stdlib-v1-bound programs, and the unattended gate
`scripts/verify-10-forensics.sh` (exit 0 green / 1 any-miss / 2
malformed, mirroring `deriv-check`).

The driver is `forensics.py` (scripted mutation; `tree.py` holds the
ternary-tree primitives). It talks to a live server over HTTP and runs
the whole protocol loop. `verify-10-forensics.sh` is a thin wrapper
that also enforces CORE UNTOUCHED (`interpreter/ compiler/ common/`
byte-identical to HEAD) and prints the measured number beside every
gate.

The corpus is **idempotent under rerun**: every fault recompiles fresh
programs (content-addressed, so recompiles are no-ops) and re-runs;
the shared dev db only ever gains the same hash-addressed rows.

## Running

```
eval $(opam env --switch=poohstack --set-switch)
export TUNA_HTTP_PORT=18091 TUNA_SMOKE_TOKEN=<token>   # or verify-lib defaults
scripts/verify-10-forensics.sh                          # the gate
python3 scripts/forensics-corpus/forensics.py --json     # raw corpus JSON
```

## Protocol (T1) — curl over the live server

Every fact is a hash-addressed artifact; a transcript replays in
another agent's hands from the starting hashes alone. All requests carry
`Authorization: Bearer $TOKEN`.

```sh
B=http://127.0.0.1:18091

# SEAL — compile the known-good program; its run rides a deriv record
POST $B/api/repl   {"command":"eval <src>","compile_fuel":50000000}
                   -> {"round":{"program_hash":H,"ternary":T,...}}
POST $B/api/runs   {"program_hash":H,"inputs":[...],"fuel":100000,...}
                   -> {"run":{"id":R,"status":...,"result_ternary":...},
                       "journal":[...]}
GET  $B/api/runs/$R            -> {"run":{"verify_status":"verified",...}}

# DIFF — first divergent path of two program trees (and of two records)
POST $B/api/repl   {"command":"first-diff <ha> <hb>"}
                   -> {"round":{"ternary":"<path>"}}

# CAUSE — world-answered or code-answered? fork the journal with an
# edited prim answer; if the outcome moves, the divergence is journal-shaped
POST $B/api/journals/$R/fork  {"edits":[{"seq":N,"result_ternary":"0"}]}
                   -> {"run":{"id":F,"status":...},"verify":{...}}
GET  $B/api/runs/$F            -> verify both the parent and the fork
GET  $B/api/runs/$R/trace      -> per-firing digest view ("loop" events
                                  carry the named (fun,arg) closure pair)

# LOCATE — tree path -> callsite pair -> provenance -> IR node + span
GET  $B/api/programs/$H        -> {"ternary":...,"ir":{"tags":[{"path",
                                  "span",...}]}}
#   callsite paths are canonical compiled-tree paths paired with the
#   program hash (every journal row carries one)

# FIX — CAS patch at the divergent path; expected old hash = the diff's
# failing subtree; produces a fresh program hash, old rows stay history
POST $B/api/programs/$H/patch  {"path":"<p>","expected_old_hash":"<h>",
                                "new_ternary":"<subtree>"}
                   -> {"hash":"<fresh>"}     # 409 carries first-diff

# PROVE — replay-verify the fix against the frozen journal, then (where
# the class calls for it) a fresh live run
GET  $B/api/runs/$F            -> verify_status
POST $B/api/runs   {...}       -> the new count beside the fault table

# HANDOFF — the transcript is exactly the queries above, re-runnable
# from the starting hashes; two runs must agree on every answer.
```

## Fault table (measured 2026-10-04, verify-10 green)

N = 15 faults: 8 class-1, 3 class-2, 2 class-3, 2 class-4. Class-1
`path` is the mutation path; class-1 `expect` equals `path` except for
swap-arms, where the mutated fork keeps its shape so first-diff
descends into the swapped arm (`path`+"1").

| id | class | base / caller | mutation | path | first-diff | queries |
|----|-------|---------------|----------|------|-----------|---------|
| c1-not-stem     | 1 | `(lambda (w) (not w))`                    | stem->leaf | 11    | 11    | 1 |
| c1-not-fork     | 1 | `(lambda (w) (not w))`                    | drop-left  | 12    | 12    | 1 |
| c1-and-swap     | 1 | `(lambda (w) (bool-and (not w) %10))`     | swap-arms  | 1010  | 10101 | 1 |
| c1-and-drop     | 1 | `(lambda (w) (bool-and (not w) %10))`     | drop-left  | 10    | 10    | 1 |
| c1-length-drop  | 1 | `(lambda (w) (list-length w))`            | drop-left  | 1010  | 1010  | 1 |
| c1-intneg-stem  | 1 | `(lambda (w) (int-neg w))`                | stem->leaf | 1     | 1     | 1 |
| c1-xor-drop     | 1 | `(lambda (w) (bool-xor (not w) %10))`     | drop-right | 10102 | 10102 | 1 |
| c1-or-stem      | 1 | `(lambda (w) (bool-or (not w) %0))`       | stem->leaf | 1     | 1     | 1 |
| c2-first        | 2 | def `c2-first`; caller `(lambda (w) (c2-first w))` | wrong def | — | 2, provenance-named | 2 |
| c2-isleaf       | 2 | def `c2-isleaf`; caller `(lambda (w) (pair (c2-isleaf w) w))` | wrong def | — | 10211, provenance-named | 2 |
| c2-natpred      | 2 | def `c2-natpred`; caller `(lambda (w) (c2-natpred w))` | wrong def | — | 1, provenance-named | 2 |
| c3-echo-leaf    | 3 | identical tree; wrong `echo` answer `0`   | journal fork | — | fork → `fuel_exhausted` (parent `loop`) | 1 |
| c3-echo-leaf2   | 3 | identical tree; wrong `echo` answer `0`   | journal fork | — | fork → `fuel_exhausted` (parent `loop`) | 1 |
| c4-omega-fix    | 4 | omega patched over `(lambda (x) (pair x x))`   | root CAS patch | — | `loop`, named closure pair | — |
| c4-omega-fix2   | 4 | omega patched over `(lambda (x) (pair x %0))`  | root CAS patch | — | `loop`, named closure pair | — |

Class-1/2 are **observed behavior changes** (a deliberate fault, not an
inert edit) and both the good and faulty records replay-verify.
Class-3 program trees are byte-identical, so no tree query can see the
difference — only the fork localizes it. Class-4 faults are landed as
scripted CAS patches at the root, exactly like the structural classes.

## Gate verdicts (measured 2026-10-04)

1. LOCALIZATION — class-1 8/8 = 100.0% exact mutation path (≥90);
   class-2 3/3 = 100.0% def boundary + provenance span (≥80).
2. QUERY BUDGET — class-1/2 max 2 queries (≤8); class-3 max 1 (≤12).
3. COLLABORATION — two driver runs from identical starting hashes agree
   on every localization answer exactly.
4. COUNTERFACTUAL — class-3 2/2 different terminal status with both
   records replay-verified; class-4 2/2 `loop` with a named (fun,arg)
   closure pair and verified replay.
5. UNATTENDED — the script itself, zero human state.

## Claims deliberately not made

Step count is **not** a behavior-equivalence oracle across different
tree shapes. The counterexample is pinned and re-checked every gate
run: `(lambda (x) (pair x %0))` vs `(lambda (x) (pair x %10))` on input
`10` both take **2 steps** but return `2100` vs `21010` (behavior
differs, count preserved). Gates passing scope the claim to
stdlib-v1-scale programs and agent debuggers only. A frozen gate miss
is **F9 SUPERLATIVE OVERREACH** in `FINDINGS.md`, never a quietly
relabeled test.
