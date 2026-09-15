# Tuna build plan — loop workspace

You are implementing tuna (see AGENTS.md, SPEC.md, tuna.borg + borg/*.borg).
Work through milestones in order. **One milestone per loop**: implement its
tasks, get `dune build @all && dune runtest` green, run `borge lint`, commit
with a descriptive message, update this file (check the box, add notes under
Deviations if reality diverged from the book), then pick the next milestone
in the NEXT loop. If a milestone is genuinely blocked, write the blocking
question under Open Questions, mark it blocked, and move to the next
milestone that doesn't depend on it.

Milestone statuses live below as checkboxes. Keep subtask granularity; strike
through subtasks you fold into others, appending a parenthetical reason.

Ground rules:
- `eval $(opam env --switch=poohstack --set-switch)` before anything.
- Budget yourself: prefer the smallest slice that compiles, tests, commits.
- The agent identity bootstrap: server must accept `TUNA_BOOTSTRAP_TOKEN`
  (or generate + print one to the log and store its sha256) on first boot,
  inserting an `identities` row (name `root`, is_admin). Follows
  pricklypear's PP_BOOTSTRAP pattern.
- Dream servers print their port from `TUNA_HTTP_PORT` (default 18090).
- pgx_lwt connection string: socket /tmp, port 5434, db tuna, user tuna.
- Postgres may need `scripts/dev.sh start-pg` for integration tests; unit
  tests must NOT require PG (pure interpreter/compiler tests run always).

## M0 — bootstrap [done by hand before this plan started]
- [x] repo scaffold, migrations/0001, scripts/dev.sh, vendored reference/tree-calculus
- [x] dune-project, AGENTS.md, borge book validated

## M1 — common: trees, ternary, hashes
- [x] `common/lib/tree.ml`: `type t = Leaf | Stem of t | Fork of t * t`, size, fsize helpers
- [x] `common/lib/canon.ml`: ternary encode/decode (`0`/`1`+child/`2`+left+right), strict parse errors with offset
- [x] `common/lib/hash.ml`: sha256 lowercase hex over ternary string (digestif)
- [x] alcotest suite: roundtrip, size(not)=8, canonical form is unique (hand-check `not`)
- [x] commit

## M1 notes (loop #1)

- Canonical `not` = `22102000` (verified against the python reference:
  `apply(not, false) = true`, `apply(not, true) = false`). The earlier
  guess `211011101110110` was a bad literal; hand-check done against
  upstream this time.
- Vendored upstream OCaml implementation is EXCLUDED from our dune
  build (`reference/tree-calculus/implementation/dune` uses
  `ignored_subdirs`): it needs `core`/`ppx_expect` which we do not
  vendor. It stays as normative source. If M3 wants an instrumented
  OCaml twin, port the counting apply into a buildable dir instead.
- Removed `(using alcotest 1.9)` from dune-project (unsupported
  extension with installed packages); tests are a plain executable
  under `tests/` wired to the `runtest` alias.
- dune-project `lang dune 3.16`, dune 3.23.1; tests/dune has a stale
  deprecation warning about `ignored_subdirs` — harmless, revisit if
  dune complains harder.

## M2 notes (loop #2)

- Module: `Tuna_interp.Eval` (library `tuna_interp`, dir
  `interpreter/lib`). API: `eval ~fuel ~size_cap ~program args : result`;
  result = `Normal of t * int | Fuel_exhausted of int | Size_exhausted of
  int` (snake_case, OCaml style — plan's CamelCase naming adjusted).
  `apply` is a verbatim instrumented port of upstream `apply` (same match
  arms, same evaluation order); steps counted only on triage-rule
  firings; wrapper applications free. Exceptions `Fuel_out`/`Size_out`
  are internal, caught in `eval` — they never escape the API.
- **CRITICAL OCaml gotcha (cost the loop ~an hour):** native OCaml
  evaluates constructor/function arguments RIGHT-TO-LEFT. Writing
  `Normal (go program args, b.steps)` snapshot `b.steps` BEFORE the run,
  producing "correct tree, 0 steps". Fix: bind `let t = go program args`
  first, then `Normal (t, b.steps)`. Same gotcha bit printf-style probes.
  Any future code mixing effects with constructor arguments must bind
  first. Also: trace/probe stderr disappears under `2>/dev/null` —
  capture `2>&1` when debugging.
- Corpus facts (all cross-checked against the vendored python reference,
  `reference/tree-calculus/implementation/python/tree-calculus.py`):
  - not = `22102000`; not true = leaf in 2 steps; not false = true in
    1 step; not(not(true)) = leaf in 3 steps.
  - omega (self-application fixed point) = `221000` =
    `Fork (Fork (Stem Leaf, Leaf), Leaf)`: f f = f upstream-verified,
    exactly 1 triage firing per application. Run of f against a long
    arg list of f's halts with Fuel_exhausted at exactly fuel (tests at
    0/1/2/17). The first omega attempt (`2100`, Fork(Stem Leaf, Leaf))
    was wrong: it is a GROWER (apply g r = Fork(r, Stem r)), now used
    for the mid-run size_cap test (cap 8 trips after 4 steps; cap 10
    completes in the same 4 steps).
  - identity discovered en route: `21100` (`Fork (Stem (Stem Leaf),
    Leaf)`) satisfies id x = x in 2 steps — candidate corpus entry for
    M3's differential harness.
- Note on AGENTS.md rule 4: "not true -> leaf in 2 steps (rule 3b, then
  3a)" — the count 2 is correct, but the fired rules are
  fork(fork,_) then fork(leaf,_) (per the rule list in the same
  sentence). Labels appear swapped in the note; step-count invariant
  unaffected. Recorded here rather than editing AGENTS.md.
- Tests now 35/35 green (17 M1 + 18 eval).

## M2 — interpreter: apply + bounded stepper
- [x] `interpreter/lib/eval.ml`: port upstream `apply` verbatim (M1 tree type)
- [x] step counting per AGENTS.md rule 4; fuel; live-tree size check against size_cap
- [x] result type: `Normal of tree * steps | FuelExhausted of steps | SizeExhausted of steps`; step budget is exact (omega halts at exactly fuel)
- [x] tests: not/true/false corpus, omega fuel-exact, determinism (run twice, same steps), size counting
- [x] commit

## M3 — differential harness (reference twin)
- [ ] `reference/tree-calc.lisp`: faithful CL port of the triage rules + step counter (same counting rule); documented header
- [ ] `scripts/diff-corpus/`: corpus files (ternary program + inputs + expected result + expected steps) for: not, id, K, S-flavored compositions, bool ops, nat encoding arithmetic (succ/add/mul), self-apply omega, Y-combinator fixed point, published upstream values
- [ ] generate expected steps from upstream OCaml apply (instrumented) once, checked in
- [ ] `scripts/verify-differential.sh`: runs sbcl CL reference + tuna CLI evaluator over corpus, compares result hash + step count; exit 0
- [ ] `cli/bin/main.exe` needs a subcommand: `tuna eval <ternary-file>` (program + args, fuel, prints ternary + steps + status) — build this
- [ ] commit

## M4 — compiler: surface language + bracket abstraction
- [ ] `compiler/lib/sexp.ml`: surface s-expr reader: `(lambda (x) e)`, application, leaf 0, define-free (REPL adds defines later), `%/hash` literal tree refs
- [ ] IR: named lambda IR with source spans on every node + parent pointers
- [ ] compile-time checks: unbound variables, arity errors — reported with IR path
- [ ] `compiler/lib/bracket.ml`: bracket abstraction WITH eta (upstream tree_builder.ml is the reference); compile IS reduction: closed expr evaluates during compile
- [ ] provenance map: tree-path ↔ IR-node; stored alongside compiled ternary in `programs.ir`
- [ ] tests: compile not/bools/nats; compile-time-eval; diagnostics carry IR path
- [ ] commit

## M5 — store layer
- [ ] `store/lib/db.ml`: pgx_lwt pool over unix socket /tmp:5434 db tuna user tuna (env-configurable, matching dev.sh)
- [ ] accessors: programs upsert(get-or-create by hash), fetch; runs insert/update; journals append (with hash chain) / fetch by run ordered; grants mint/fetch/revoke/deny-check
- [ ] identities: bootstrap token handling at server boot; token verify (sha256 lookup)
- [ ] migration runner re-used from dev.sh (server assumes schema exists)
- [ ] integration tests gated behind env TUNA_TEST_PG=1 (skip silently otherwise)
- [ ] commit

## M6 — HTTP API (JSON, agent surface)
- [ ] `server/lib/api.ml` (Dream, Lwt): 
  - POST /api/programs (ternary or source; returns hash), GET /api/programs/:hash
  - POST /api/programs/:hash/patch {path, expected_old_hash, new_ternary} → CAS apply or 409 with first-diff
  - POST /api/runs {program_hash, inputs[], fuel, size_cap, grants[]} → run row (executes synchronously v0-style, journals every prim event)
  - GET /api/runs/:id (row + journal), GET /api/runs?caller=&program=
  - GET /api/journals/:run_id; POST /api/journals/:run_id/fork {edits} → derived journal run
- [ ] auth: Authorization: Bearer <token>; 401 otherwise; grant checks via grants table
- [ ] /health (live + db ping), no auth
- [ ] curl smoke script `scripts/smoke-api.sh` exercising the whole chain
- [ ] commit

## M7 — prims, journal, replay engine
- [ ] `server/lib/prims.ml`: prim registry. v1 set: `echo` (return args tree), `now` (wall clock), `uuid`, `store/get`+`store/put` (kv table), `http/get` (egress against an allowlist env var). Each: contract version const "1", runs at boundary under grant check/journaling
- [ ] boundary: interpreter calls out via a callback; host journaling (journal.row-schema: callsite path from program IR provenance — prim-call TAGS: literal-fix prims must be addressable; document callsite-path convention for prims compiled under bracket abstraction: use the IR-node identity, store IR path, then map)
- [ ] `server/lib/replay.ml`: faithful replay (journal-fed), verification invariants (result hash, step count, per-seq answer match, chain walk); verify_status update
- [ ] divergence JSON: first mismatch seq + first_diff_path (subtree-hash walk) + provenance join
- [ ] verification sweeper: GET /api/runs/verify?all=1 + auto-verify run on fetch
- [ ] tests: effectful sample program; replay identity; counterfactual fork changes exactly the reachable suffix
- [ ] commit

## M8 — htmx UI (PP-style server-rendered)
- [ ] `server/lib/pages/`: dashboard (runs table: status/verify badges, program links), program page (ternary + pretty tree render + provenance + patch form), run page (journal table + replay button + divergence view), grants admin (mint/revoke), REPL page
- [ ] auth: session cookie for humans (login with identity token), same bearer tokens accepted — keep ONE credential store, PP-style
- [ ] htmx fragments for: run list refresh, journal tick, REPL round-trip, verify button
- [ ] UI must degrade to pure HTML (no-JS navigable)
- [ ] commit

## M9 — REPL
- [ ] name→tree dictionary per identity; REPL round = compile+eval a term against dictionary; defines update dictionary rows (new table in migration 0002)
- [ ] REPL transcript journaled as a run row (parent_run_id chain per session)
- [ ] structural commands: get <path>, eval <term>, patch <path>, first-diff <hashA,hashB>
- [ ] served on the REPL page (htmx) AND via POST /api/repl (agent surface)
- [ ] commit

## M10 — acceptance + findings
- [ ] scripts/verify-*.sh, one per acceptance.criteria item 1–7 (scripts callable in any order, each exit 0 on green)
- [ ] wire failures to FINDINGS.md (F1–F4 observed evidence)
- [ ] README.md: quickstart + API reference
- [ ] borge: statuses flipped for implemented stanzas (make pass), agent notes appended to each chapter with evidence lines
- [ ] full run: dev.sh start + smoke + all verify scripts green; commit

## Deviations (append as they occur)

- M2: result-variant naming is `Fuel_exhausted`/`Size_exhausted`
  (snake_case) vs plan's `FuelExhausted`/`SizeExhausted` — cosmetic,
  OCaml style.
- M2: AGENTS.md rule 4's parenthetical "(rule 3b, then 3a)" for
  not-true labels the fired rules inconsistently with its own rule list
  (actual: fork(fork,_) then fork(leaf,_)). Step count 2 confirmed.
  Recorded, AGENTS.md untouched.

## Open Questions (blocking notes)
