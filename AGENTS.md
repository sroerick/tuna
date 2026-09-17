# AGENTS.md — Tuna

Standalone tree-calculus evaluator daemon. Read `SPEC.md` (v0 handoff
spec) and `tuna.borg` + `borg/*.borg` (the v1 borge book) before
changing behavior; the book is the source of truth and code must
converge to it.

## Stack (fixed; do not swap)

- OCaml 5 + dune, opam switch **poohstack** (`eval $(opam env --switch=poohstack --set-switch)`)
- HTTP: **Dream** (Lwt); pages are server-rendered **htmx** fragments
  (`server/static/htmx.min.js` is vendored)
- Postgres: **pgx/pgx_lwt** (pure OCaml client, same line as pricklypear)
  — socket dir `/tmp`, see `scripts/dev.sh`
- JSON: **yojson**; hashing: **digestif** (sha256); tests: **alcotest**

## Build / test / run

```
eval $(opam env --switch=poohstack --set-switch)
dune build @all          # exit 0 is the gate
dune runtest             # alcotest suites must pass
scripts/dev.sh start     # boots PG cluster (/tmp/tuna-pgsup, :5434, db tuna) + server (:18090)
scripts/dev.sh status    # migrations auto-apply
curl -s http://127.0.0.1:18090/health
```

Never use sudo. PG runs as the local user with trust auth over the
unix socket. DB migrations live in `migrations/*.sql`, idempotent-ish:
each file must guard itself against re-application (0001 inserts itself
into `schema_migrations` inside its own transaction).

## Domain rules (load-bearing; do not improvise)

1. **Trees.** `type t = Leaf | Stem of t | Fork of t * t` — that's all.
2. **Canonical serialization** is the ternary string format:
   `0`=leaf, `1`+child=`stem`, `2`+left+right=`fork` (see
   `reference/tree-calculus/implementation/python` — same encoding as
   upstream). Hash = `sha256` of that string, lowercase hex.
3. **Reduction rules** are the olydis 2024 triage rules, exactly the
   `apply` function in `reference/tree-calculus/implementation/ocaml/lib/tree.ml`.
   Do not paraphrase — port it verbatim.
4. **Step counting** (the differential-harness invariant): a step is
   one firing of a *triage rule* (the Fork cases: fork(leaf,x),
   fork(stem,_), fork(fork,_) with Leaf/Stem/Fork arguments). The two
   "wrapper" applications (`apply Leaf b = Stem b`, `apply (Stem a) b =
   Fork(a,b)`) are application, NOT steps. Known-good values:
   `size(not) = 8`; `not true -> leaf in 2 steps` (rule 3b, then 3a).
   Strategy is leftmost-innermost in exactly the order the OCaml
   reference evaluates (`apply (apply a1 b) (apply a2 b)` — inner a1
   first, then a2, then the outer). This makes the total step count an
   invariant under confluence.
5. **Fuel & size cap.** Every run carries `fuel` (max rule firings) and
   `size_cap` (max live tree size). Exhaustion is a normal result
   (`status fuel_exhausted / size_exhausted`), never an exception,
   watchdog, or timeout. The run boundary additionally enforces a
   wall-clock cap (`TUNA_RUN_MAX_SECONDS`, default 10s; 0/negative
   disables): a run past it finalizes as `deadline_exceeded`. That is
   operator policy at the boundary only - pure evaluation is untimed,
   so step counts remain an invariant of the calculus. Replay and the
   fork counterfactual carry the same cap (an aborted replay is a
   verdict-unverifiable, never a divergence; an aborted counterfactual
   finalizes its fork row as deadline_exceeded), and deadline_exceeded
   rows themselves are unverifiable: the clock is not a calculus fact.
   Compile-time reduction (repl eval/def, source program upload) has
   its own budget: default 1e8 firings, per-request `compile_fuel` /
   `compile_size_cap` on /api/repl + /api/programs, and the same clock
   policy under its own knob `TUNA_COMPILE_MAX_SECONDS` (default 10s;
   0 disables) - a compile past it is a 400 "compile failed", never a
   pinner.
6. **The journal is the system.** Faithful replay = re-execute
   program+inputs with prim calls answered sequentially from the run's
   journal rows. Replay must not touch the live host/network.
7. **Grants are arguments.** The evaluator never resolves names. At a
   prim boundary the host checks the grant row live; denial is a
   journaled error result, not an exception.
8. **Callsite paths** are canonical paths in the compiled tree, paired
   with the program hash. Every journal row carries one; diagnostics
   resolve tree-path → IR-path → source span.

## Layout

```
common/       tree type, ternary serialize/parse, hashing, ids
interpreter/  pure core: apply + fuel/size-bounded stepper
compiler/     surface s-expr -> IR (with spans) -> bracket abstraction w/ eta -> tree + provenance
store/        pgx_lwt access layer over migrations schema
server/       Dream app: JSON API + htmx UI + REPL; bin/main.exe entry
cli/          thin terminal clients (repl, curl wrappers) if needed
reference/    vendored upstream tree-calculus repo (normative) + our CL twin
migrations/   additive SQL
scripts/      dev.sh orchestration, verify_*.sh acceptance scripts
```

## Verification culture

- After any change: `dune build @all && dune runtest` must be green
  before commit. Commit early, commit small.
- `borge lint && borge report` must stay clean; if a stanza's reality
  changes, update the borg book in the same commit (status flips are
  make-pass work; docstring/details edits accompany code).
- Acceptance scripts (acceptance.criteria) live in `scripts/verify_*`
  and are the definition of done per borg chapter — wire them up as
  their chapter lands.
- Failure definitions F1–F4 (borg/acceptance.borg) are findings, not
  bugs: if you observe one, record it in
  `FINDINGS.md` with evidence, don't hide it.
