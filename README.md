# tuna

Standalone **tree-calculus evaluator daemon**: a pure tree-calculus
core, an auditable effect boundary, and a journal that makes every run
faithfully replayable. Trees are exactly

```
type t = Leaf | Stem of t | Fork of t * t
```

Everything else — programs, effects, grants, REPL sessions — is built
on top of that one datatype, with a hash-chained journal as the system
of record. See `SPEC.md` and the `tuna.borg` book (the book is the
source of truth; code converges to it).

## Quickstart

```
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest          # build + unit/differential tests
scripts/dev.sh start                     # PG (:5434, db tuna) + server (:18090)
curl -s http://127.0.0.1:18090/health    # {"status":"ok","db":true}
```

The dev server boots an admin identity (`root`) from
`TUNA_BOOTSTRAP_TOKEN`, or generates one and prints it once (persisted
at `/tmp/tuna-dev/bootstrap.token`). Every `/api/*` route and every
UI page (except login) requires that bearer token:

```
TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' /tmp/tuna-dev/bootstrap.token)
curl -s -H "Authorization: Bearer $TOKEN" http://127.0.0.1:18090/api/runs | head
```

Point a browser at `http://127.0.0.1:18090/login` (same token) for the
htmx UI: dashboard, program pages with provenance + patching, run
journal views with replay-verify buttons, grants admin, REPL.

## The calculus

- **Trees**: `Leaf | Stem t | Fork (t, t)`. Canonical serialization is
  the ternary string format (`0` = leaf, `1`+child = stem,
  `2`+left+right = fork); hash = sha256 of the ternary, lowercase hex.
- **Reduction**: the olydis 2024 triage rules, verbatim port of the
  vendored reference (`reference/tree-calculus/`). A "step" is one
  triage-rule firing; wrapper applications are free. Runs are bounded
  by fuel (max firings) and a live-size cap — exhaustion is a normal
  result, never an exception.
- **Compile IS reduction**: the surface s-expr language
  (`(lambda (x) ...)`, application, `%<ternary>` tree literals) goes
  through bracket abstraction with eta and is fully evaluated at
  compile time. Every compiled-tree node carries the id of the IR node
  responsible for it — the provenance map that diagnostics resolve
  through.

## Surfaces

### CLI (`cli/bin/main.exe`)

```
tuna compile <source-file>       # hash, ternary, size, steps; compile errors carry IR paths
tuna eval <corpus-file>          # run a diff-corpus entry (program+args+fuel+cap)
tuna eval-compiled <ternary> [args...] [--fuel N] [--cap N]
```

### JSON API (bearer auth on everything; 401/400/403/404/409 by case)

```
POST   /api/programs               {"ternary": "..."} | {"source": "..."}  → 201 {hash}
GET    /api/programs/:hash         → row (+ ir provenance when compiled from source)
POST   /api/programs/:hash/patch   {"path","expected_old_hash","new_ternary"} — structural CAS; 409 carries both subtree hashes + first-diff path
POST   /api/runs                   {"program_hash","inputs":[ternary...],"grants":[id...],"fuel","size_cap"} → run row (synchronous, journaled)
GET    /api/runs?caller=&program=  → list
GET    /api/runs/:id               → row + journal (auto-verifies an unverified finished run)
GET    /api/runs/verify?all=1      → replay-verification sweep over all runs
POST   /api/runs/:id/gc            → retention GC: cited tombstone, verifier reports GONE never VERIFIED
GET    /api/journals/:run_id       → full journal
POST   /api/journals/:run_id/fork  {"edits":[{seq, result_ternary|error, ...}]} → counterfactual run (chain rebuilt, edited suffix re-executed)
POST   /api/grants                 {"prim","args_attenuation"?} → grant id
POST   /api/grants/:id/revoke      → revoke (live at every prim call)
POST   /api/repl                   {"command": "..."} or {"term": ...} (+ inputs/grants/fuel/size_cap for eval rounds)
GET    /health                     → no auth, db ping
```

REPL command grammar (`eval`/`def`/`undef`/`get`/`patch`/`first-diff`/
`dict`; a bare term evaluates): see `server/lib/repl_cmd.ml`. Eval and
def rounds are first-class journaled runs, chained per identity via
`parent_run_id`; structural commands are store queries.

### Effects (prims) and the boundary

Programs call out at a prim boundary: `(prim "name" args...)` compiles
to an inert gate-shaped tree that only fires inside a run boundary.
The v1 registry: `echo`, `now`, `uuid`, `store/get`, `store/put`,
`http/get` (egress allowlisted via env). Every call is checked LIVE
against a grant row (mid-run revocation bites) and journaled — denial
is a journaled error answer, not an exception. **Faithful replay** =
re-executing program+inputs with prim answers consumed sequentially
from the run's journal; verification checks the chain, per-seq
answers, result hash and step count. No replay ever touches the live
host or network.

## Repository layout

```
common/       tree type, ternary serialize/parse, hashing, string codec
interpreter/  pure core: apply + fuel/size-bounded stepper; monadic twin
compiler/     surface s-expr → IR (spans) → bracket abstraction w/ eta → tree + provenance
store/        pgx_lwt access layer over the migrations schema
server/       Dream app: JSON API + htmx UI + REPL; bin/main.exe entry
cli/          terminal clients (compile / eval / eval-compiled)
migrations/   additive SQL (each file self-guards against re-application)
scripts/      dev.sh orchestration + smoke/verify/acceptance scripts
reference/    vendored upstream tree-calculus (normative) + CL twin
tests/        alcotest suites (unit / compiler / prim / store / differential)
```

## Tests & acceptance

```
dune runtest                     # 60+ alcotest suites + 24-entry differential corpus (3 engines agree)
scripts/smoke-api.sh             # 14 sections over a live server
scripts/smoke-ui.sh              # 17 sections over the htmx UI
scripts/verify-1..7-*.sh         # acceptance criteria 1–7 (callable in any order, exit 0 on green)
borge lint && borge report       # book/reality consistency
```

Failure definitions F1–F4 (`borg/acceptance.borg`) are pre-registered
findings, not bugs — observed evidence lives in `FINDINGS.md`.
