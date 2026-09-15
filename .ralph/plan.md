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
- [x] `reference/tree-calc.lisp`: faithful CL port of the triage rules + step counter (same counting rule); documented header
- [x] `scripts/diff-corpus/`: corpus files (ternary program + inputs + expected result + expected steps) for: not, id, K, S-flavored compositions, bool ops, nat encoding arithmetic (succ/add/mul), self-apply omega, Y-combinator fixed point, published upstream values
- [x] generate expected steps from upstream OCaml apply (instrumented) once, checked in
- [x] `scripts/verify-differential.sh`: runs sbcl CL reference + tuna CLI evaluator over corpus, compares result hash + step count; exit 0
- [x] `cli/bin/main.exe` needs a subcommand: `tuna eval <ternary-file>` (program + args, fuel, prints ternary + steps + status) — build this
- [x] commit

## M4 — compiler: surface language + bracket abstraction
- [x] `compiler/lib/sexp.ml`: surface s-expr reader: `(lambda (x) e)`, application, leaf 0, define-free (REPL adds defines later), `%/hash` literal tree refs
- [x] IR: named lambda IR with source spans on every node + parent pointers
- [x] compile-time checks: unbound variables, arity errors — reported with IR path
- [x] `compiler/lib/bracket.ml`: bracket abstraction WITH eta (upstream tree_builder.ml is the reference); compile IS reduction: closed expr evaluates during compile
- [x] provenance map: tree-path ↔ IR-node; stored alongside compiled ternary in `programs.ir`
- [x] tests: compile not/bools/nats; compile-time-eval; diagnostics carry IR path
- [x] commit

## M4 notes (loop #8)

- Layout: `compiler/lib/{sexp,ir,bracket,provenance}.ml` (library
  `tuna_compiler`). The reader produces the IR DIRECTLY (single AST,
  no separate surface AST): every node carries a unique id + span;
  closure is enforced at read time — an unbound variable raises
  Ir.Error with the occurrence's IR path, computed post-parse by node
  id (`Ir.find_id`). Multi-arg lambda desugars to nested Lams; bare
  digit-run atoms are rejected (tree literals need %); comments +
  whitespace tolerated.
- Bracket abstraction is a verbatim port of upstream `star_abstraction`
  (with eta). The combinator language mirrors upstream Ref/Node/App
  exactly: `CVar`/`CLeaf`/`CApp` (+`CLam` shell pre-elimination), with
  an id tag threaded through every synthesized node (tag = enclosing
  lambda's id). k u / s u v / i are built as PENDING CApps over the
  leaf — during compile-time eval these reduce by WRAPPER application
  only (no fuel); triage rules fire only for actual redexes, so the
  compiled artifact is the normal form and `compile IS reduction` is
  literal. `to_tagged` mirrors upstream `to_tree` with tags.
- Corpus-fact cross-checks (python reference):
  - compile `(lambda (x) x)` = 21100 (upstream id tree; apply(i,x)=x).
  - compile `(lambda (x) (x x))` = s i i = 212110021100;
    apply(sii,sii) diverges (python hits RecursionError) — the
    divergent-term compile test uses fuel 200 (compile fails cleanly,
    never hangs; compile fuel/cap default 1e6, overridable).
  - compile `(lambda (f) (lambda (y) x))`-style K = 10; k i (zero's
    form) = 2021100.
- Extensional tests through the M2 interpreter: compiled zero, church
  succ (succ zero not false = true), K, S (s not not false = not
  false (not false) = 2010) all reduce correctly.
- Provenance: every compiled-tree node carries the id of the IR node
  responsible for it (`tags`, pre-order, path convention 0=stem-child
  1=fork-left 2=fork-right — matches ternary digits);
  provenance.ml resolves tree path → IR node → IR path → span and
  backs `describe`. Compile determinism (same hash AND same tags on
  recompile) is asserted — call-sites.stability's recompilation rule.
- CLI: `tuna compile <source-file>` (hash/ternary/size/steps; compile
  errors exit 1 with the IR path), `tuna eval-compiled <ternary>
  [args...] [--fuel N] [--cap N]` (the M3 deviation's promised bare-
  ternary subcommand). Bug found in first draft: eval-compiled fed the
  program token into the args list — fixed before commit.
- borge gotcha (cost ~15min): `(* agent note ... *)` comments must sit
  at PROJECT level — placing one inside a subsection (after details)
  silently breaks tuna.borg's inline-tree match and the report starts
  orphaning every file, while `borge lint` stays clean. Note moved to
  project level in borg/call-sites.borg.
- OCaml gotcha (recurring): inline-record constructor args need ALL
  fields (`App {id; span; fn; arg}` — omitting fn/arg is a type error,
  not a record-update opportunity).
- Tests now: 36 (M1/M2) + 26 (M4) green; differential harness 24/24
  across refeval / CL twin / tuna CLI.
- Deviation: plan's "parent pointers" are realized as (unique node id
  + on-demand structural path via `Ir.find_id`/`at_path`) — same
  navigation capability, no mutable parent links. Plan checkbox kept
  as-is with this note.
- Deviation: plan's "stored alongside compiled ternary in `programs.ir`
  " — the artifact type (`Bracket.artifact` + `Provenance.t`) is ready
  for the M5 store row; actual persistence lands with M5's schema.

## M5 — store layer
- [x] `store/lib/db.ml`: pgx_lwt pool over unix socket /tmp:5434 db tuna user tuna (env-configurable, matching dev.sh)
- [x] accessors: programs upsert(get-or-create by hash), fetch; runs insert/update; journals append (with hash chain) / fetch by run ordered; grants mint/fetch/revoke/deny-check
- [x] identities: bootstrap token handling at server boot; token verify (sha256 lookup)
- [x] migration runner re-used from dev.sh (server assumes schema exists)
- [x] integration tests gated behind env TUNA_TEST_PG=1 (skip silently otherwise)
- [x] commit

## M6 — HTTP API (JSON, agent surface)
- [x] `server/lib/api.ml` (Dream, Lwt): 
  - POST /api/programs (ternary or source; returns hash), GET /api/programs/:hash
  - POST /api/programs/:hash/patch {path, expected_old_hash, new_ternary} → CAS apply or 409 with first-diff
  - POST /api/runs {program_hash, inputs[], fuel, size_cap, grants[]} → run row (executes synchronously v0-style, journals every prim event)
  - GET /api/runs/:id (row + journal), GET /api/runs?caller=&program=
  - GET /api/journals/:run_id; POST /api/journals/:run_id/fork {edits} → derived journal run
- [x] auth: Authorization: Bearer <token>; 401 otherwise; grant checks via grants table
- [x] /health (live + db ping), no auth
- [x] curl smoke script `scripts/smoke-api.sh` exercising the whole chain
- [x] commit

## M6 notes (loop #14)

- Layout: `server/lib/{api,patch}.ml` (library `tuna_server`) +
  `server/bin/main.ml` (entry: TUNA_HTTP_PORT default 18090; PP_BOOTSTRAP
  identity bootstrap — TUNA_BOOTSTRAP_TOKEN or generated-and-printed-once,
  sha256 stored via Store.bootstrap_identity name "root"; re-boots with a
  DIFFERENT token against an existing root are a hard boot error, never a
  silent new credential).
- Patch engine (`server/lib/patch.ml`): structural CAS per SPEC §4 — path
  digits match ternary/provenance convention (0=stem-child 1=fork-left
  2=fork-right, leading '/' tolerated, ""=root); Atomic outcome type
  Applied{ternary;hash} / Conflict{expected,actual,first_diff} /
  Bad_path. Successful patches create a NEW program row (old row immutable
  history); CAS conflicts 409 with the two subtree hashes, and — when the
  caller supplies old_ternary consistent with its pinned hash — a
  first-diff path walk between believed vs stored subtree. Smoke [5]
  covers apply, idempotent-repeat-conflict, first_diff="" (root shape
  mismatch), and bad-path 404.
- Run semantics (v0, pre-prim): POST /api/runs inserts the run row, then
  evaluates synchronously with the M2 stepper, then updates status /
  result_ternary / step_count. Pure tree programs journal nothing (the
  prim boundary + journaling is M7's boundary). grants[] validated
  UP-FRONT against the grants table (exists, unrevoked, belongs to
  caller → 403 on failure) — the M7 prim boundary will do per-call checks
  using the same Store.check_grant primitive.
- Journal fork (data-plane counterfactual, journal.counterfactual-edits):
  POST /api/journals/:run_id/fork validates edit seqs against the parent
  journal (edit beyond known seq → 400), copies rows through
  append_journal so the hash chain REBUILDS over the new run id (row
  fingerprints include run_id, so every row_hash changes — expected),
  applies edits (result_ternary / error / clear), records
  derived_journals (run_id, parent_run_id), and snapshots the parent's
  status/result. RE-EXECUTION of the edited suffix is M7's replay engine
  — fork currently produces a derived DATA row, not a re-run.
- API JSON facts: program rows carry the ir column (provenance-lite:
  tree-path → IR-node-id tags) when compiled from source; ternary-only
  programs have ir:null. Error convention: 400 malformed input / compile
  error, 401 no/bad bearer, 403 grant denial, 404 unknown hash|run|path,
  409 CAS conflict, 500 store/structural faults. All /api/* requires
  auth; /health is open and reports db ping status without raising.
- Smoke script (scripts/smoke-api.sh, 9 sections, exit 0): health, 401s,
  program post (ternary+source+compile-error), get, patch chain,
  runs (not-true 2-step exact, omega fuel-exact at 5, unknown-grant 403),
  run list / journal fetch / unknown-id 404s, fork (empty-edit +
  out-of-range edit + derived_journals row). Token default: pulled from
  TUNA_BOOTSTRAP_TOKEN line in /tmp/tuna-dev/server.log.
- borge: grants.borg agent note updated (M6 wired the VALIDATION half of
  the run-submission surface; prim-boundary per-call checks + attenuation
  interpretation remain M7). lint clean; report still 0-implemented /
  20-planned (statuses flip in M10 per plan).
- Test counts: 35 unit + 26 compiler + 24 differential + 6 store = 91
  green, plus smoke-api.sh 9/9 sections against a live dev.sh server.

## M7 — prims, journal, replay engine
- [x] `server/lib/prims.ml`: prim registry. v1 set: `echo` (return args tree), `now` (wall clock), `uuid`, `store/get`+`store/put` (kv table), `http/get` (egress against an allowlist env var). Each: contract version const "1", runs at boundary under grant check/journaling
- [x] boundary: interpreter calls out via a callback; host journaling (journal.row-schema: callsite path from program IR provenance — prim-call TAGS: literal-fix prims must be addressable; document callsite-path convention for prims compiled under bracket abstraction: use the IR-node identity, store IR path, then map)
- [x] `server/lib/replay.ml`: faithful replay (journal-fed), verification invariants (result hash, step count, per-seq answer match, chain walk); verify_status update
- [x] divergence JSON: first mismatch seq + first_diff_path (subtree-hash walk) + provenance join
- [x] verification sweeper: GET /api/runs/verify?all=1 + auto-verify run on fetch
- [x] tests: effectful sample program; replay identity; counterfactual fork changes exactly the reachable suffix
- [x] commit

## M8 — htmx UI (PP-style server-rendered)
- [x] `server/lib/pages/`: dashboard (runs table: status/verify badges, program links), program page (ternary + pretty tree render + provenance + patch form), run page (journal table + replay button + divergence view), grants admin (mint/revoke), REPL page
- [x] auth: session cookie for humans (login with identity token), same bearer tokens accepted — keep ONE credential store, PP-style
- [x] htmx fragments for: run list refresh, journal tick, REPL round-trip, verify button
- [x] UI must degrade to pure HTML (no-JS navigable)
- [x] commit

## M8 notes (loop #16)

- Layout: `server/lib/pages.ml` (routes) + `server/lib/pages/{layout,auth,
  dashboard,program,run_page,grants,repl}.ml`. `include_subdirs unqualified`
  in server/lib/dune. All handlers session-auth'd except /login /logout
  (open). Static: `Dream.get "/static/**" (Dream.static dir)` with dir
  from TUNA_STATIC_DIR (dev.sh exports $ROOT/server/static) — htmx.min.js
  vendored since M0.
- Session auth over the ONE credential store: cookie `tuna_session`
  carries the identity's own token; `Auth.identity_of_req` verifies it
  via the SAME `Store.verify_token` used for bearer. No session table,
  no second secret class (v0 stance; upgrade path = keyed/signed
  cookies). Login POST = form token -> verify -> 303 + Set-Cookie
  (HttpOnly, SameSite=Lax, path=/); logout drops it.
- **Dream cookie gotchas (cost ~30min):** (1) `Dream.cookie` defaults
  `~decrypt:true` — a plain (non-encrypted) cookie silently reads as
  None; must pass `~decrypt:false` on the read side since we set
  `~encrypt:false`. (2) `Dream.set_cookie` takes a RESPONSE (not a
  promise), so cookie-setting handlers do `redirect >>= fun resp ->
  set_cookie resp req ...`.
- htmx pattern: every interactive control is a PLAIN FORM working
  without JS (POST -> 303 redirect back). With `HX-Request: true` the
  same URL returns a FRAGMENT: dashboard runs-table tick
  (`/frag/runs`, every 5s), journal tick (`/runs/:id/journal`, every
  2s — the tick STOPS by dropping its own poll attribute once the run
  is finished), verify button (`POST /runs/:id/verify` swaps the badge
  + divergence table in place), patch/run/repl/grant fragments.
  `<noscript>` fallback links on ticked sections.
- Run page: auto-verifies an unverified finished run on fetch (same
  rule as GET /api/runs/:id); failed runs get a re-verify button; the
  divergence view renders Replay.verdict as addressed structure (seq /
  callsite path / prim / first_diff_path / recorded vs replayed hash).
- Refactor: run execution shared by API + UI via
  `Run.execute_run` (lives in Run, NOT Api, to avoid the module cycle
  Api -> Pages -> Program -> Api): up-front grant validation
  (Store.check_grant per id), program fetch, boundary execution;
  returns `(row, journals) | (code, msg) result`. **BUG FIXED en
  route:** api post_run used to insert a run row AND call Run.execute
  (which inserts its own) — an orphan 'running' row per API run;
  removed the duplicate insert.
- Program page: canonical ternary, box-drawing tree outline
  (`Layout.tree_outline`), provenance table from the ir column's tags
  (tree path -> IR node id -> span), CAS patch form (path /
  expected_old_hash pre-filled with the program hash / new_ternary),
  and a run-it form (inputs one-per-line, fuel/cap, grant ids). htmx
  error fragments for bad path / CAS conflict; no-JS variants answer
  404/409 error pages.
- REPL page (M8 slice): compile+eval round-trip over the pure engine
  (same engine as the differential harness; compile IS reduction).
  Dictionary/defines/journaled transcripts + POST /api/repl are M9.
  A `(prim ...)` fired at compile time correctly surfaces as a compile
  error (not an effect) even from the UI.
- New smoke: `scripts/smoke-ui.sh` — 16 sections, exit 0: health+static,
  anon redirects, login (bad 401 / good + cookie), dashboard + htmx tick
  + noscript, frag/runs both modes, program page sections, lookup
  redirect, run page + auto-verify, journal tick both modes, patch
  (apply / no-JS 303 / bad path fragment+404 / conflict fragment+409),
  run form (fragment + no-JS 303), repl (round-trip / no-JS page /
  compile error / page), grants (page / mint / revoke / unknown prim
  400), out-of-band journal tamper -> verify button shows 'verify
  failed' while the run row keeps its recorded status, logout.
- Tests now: 36 unit + 26 compiler + 12 prim + 11 store = 85, plus
  differential 24/24, smoke-api 13/13, smoke-ui 16/16.

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

## M5 notes (loop #12)

- Layout: `store/lib/{pgx_io,db,store}.ml` (library `tuna_store`),
  tests in `tests/store_tests.ml` (6 alcotest-lwt suites) gated by
  `scripts/test-store.sh` (skips silently when PG is down — dev.sh
  start-pg to enable), wired into `dune runtest` via tests/dune.
- **Deadlock #1 (pre-spin)**: the first draft filled the pool eagerly
  with `Lwt_mvar.put` — a second put on a full mvar BLOCKS until a
  consumer takes, so `Db.init` never returned (nobody had taken conn #1
  yet). Fix: connections are created lazily in `take_conn` (mvar first
  via `take_available`, else connect while `created < size`, else block
  on the mvar). `created` is an int ref — Lwt's cooperative scheduling
  makes check+incr atomic up to the first await.
- **Deadlock #2 (put-blocking)**: even with lazy creation, a naive
  `created < size` take never consults the mvar, so returned conns pile
  up and `with_pool`'s finalize `put` blocks forever on the second
  call. The `take_available`-first rule fixes both.
- pgx 2.2 protocol facts: `connect ~host:"/tmp"` routes '/'-prefixed
  hosts to `<dir>/.s.PGSQL.<port>` in our pgx_io Thread module (libpq
  convention); `simple_query` handles multi-statement SQL incl.
  BEGIN/DDL/COMMIT fine (used by apply_migrations); jsonb round-trips
  with server-defined spacing — compare parsed (Yojson), never text.
- Bug fixed: `insert_run` omitted `status` (not-null violation) — now
  inserts 'running' explicitly. 0001-init.sql is already applied, so
  schema stays untouched; store-side default.
- borge: grant-token/revocation (grants.borg) and row-schema
  (journal.borg) flipped to `partial` with project-level agent notes;
  lint clean, report shows 3 stanzas out of planned (partial isn't a
  report column — same as M4's call-sites.provenance).
- Housekeeping: `reference/tree-calculus/implementation/dune` now uses
  `data_only_dirs (ocaml)` (no-arg-list form) — the recurring
  `ignored_subdirs` deprecation warning from loops 1–11 is gone.
- Tests now: 35 unit + 26 compiler + 24 differential + 6 store = 91
  green.

## M3 notes (loop #6)

- Previous loop (#5) died mid-write: the .corpus files contained ONLY
  the expect lines (no program/arg/fuel/size_cap). Loop #6 rebuilt the
  corpus via a checked-in generator, `scripts/diff-corpus/regen.sh`:
  it writes each entry, generates expects with `tools/gen refeval`
  (instrumented verbatim copy of upstream apply — the independent
  generator the plan wanted), and fails if the CL twin disagrees.
  Re-run it after changing any corpus definition.
- Corpus = 24 entries: not_true/false/not_not_true, id_not, k_xy,
  s_k_k_id, and/or all 4 arg combos, omega_4self (fuel 17, 4 steps),
  grower_cap8/10 (same run, cap 8 trips / cap 10 completes), fix_fuel
  (Y(identity) fuel-exhausts at exactly 500), succ_dag_7 (upstream DAG
  succ on nat encoding: nat 7 -> nat 8, 98 steps — reproduces the
  previous loop's recorded value), succ_zero/five (church succ,
  observable via not/false), add_ch_2_3 / mul_ch_2_3 /
  church3_succ_zero (church arithmetic observable via not/false:
  not^(m+n) false etc.).
- `tools/gen` gained a `TCase` term constructor: the old
  `App(App(N, App(f0,f1)), f2)` tcase was WRONG — `App` marshals by
  eager application, which corrupts the T{f0,f1,f2} dispatch tree.
  TCase marshals structurally to Fork(Fork(f0',f1'),f2').
- Bracket-abstraction gotchas discovered while building bool ops:
  - raw-variable triage branches (λu.b, or bare V b) do NOT survive
    SK elimination as pointwise functions — `or` is therefore defined
    as not(and(not a)(not b)) from the verified and/not;
  - identity tree 21100 (upstream's id_ternary) is identity only on
    function-trees: id x reduces to apply x x, so id Leaf = Stem Leaf,
    id StemLeaf = Fork(Leaf,StemLeaf). Do not use it as a pointwise
    identity in corpus programs (that silently broke or_ff/ft once).
  - Correct and: T{λu.false, λu.λv.b, λx.λy.λz.b} a b — branch arity
    matters: dispatch gives f0 NO consumed arg, f1 gets u, f2 gets u v,
    and each branch then receives the remaining and-arg.
- All bool entries verified semantically: and T=10/else 0,
  or F=0/else 10 (only Leaf is false in tree calculus).
- `scripts/verify-differential.sh` compares checked-in expects against
  all three engines (refeval / sbcl CL twin / tuna CLI); wired into
  `dune runtest` (tests/dune rule, skipped silently if sbcl absent).
  24/24 agree.
- Deviation: plan's `tuna eval <ternary-file>` is implemented as
  `tuna eval <corpus-file>` — the corpus format carries program+args+
  fuel+cap together, which is what the differential harness needs;
  a bare-ternary subcommand can come with the M4 compiler.
- tools/search: brute-force hunt tool for an intensional (chain-nat)
  add; exploratory, kept for reference, not part of the corpus.

## Deviations (append as they occur)

- M8: the REPL page is the M8 SLICE (pure compile+eval round-trip);
  the plan's M9 items (name->tree dictionary per identity, defines,
  journaled REPL transcripts as run rows, POST /api/repl) remain open
  and are the next milestone.
- M8: session-cookie auth carries the identity token itself (v0; ONE
  credential store, no session table) — upgrade path noted in M8
  notes. Also: plan's "pretty tree render" is a box-drawing ASCII
  outline, no dependency added.
- M7 notes (loop #15, continued into #16 — loop #15 died on a dead PG
  cluster; this loop finished + verified + committed):

  - Layout: `common/lib/{cstr,cprim}.ml[i]` (strings-as-trees codec:
    tree = UTF-8 bytes of chars-as-unary-`Stem` chains... concretely
    Leaf="", Stem chain per char? — see cstr.ml; and the prim GATE
    convention: a prim call is `Fork (gate, Fork (name-tree, site))`
    with gate = Stem^4 Leaf = `11110`, inert as data until applied);
    `interpreter/lib/prim_eval.ml` (Prim_eval.Make(M): ONE verbatim
    triage port parameterized over a MONAD — the pure Eval API stays
    untouched, Run/Replay instantiate it in Lwt, unit tests in the
    identity monad; prim boundary is FUEL-FREE and STEP-FREE (AGENTS
    rule 4), host answers `\`Ok tree | \`Error msg` with errors
    becoming the canonical error tree `Stem (cstr msg)` so the
    calculus keeps computing deterministically);
    `server/lib/{prims,run,replay}.ml`; `compiler` gained a `(prim
    "name" ...)` surface form.
  - Compiler/prim interplay: a `(prim ...)` under a lambda compiles to
    a normal-form function embedding the gate; a TOP-LEVEL or
    compile-reducible prim (e.g. `((prim "echo") x)` under a lambda
    whose args don't depend on x, or `(prim "echo" %0)`) would FIRE at
    compile time -> compile error, not an effect (prims execute only
    inside a run boundary). Callsite site = the Prim IR node id; the
    compiled artifact's tags map it to a tree path; bracket
    duplication of a callsite resolves to the FIRST tag (v0).
  - Run boundary (Run.execute): inserts run row (inputs stored
    content-addressed so replay can recover them), evaluates with the
    Lwt host: per-call LIVE grant check (exists, unrevoked, belongs to
    caller — mid-run revocation bites) + v0 attenuation (null/{}
    admit-all; {"max_ternary": N} caps encoded args length); denial is
    a JOURNALED error answer, run continues (AGENTS rule 7); every
    event journaled with callsite path + prim_contract "1" + grant_id
    + wall_ms; payloads over Prims.payload_cap (65536) are journaled
    as errors, never inlined.
  - Replay (server/lib/replay.ml): faithful replay = the SAME
    Prim_eval engine journal-fed (execute_fed) — answers consumed in
    order; divergence = Diverged{div_seq; callsite_path; prim;
    reason; first_diff_path; recorded_hash; replayed_hash}. verify()
    checks chain walk + per-seq match + no unconsumed rows + status /
    result-hash / step-count equality vs the run row (replay identity
    by construction: one engine, one step counter).
    verify_and_record writes verify_status; auto-verify on
    GET /api/runs/:id; sweeper GET /api/runs/verify?all=1 (routed
    BEFORE /:id). Counterfactual fork: edits REPLACE recorded rows,
    chain rebuilt over the new run id, then Replay.reexecute writes
    the counterfactual outcome into the derived row; out-of-range
    edit seq -> 400 (restored M6 semantic that the first M7 draft
    had dropped).
  - BUGS FIXED en route (this loop): (1) scripts/dev.sh only created
    the tuna db on the fresh-init path — `start-pg` on a running
    cluster whose db was missing (the exact error loop #15 died on)
    now self-heals via ensure_db; (2) dev.sh now replays
    TUNA_BOOTSTRAP_TOKEN on re-boot: generated tokens persist to
    /tmp/tuna-dev/bootstrap.token (grep anchored ^TUNA_BOOTSTRAP_TOKEN=
    so error lines naming the var can't poison it) — otherwise a
    restart after a failed boot destroys the only token copy in the
    log and the server can never boot again; (3) smoke-api.sh extracts
    the token from bootstrap.token first; (4) migrations/0001 had
    `grants.minted_by REFERENCES grants(id)` — unusable FK; fixed to
    identities(id) in 0001 + migrations/0003 repairs applied clusters;
    (5) store tests ran against the DEV db and bootstrap identities
    named 'root'/'m7-root', poisoning the dev server's bootstrap —
    scripts/test-store.sh now drops+creates its own `tuna_test` db
    every run; (6) Db.apply_migrations queried schema_migrations
    before the first migration creates it (fresh db -> crash); now
    CREATE TABLE IF NOT EXISTS first.
  - Borge: grants (grant-token, revocation, invocation), journal
    (row-schema, recorded-environment, counterfactual-edits), replay
    (faithful, divergence-surface) flipped to implemented;
    call-sites.provenance note updated (journal side done; span join
    in divergence surface still open); replay.prim-versioning partial
    (contract pinned per row, unconditional replay; mismatch fields
    open). lint clean.
  - Test counts: 36 unit + 26 compiler + 12 prim + 11 store (own
    scratch db) = 85, differential 24/24, smoke-api.sh 13/13 sections.
  - Deviation: book's journal row text says jsonb inline payloads +
    host_build + blob spill; v0 uses ternary text inline, no
    host_build column, payload_cap-instead-of-spill — recorded in
    journal.borg's project note.

- M2: result-variant naming is `Fuel_exhausted`/`Size_exhausted`
  (snake_case) vs plan's `FuelExhausted`/`SizeExhausted` — cosmetic,
  OCaml style.
- M2: AGENTS.md rule 4's parenthetical "(rule 3b, then 3a)" for
  not-true labels the fired rules inconsistently with its own rule list
  (actual: fork(fork,_) then fork(leaf,_)). Step count 2 confirmed.
  Recorded, AGENTS.md untouched.
- M3: CLI subcommand is `tuna eval <corpus-file>` (not bare ternary);
  corpus files bundle program+args+fuel+cap+expects (see notes). The
  bare-ternary subcommand arrived in M4 as `tuna eval-compiled`.
- M4: "parent pointers" in the IR are realized as unique node ids +
  on-demand structural paths (`Ir.find_id` / `Ir.at_path`), not mutable
  parent links — equivalent navigation, simpler OCaml.
- M4: borge agent-note comments must sit at project level (see M4
  notes); call-sites.provenance flipped to `partial` (compiler half
  done, run-time diagnostic wiring lands with M7).

## Open Questions (blocking notes)

- OPERATOR THESIS (roerick, 2026-09-15 — canonical statement now in
  tuna.borg docstring "RUNTIME PURITY THESIS"; binding spec-level:
  tunacore admits no scheduler, end state is a PURE STEP value->value
  core. `Prim_eval.Make(Lwt)` in server/lib is a migration stage.)
  M10+ experiment (NOT blocking M7/M8): effects/flat-machine bridge.
    - Shape (2) is the headline: explicit-stack (CEK-style) evaluator,
      pure `step : state -> state`, perform from the flat loop O(1);
      run suspension = a state VALUE (serializable/steppable) — a
      feature the monad/callback cannot offer without becoming it.
    - Shape (1) local handlers is the control group (answer-in-place,
      O(1) transport; proves the O(depth) tax was rim-design, not
      effects-physics).
    - Known cost asymmetry to measure, not pre-judge: rim-handler
      effects pay O(depth) per capture (prim-heavy adversarial
      programs), monad(Lwt)/flat pay small-per-step constant or zero;
      criterion is MEASURED cost on small-prim + prim-heavy workloads
      with step counts identical, nothing aesthetic.
    - Either outcome is a finding for FINDINGS.md, equal validity.
    - Do not de-color the core before M8 ships; do it as one corpus-
      refereed commit once the journal tests have pinned semantics.

