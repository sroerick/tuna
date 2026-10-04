# FINDINGS.md — pre-registered failure definitions (F1–F4)

The book (`borg/acceptance.borg` §failure-definitions) names four
failure modes. They are **findings, not bugs**: if one is observed, it
is recorded here with evidence and kept visible — never hidden. None
of F1–F4 blocks shipping v1 acceptance criteria; each is a publishable
answer to "does the structural seam pay for itself?".

Status vocabulary: **not triggered** (no observation), **partial
signal** (related evidence exists; the defining condition not met),
**OBSERVED** (definition met; record the evidence and the recorded
consequence).

---

## F1 — STEP COUNT DOES NOT SURVIVE OPTIMIZATION

> If the first real optimization (hash-consing / memoization) cannot
> reproduce the canonical step count, record the engine split
> (canonical-verifiable vs optimized-unverifiable) and what it costs:
> replay identity becomes a statement about the slow engine only.
> Thesis survives weakened.

**Status: OBSERVED 2026-09-18** (the optimizer arrived: sharing v1, d3c156f; live on town since 76cb59f).

- The defining condition is met verbatim: sharing v1 (borg/sharing.borg) counts DISTINCT firings, keyed by sha256 content digest of the (fun, arg) pair, and its step counts diverge from v0 by design.
- Measured: the T-family costs 2^(d+1)-2 raw firings under v0 vs O(d) distinct under v1 at the same normal form (chapter agent note + fixtures); omega is fuel_exhausted under v0 and a finite loop answer under v1 (live demos 09-17: omega loop@3, run 15c8b75c; the crown loop answers @25,813 steps where v0 burned the 132M-fuel deadline).
- Recorded consequence, weaker than pre-registered: replay identity became a statement about the canonical engine only (differential corpus + three-engine harness stay v0-only; cross-engine step-count equality not claimed) - but the split did NOT cost verifiability. Each run row carries its semantics version (migration 0009) and replay re-executes under the row's own law; both laws verify (borg/sharing.borg replay-per-version). Thesis survives weakened, with the verification cost smaller than predicted.
- Owner call 2026-09-18: engine split accepted; v0 stays the API default (absent semantics field = v0) and the memo stays per-run for now; cross-run demand-memo is planned in borg/sharing.borg (owner-interested, not scheduled).
- Owner call 2026-10-02: the demand-memo leaning shipped (borg/sharing.borg §demand-memo, status implemented).  The three owner-gated questions are answered in force, recorded here because they harden the F1 dirty rule into a boundary law: trust model = OWN-GARDEN (demand_memo keyed by caller; a run consumes only answers its own identity produced), price = FREE (a garden hit is a v1 memo hit: no step, no fuel; demand_hits recorded on the run row), replay = RE-EXECUTE WITHOUT THE GARDEN (a demand_sharing row's step count is an environment fact, not a calculus fact, so replay skips the step-count check for those rows and still enforces status + result identity - clean firings are pure, so the result must reproduce regardless of cache warmth).  Consequence for F1: the "memo stays per-run" line is now "memo is per-run; CLEAN firings additionally share, own-garden only" - dirty firings still never memoize anywhere, so grant liveness and the journal audit are untouched by construction.

- v0 has one engine per mode and no optimization pass: the pure
  evaluator (`interpreter/lib/eval.ml`), the Lwt journaling twin
  (`interpreter/lib/prim_eval.ml`, one verbatim triage port
  parameterized over the monad), and the CL reference twin share a
  single step-counting convention (AGENTS.md rule 4).
- Differential harness: 24/24 corpus entries agree on result hash AND
  step count across all three engines
  (`scripts/verify-differential.sh`), and every finished run
  replay-verifies (`scripts/verify-3-replay-identity.sh`).
- The known confluence caveat is recorded in AGENTS.md rule 4 itself:
  step counting is exact only under the leftmost-innermost order the
  OCaml reference evaluates. Any future hash-consing/memoization must
  preserve that order to keep replay identity (see the M10+ operator
  thesis in `.ralph/plan.md` — the flat-machine experiment explicitly
  keeps step counts identical).

## F2 — PATH PATCHING UNUSABLE UNDER CHURN

> If live multi-user editing collapses into rebase-mania — every patch
> invalidating queued patches — record it: CAS is a correctness tool,
> not a collaboration tool. The habitat use-case over tuna is priced
> out.

**Status: not triggered** (no multi-user churn workload has run).

- v0 patching is structural CAS (`server/lib/patch.ml`): path +
  expected_old_hash → apply or 409 with a first-diff walk. No queued /
  speculative patch layer exists that could churn.
- Open instrument, whenever multi-user editing is exercised: track
  (invalidated queued patches) / (applied patches) under concurrent
  editing. If that ratio collapses, this file records the numbers.

## F3 — THESIS INVERSION

> If, after v1, ~90% of useful work lives in host prims and the tree
> layer is ceremony, record the inversion: the boundary bought
> auditability, not a computing platform; the security properties came
> from prim discipline (replay.prim-versioning) all along. PP gains a
> tier, tuna stays a boundary experiment.

**Status: not triggered** (v0 corpus is mostly pure tree work).

- The v0 corpus (differential harness, M3) is pure calculus: not, id,
  K, S, bool ops, nat arithmetic, omega, Y fixed points — zero prim
  calls. Prim programs in the acceptance corpus are thin: nested
  `echo`, `store/get+put` round-trips, one `http/get` allowlist case.
- The RUNTIME PURITY THESIS (tuna.borg docstring; operator note in
  `.ralph/plan.md`) treats the Lwt monad host as a migration stage —
  the M10+ effects/flat-machine experiment is the designed instrument
  for this question: whichever shape wins on measured cost, with step
  counts identical, is a finding recorded here either way.
- Watch metric when real workloads appear: fraction of run time spent
  inside the prim boundary vs the tree layer.

## F4 — UNREADABLE IN PRACTICE

> If producing a ~100-line useful program requires debugging the
> compiled tree by hand even WITH the provenance map, declare the UX
> bar failed (call-sites.provenance). Trees stay machine-shaped; the
> habitat vision does not port.

**Status: not triggered** (partial signal recorded, deliberately not
escalated).

- Partial signal (size, not readability): bracket abstraction is
  literal-minded about prims — a 3-nested-echo program compiles to a
  901-character ternary with ~900 provenance tags (gate trees
  materialize as data under SK elimination). Nothing was hand-debugged
  through the tree to build it; the surface form stayed 42 characters
  and every callsite still resolves to a source span through the tags
  (`scripts/verify-2-callsite-paths.sh`). The provenance map did its
  one required job on this worst case so far.
- The defining condition (hand-debugging the compiled tree to produce
  a ~100-line useful program) has not been attempted — there is no
  v1-scale useful program yet. When one exists, this file gets the
  anecdote either way.
  - Diagnostics available without reading trees: compile errors carry IR
    paths (`tuna compile` exit 1); divergence surfaces carry seq +
    callsite path + span + first_diff_path (`scripts/verify-5`); the
    program page renders a box-drawing outline plus the provenance
    table.
  - 2026-09-19 F4 experiment (a real appender, live on the dev
    instance): a message-log program (tree/put + tree/get + now,
    journaled, replay-verified) compiled clean and ran end to end with
    zero hand-debugging through the tree — every prim callsite resolved
    to a source span, replay verified both runs. The friction was
    SURFACE, not tree: no cons/pair combinator (had to inline
    (lambda (a b) ((0 a) b))), no string literals (hand-encoded the
    path as a unary Cstr ternary), and compile-IS reduction fires any
    prim call whose args are all constants at COMPILE time (aborts
    with "a prim call fired during compile-time evaluation") — so an
    effect's args must be threaded through a runtime variable (K-style
    wrapper) to survive to run time. That was a genuine dialect gap;
    the first slice (pair/cons names + "..." string literals) cut the
    appender source 401 -> 116 chars with identical behavior.
    - The dialect gap is closed (same commit stream): a `(runtime
      (prim ...))` form now threads the enclosing lambda parameter
      through constant args (the same K combinator, done automatically)
      so compile-IS leaves the call alone; effectful programs write prims
      plainly instead of hand-threading. The runtime-form appender is
      109 source chars (401 pre-dialect), status normal, 23 steps, replay
      verified, 3 journal rows (now / tree/get / tree/put). F4 stays
      not-triggered; the remaining design question is whether compile-IS
      should know a prim is effectful (call-sites.borg: prims are data
      until applied).
    - The unary string codec is gone (same commit stream): Cstr now uses
      the book's BINARY convention (marshal.ml — string = list of chars,
      char = little-endian bit list over bools), so a byte costs O(bits)
      nodes instead of a Stem^n chain.  prim_contract bumps "1" -> "2"
      (every string payload changed shape; "1" rows stay self-consistent
      but are not cross-comparable).  Measured: "log/app" 712 -> 140
      ternary chars, "hello" 548 -> 102, and the runtime-form appender's
      compiled ternary 2384 -> 542 (4.4x).  The byte-values pages stay
      strictly smaller than their tree-encoded twins (bloat test
      updated to the binary closed form).

---

## F5 — INLINE BLOWUP (sabra stdlib)

> borg/stdlib.borg pre-registration: any def > 100k compiled nodes, or
> a consumer layer > 10x its consumed layer. If it fires, that is a
> FINDING: the dictionary-definitions-alone thesis pays an
> automation-shaped size tax.

**Status: not triggered** (2026-10-03, L0-L3 complete in the probe).

- Largest def: int-add at 22,259 nodes (engines fully inline:
  mag-sub twice, mag-add, mag-cmp, int-canonical twice). Next:
  int-cmp 5,613, int-mul 6,213, mag-sub 3,776, mag-add 3,089.
  Median L2 def is in the 300-700 range. Everything is an order of
  magnitude under the 100k tripwire, and the consumer/consumed ratio
  stays near 1 (int-add at 22.2k consumes a ~46k lived dictionary
  *including itself*, i.e. per-def marginal consumption is nowhere
  near 10x).
- Watch note: def sizes are additive-linear in cross-references
  (every reference inlines the referenced compiled tree), so the
  blowup risk concentrates in deep chains (int-* over mag-* over
  bool/* + list-fold). It has not materialized at L3 depth.

## F6 — NUMERIC FUEL (sabra stdlib)

> borg/stdlib.borg pre-registration: 64-bit int-add exceeds 100k steps
> under v0, or trips the compile cap on seeding. THEN: a follow-up
> chapter adds a journaled math prim family and default arithmetic
> moves to prims, while invariant checks stay pure.

**Status: OBSERVED 2026-10-03** (tools/stdprobe/fuelprobe.exe; the
chapter's fuel-table law 3 carries both engines' numbers here).

The defining condition fired at EVERY measured size, not just 64-bit:
even the 8-bit add is 2.5x over the 100k line. Measured runtime steps
(lambda-wrapped programs, runtime-supplied operands, all-ones
magnitudes = worst-case carry for add):

| measurement      | v0 steps        | v1 (sharing) steps |
|------------------|-----------------|--------------------|
| tree-case leaf   | 1               | 1                  |
| tree-case stem   | 2               | 2                  |
| tree-case fork   | 3               | 3                  |
| list-fold 10     | 1,431           | 295                |
| list-fold 100    | 13,221          | 2,095              |
| int-add 8-bit    | 249,749         | 3,974              |
| int-add 32-bit   | 2,941,349       | 12,206             |
| int-add 64-bit   | 11,106,917      | 23,182             |
| int-mul 8-bit    | >18.4M (60-90s deadline; 1e8 fuel never needed) | 203,268 |
| int-mul 16-bit   | >18.4M (same)   | >1.1M (600s deadline inline; throughput collapse) |

- Trigger wording was "64-bit int-add > 100k v0 steps": fired, with
  the margin the chapter asked to know (11.1M, 111x the line).
  compile-side is unaffected: seeding (compile-IS reduction) eats
  these costs at def time without breathing on the 1e8 compile cap -
  the breaker line "or trips the compile cap on seeding" did NOT fire.
- v1 sharing is the counterweight the table law asks to carry
  alongside: 64-bit add fits comfortably at 23k steps; 8-bit mul
  203k. But sharing is NOT a rescue: 16-bit mul under v1 died on a
  600s wall clock having passed only 1.1M steps (memo throughput
  collapses on this workload; steps with large unique (fun,arg) keys
  dominate). Arithmetic at v1-useful sizes is bounded but dear.
- Steps-per-bit under v0 is ~173k for int-add 64-bit - the inlined
  per-position machinery (tree-case dispatch + bool gates + rec-fix
  unfolds at ~7.7k compiled nodes per consumer) prices every bit
  position in four-to-five-figure steps. This is the honest cost of
  intensional sign-magnitude arithmetic through dictionary defs;
  exactly the regime the pre-registration pre-decided prims for.
- Consequence per the pre-registration: a follow-up chapter adds a
  journaled math prim family (math/add, math/cmp, ...) and default
  arithmetic moves to prims; the stdlib defs stay as the pure
  reference (and as the replay-recomputed CHECK layer where the
  review wants that). The dictionary-definitions-alone thesis is
  REFUTED for arithmetic-scale numeric work and UNAFFECTED for the
  structural vocabulary (bools/lists/tree-eq/cmp-free layers all
  price in the hundreds-to-thousands of steps - usable).

## F7 — PRIM DRIFT (math-prims)

> borg/math-prims.borg pre-registration: the host mirror and the sabra
> reference can diverge silently. TRIGGER: any differential vector
> disagrees post-landing.

**Status: not triggered** (2026-10-03). 12 landed cells in
tests/math_prim_tests.ml (30 small vectors in-test against the live
reference; 64-bit goldens from tests/math_prim_golden.ml; junk-corner
probes for the recorded passthrough split) all agree prim ==
reference on the make.  Future drift is visible as a test failure.

## F8 — PRIM CREEP (math-prims)

> borg/math-prims.borg pre-registration: arithmetic working invites
> everything else landing as prims instead of conventions. TRIGGER:
> any prim whose answer a pure def could produce under 10k v0 steps
> at probe scale without a named workload under it.

**Status: not triggered** (2026-10-03). The family is exactly the
five F6-named members (math/add, math/sub, math/mul, math/cmp,
math/neg); the ledger invariant stays pure (verify-8 [8.5]); no
further prim has been proposed through any channel.

## F9 — SUPERLATIVE OVERREACH (forensics)

> borg/forensics.borg pre-registration: the 2026-10-02 review
> hypothesis called sabra code with the stdlib vocabulary
> "EXTREMELY debuggable" relative to conventional stacks. TRIGGER:
> any frozen localisation/collaboration/counterfactual gate (verify-10
> gates 1–5) misses.

**Status: not triggered** (2026-10-04). The deliberate-fault corpus
landed (15 faults: 8 class-1 structural, 3 class-2 semantic, 2 class-3
world-via-journal, 2 class-4 divergence) and
`scripts/verify-10-forensics.sh` measured every gate at or above its
frozen threshold — class-1 first-diff 8/8 = 100.0% exact mutation path
(>=90), class-2 3/3 = 100.0% def boundary with a provenance-named span
(>=80), class-1/2 max 2 API queries (<=8), class-3 max 1 (<=12),
two transcript runs agree on every answer exactly, class-3 2/2 forks
reach a different terminal status with both records replay-verified,
class-4 2/2 close as `loop` with a named (fun, arg) closure pair, the
count-preservation counterexample is pinned. The background pitch
language keeps EXTREMELY; if any future gate miss is observed, this is
the entry that records it (drop the superlative, keep "a debugging
discipline over hash-addressed artifacts — receipts all the way
down"), and the artifacts stay load-bearing either way — the same
F5/F6 regime: MEASURE, never massage.

The inverse overreach is pre-failed too: gates passing scope the claim
to stdlib-v1-scale programs and agent debuggers, structural evidence
only — a blast radius out to "all agent work" is a future chapter with
its own corpus, never this one's marketing. Measured numbers live in
the corpus README (`scripts/forensics-corpus/README.md`) and the
chapter agent note.

---

## F12 — RIM REGRESSION (rim-eio)

> borg/rim-eio.borg pre-registration (authored as "F8", renumbered
> F12 — F8 is PRIM CREEP; F10/F11 are accounts): TRIGGER: the eio rim
> cannot reproduce the run-boundary clock policies (deadline/compile/
> fork caps), OR the battery goes red in a way that is not a straight
> port artifact, OR stage cost balloons beyond the playbook estimate
> without a named cause.

**Status: not triggered** (2026-10-02). The Lwt -> eio migration
landed in place on branch `rim-eio` (commit series from `57d0dc5`):
direct-style eio driver over `Tuna_interp.Flat` (prim suspension as a
value; `TUNA_RUN_MAX_SECONDS` became the loop's own cancellation
discipline), `store/lib/pgx_eio.ml` (pure pgx over Eio.Net) + a
`Direct` identity monad replacing Lwt coloring, and `server/lib/web.ml`
(httpun_eio) replacing Dream. Lwt/Dream departed store/ and server/
dune deps; `main.ml` is an `Eio_main` mainloop. Battery on the isolated
eio instance: `dune runtest` green, differential 77/77 (24/24 fixture
subset included), smoke-api 14/14, smoke-ui 17/17, smoke-public 12/12,
verify-1..11 11/11 — every step count unchanged. Core untouched
(interpreter/, compiler/, common/ diff-empty vs master). Clock pins
reproduced exactly: run cap -> status `deadline_exceeded` at the
4096-firing poll with the row unverified; compile cap -> HTTP 400
"compile failed: ... exceeded the wall-clock budget", never a pinner.
The Lwt rim remains a legal labeled stage per the purity thesis had
this triggered; it did not, so the swap stands. No bridge, no flag.

---

## F13 — SUGAR SEAM BREACH (dialect)

> borg/dialect.borg pre-registration: reader-level sugar is sold as a
> total fold into the existing grammar. TRIGGER: any v0.1 form cannot
> desugar into the reader's existing grammar; or a desugar-equality row
> (12.1) diverges on tree or steps; or any form pushes a change into
> Ir.ml, bracket.ml or interpreter/; or desugar multiplies compiled
> size against its hand-written twin (law L6).

**Status: not triggered** (2026-10-02, borg/dialect.borg make). The
reader fold held on every shipped form: 6/6 live desugar pairs compile
to identical ternary + compile steps, and the three-engine corpus
(`scripts/diff-corpus/dialect_*`) agrees (86/86). The compiler delta
since the dialect baseline is exactly `compiler/lib/sexp.ml`; the
interpreter is byte-identical; the one core-side addition is
`common/lib/int_enc.ml` (the law-5 codec the reader and tests share,
beside `Tuna.Cstr`) — an additive codec, not a change to the existing
grammar's semantics. No form needed an `Ir.ml`/`bracket.ml`/
`interpreter/` change. `letrec` stayed GATED OFF: the port did not trip
on the rec-fix idiom, so it never shipped.

---

## F14 — WRITABILITY CEILING (dialect)

> borg/dialect.borg pre-registration: the chapter's thesis is that
> construction sugar + the record vocabulary buys package-scale
> writability — the exact judgement the failed todo probe made ("the
> language is not mature enough") is the thing to be measured, not
> remembered. TRIGGER: the todo bridge cannot close at 512 items under
> the pinned budget (fuel 1e7, default size cap) or blows
> TUNA_RUN_MAX_SECONDS into deadline_exceeded; or the compiled bridge
> program exceeds 100k nodes (the F5 wire, carried from defs to
> programs); or the port cannot be completed without a prim family
> outside the pinned set (tree/*, value/*, math/*).

**Status: OBSERVED** (2026-10-02, borg/dialect.borg make). The
todo bridge (`scripts/dialect/todo-board.sabra`) closes at 4/16/64
items under fuel 1e7 / size_cap 1e5, with step counts 71,833 /
287,189 / 1,148,613 respectively — but at 256 and 512 items the prim
`tree/list` hits `list_cap` = 256 entries and `Prims.payload_cap` =
65,536 ternary bytes, answers the journaled error "result exceeds the
journal payload cap", and the fold degenerates to an empty board
(128 steps, 1 journal row, normal status). So the ceiling is a HOST
pagination gap, not a reader-sugar gap: the ITEM LOGIC is written and
verified (acceptance 12.5: 2 open / 1 done, journaled, replay
verified), the WHOLE-BOARD read at unbounded size is not. The same run
shows superlinear step cost (the record folds: rec-get 2,158/2,321,
rec-upd 4,683 v0 steps per the 12.4 table). Per the THEN clause, this
is an evidence-named follow-up: a windowed/paginated tree read (or a
board sized to the cap) plus a record-fold cost pass — a VOCABULARY
pass, not a syntax pass. The todo-probe verdict is thereby corrected:
the language could carry the item logic all along; the missing piece
was board-scale pagination.

---

## NON-F — tree/list PREFIX VALIDATION (dialect make, fixed inline)

Not an F-class failure (an F-number is a PRE-REGISTERED failure; this
was neither pre-registered nor left standing). Found while taking a
granted namespace prefix into the todo bridge: the `tree/list` PRIM
validated its `prefix` argument with `validate_path` (which rejects a
trailing `/`), while every other prefix surface — the HTTP
`/api/tree/list` handler and a grant's `path_prefix` — uses
`validate_prefix` (which permits it). So `(prim "tree/list" "todo-cal\/")
was a journaled error even though the same prefix is the natural
grant shape and works over HTTP. Fixed in `server/lib/tree_prims.ml`
(`validate_prefix`), so the prim now matches the surface. No schema,
no contract bump: the accepted-input set widened, the answer shape is
unchanged. Recorded here because it is the kind of cross-surface
inconsistency the book wants named, not because it moved any
pre-registered number.

---

## RUNTIME PURITY THESIS — the pure-step core (not an F-class failure)

This is not one of F1–F4; it is the operator thesis itself (tuna.borg
docstring; `.ralph/plan.md` Open Questions), and it was pre-registered
that *either* outcome is a finding of equal validity. The instrument is
`Tuna_interp.Flat` (interpreter/lib/flat.ml, borg/purity.borg): a pure
explicit-stack (CEK-style) core whose state is a value, with no monad
parameter and no Lwt/scheduler types anywhere under common/,
interpreter/, compiler/.

**Observed 2026-10-01: the pure-step core is viable.** Evidence:

- **Step-count identity (the non-negotiable invariant).**
  tests/flat_tests.ml runs BOTH engines — the flat machine and the
  recursive `Prim_eval.Make` — on every `scripts/diff-corpus` entry and
  requires the (status, result-ternary, steps) triple to agree exactly,
  under Canonical (v0) AND Sharing (v1). 8/8 green. The flat machine is
  an INDEPENDENT re-implementation of the counting law, so this is
  agreement between two implementations, not a tautology. The corpus
  referee is exactly the instrument the thesis designated.
- **Measured cost (the criterion is measured cost on equal step counts,
  not aesthetics).** tools/flatbench on the two workloads the thesis
  names (small-prim depths 100/500/2000; prim-heavy n =
  1000/5000/20000): flat/rec wall-time ratios 0.47–1.24, IDENTICAL
  step counts throughout. The pre-registered asymmetry (rim-handler
  O(depth) tax vs flat constant-per-step) was to be measured, not
  pre-judged; observed is no measured step-cost premium, slight flat
  advantage at the largest prim-heavy size (0.89) and in v1 (0.47).
- **Suspension = a value (the property the shape was chosen for).** A
  prim call parks the machine in `pending_ : (site * name * args)
  option`; the driver answers out of band with `answer`; `run ~host` is
  sugar over `step`/`answer` for a synchronous host. Tested: a
  scheduler-free driver suspends, inspects, and resumes with no monad
  or callback in the core.

Consequence / scope, UPDATED 2026-10-01: the migration landed. The
server run boundary (`Run.execute`) and the replay/counterfactual
engine (`Replay.execute_fed`) now drive the pure core through
`Tuna_interp.Flat_drive.Make(Lwt)` — the monad lives at the rim, not in
the core — and the public pure API (`Tuna_interp.Eval`) delegates to
the flat driver via the identity monad. The recursive
`Prim_eval.Make` is KEPT as the corpus referee the tests cross-check
against. Full battery over the migrated boundary: pure suites, PG
suites (store/substrate/m11/fed/deriv), differential 24/24, smoke-api
14/14, smoke-ui 17/17, verify-1..7 7/7 (replay identity included).
No F-class failure was observed: the journal boundary did NOT leak
scheduler state back into the core (the thesis's pre-registered failure
condition); suspension being a value is what makes that so. The
monad-shaped `Prim_eval.Make` still exists as the reference engine, so
"no Lwt under interpreter/" is now true of the CORE (flat.ml) and the
public path (eval.ml -> flat_drive), with the recursive reference kept
intentionally.

## NON-F — v0.2 TODO PRELUDE PROBE: three standing v1 defects (found
## 2026-10-02; dispositioned 2026-10-04 — see DISPOSITION below)

Not F-class (neither pre-registered nor fixed in this batch). The v0.2
todo retry (`scripts/dialect/prelude_probe.py` + `todo-v02-*.sabra`)
drove the shipped prelude end to end and surfaced three defects in the
v1 layer the chapter's own probes had dodged. Repros are one REPL/run
each; all three reproduce on `list-map`/`list-append`/`first` shapes
with zero v0.2 sugar involved.

1. SILENT GRANT DENIAL mid-fold (machine/run layer). A prim whose grant
   is missing does not fail the run: `todo-open-count` over
   `tree/list` with grants `[tree/list]` only (no `math/add`) returns
   `status: normal`, `verify_status: verified`, and a poisoned
   accumulator tree (a giant Stem-nest) instead of the count. The same
   program with `math/add` granted returns exactly 2. The board in
   12.5 masked this by granting both prims. Expected (verify-1's own
   law): a denied prim denies the RUN.

2. LIST-MAP MISAPPLIES DEF-SPLICED LAMBDA ARGS (stdlib/encoding).
   `(list-map todo-title xs)` yields a list of leaves; the SAME
   `todo-title` applied directly to the same record returns the title;
   an INLINE lambda bound the same way (list-filter's `pred`) works.
   The CL reference (`reference/.../tree-calculus.py`) reduces the
   seeded trees to the same wrong answer, so this is the frozen tree
   itself, not the v0 machine: `(pair (g h) acc)` under the compiled
   encodings does not behave as its source. The 12.5 open-query dodged
   it with LET-bound lambdas. list-map has no corpus row exercising a
   def argument (verify-8 law 2 gap).

3. LIST-APPEND IS reverse(xs) ++ ys (stdlib v1 def). `(list-append 0 5)`
   returns bare `5`; `(list-append [1 2] 3)` scrambles to `[2 1 3]`;
   only single-element prefixes accidentally work. It survived because
   its only caller (rec-upd) builds assoc lists whose order lookup
   ignores. No corpus row.

Also pinned (not a defect): `list-fold` traverses right-to-left, so an
inline pair-prepend fold is ORDER-PRESERVING - the probe's open-query
relies on it and pins the exact board-order twin.

Probe consequence: `todo-v02-open.sabra` keeps its lambdas inline and
its def applications direct, and the probe grants `math/add` wherever
a prelude fold runs. All five probe stages green: create (one journaled
tree/put), keyed-literal board, named flip + read-back, exact open
query, live board re-run - every run replay-verified.

DISPOSITION 2026-10-04 (re-driven on the current build, all three
repros re-executed; evidence inline):

1. GRANT DENIAL - FIXED (0014 run-denial surfacing). Reproduced
   exactly: `math/add 1+1` with no grant returned status normal,
   verify verified, and the error tree as the result; the denial lived
   only in the journal. The LAW is unchanged - denial is data, the run
   continues per program semantics, acceptance 1 still pins status
   normal - what was missing was observability: the run row now
   carries `denial_count` (migration 0014, counted at the boundary
   where each denial is answered, persisted at finalize, surfaced in
   every run JSON). verify-1 extended (granted run must show 0; both
   denial shapes must show >= 1); prelude_probe asserts
   denial_count = 0 on every green stage. A denied prim now DENIES
   THE RUN in the only sense the book allows: on the record, not
   silently. Replay untouched (the denial count is a live-host fact,
   not a calculus fact).

2. LIST-MAP DEF-SPLICED ARGS - NOT REPRODUCING; the recorded repro was
   a mis-shaped input. `(list-map todo-title xs)` yielding "a list of
   leaves" is the CORRECT answer when xs is a FLAT KV LIST: a flat
   kv-list IS ONE multi-field record, so mapping the accessor over it
   reads a nonexistent key per element and answers leaf - and the same
   flat list applied DIRECTLY answers the first matching field (the
   "same todo-title works directly" half of the repro). With PROPER
   record lists (r = [kv], xs = [r1 r2]) the current build answers the
   titles on every engine: fresh-compiled stdprobe (`210200` = the
   titles, 0 run steps - compile-time reduction agrees), the live REPL
   (same ternary), and `eval-compiled` over the FROZEN trees (same).
   The frozen tree equals the fresh compile (list-map 340 bytes, hash
   b0bd1e19... = manifest = DB row). Inline-lambda and eta-blocked
   variants agree too. The engine pair (compile-time to_tagged, run
   Flat) was diffed arm-by-arm against the normative apply during the
   hunt: identical. Closing the gap that let the mis-read happen:
   corpus rows exercising a def-spliced lambda argument under list-map
   (verify-8 law 2) land with this disposition.

3. LIST-APPEND - NOT REPRODUCING; the recorded repros are foldr
   semantics on non-list arguments. `append 0 x = x` is foldr over the
   EMPTY list returning ys verbatim (the caller passed a bare element
   where a list was required); `[1 2] ++ 3` = `pair 1 (pair 2 3)` is
   ORDER-PRESERVING (the tail is the bare element, malformed as a list
   by the caller's own hand). With proper list arguments the current
   build appends in order: live REPL `[t f] ++ [t] -> 210202100`
   ([t f t]); stdprobe and frozen-tree eval agree. The def is the
   correct foldr-append; rec-upd's assoc-list caller is unaffected.

4. Pinned while re-driving the probe: the BOARD stage mis-verifies on
   a DIRTY namespace, and that is tree/list working as documented.
   Prefix selection is a path RANGE (`path >= prefix AND path <
   prefix || chr(255)`), so `tree/list "todo-cal-v02"` also returns
   the sibling namespaces `todo-cal-v02b/c/d/e` left in the dev DB by
   earlier probe runs - 20 entries, 11 open - and the fold answers 11
   and 20, CORRECTLY for what the prim returned (journal shows every
   math/add exact). The chapter's own note ("a plain path that still
   selects the todo-cal/* slice") is this behavior. Probe re-run on a
   fresh namespace (todo-cal-v02f): all five stages green, replay
   verified. A namespace-hygiene or exact-child list mode remains an
   evidence-named follow-up, not a defect.

---
