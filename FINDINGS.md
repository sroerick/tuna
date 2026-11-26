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
   a DIRTY namespace, and that tree/list working as documented.
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

## NON-F — a gate-FIRING corpus row cannot ride the differential
## harness (found 2026-10-04, board make; dispositioned inline)

The first draft of the stdlib_todo-add exercised row (board.borg
vocabulary row 1) applied the def - `(pair (todo-add w "hello board"
"sabralib" 1727000000) w)` - which places a PRIM GATE in the APPLYING
term. Observed on regen 2026-10-04: refeval and the CL twin AGREE with
each other (status normal, 139 steps, the gate reduces as plain data
under the verbatim rules), while the tuna CLI lane answers the prim
through its default host (`Error "no prim host at this boundary"`),
substitutes the canonical error tree, and finalizes differently (123
steps). NOT an engine defect - the flat machine and the recursive
referee step-count-identically; the disagreement is the HARNESS
boundary: refeval and the CL twin have no prim interception at all
(a gate carrier is just a reducible tree to them), while every tuna
engine parks on `Tuna.Cprim.shape` and demands a host. A corpus row
that FIRES a gate therefore has no single expected answer - the
"expected" line pins one host's fiction over another. Disposition:
the row was re-authored inert (data position: `(pair todo-add w)` -
the gate is embedded but never applied) and 93/93 agree; the
journaled RUNTIME proof of todo-add (steps, exact result twin,
replay) lives on the board acceptance (13.1/13.3) where the host
is real. Confinement rule, recorded: prim-carrying defs exercise in
the corpus only via SPLICED-BUT-UNAPPLIED terms, never via firing
terms.

## NON-F — SCOPED GRANTS COULD NOT RIDE RUN SUBMISSION (found
## 2026-10-04, board make; fixed inline)

Found while driving board 13.5 (the forged-path denial): a route
record WITH grant_prefix 500s on every hit ("program did not run:
grant ... does not cover this call's paths"). REPRO: publish a route
whose program fires tree/put inside its own prefix, minted grant is
prim "*" + path_prefix (serve_program's per-request mint, M11 law);
hit it -> 500. CAUSE: execute_run's submission check calls
S.check_grant with NO paths (default []), and check_grant's
empty-paths arm answers Prefix_denied for ANY path-scoped grant -
liveness/ownership validated fine, but the scoped rows could never
pass an argument-free check. BLAST RADIUS: every grant_prefix route
record (never exercised green - the M11 suite's program-route test
uses an unscoped record), AND any member-garden run submitted with
scoped grants via /api/runs - the platform's own garden model was
un-runnable through the public run surface. FIX (run.ml:510): at
submission, Prefix_denied is ACCEPTED - the empty-path shape simply
has nothing to check there; the per-prim live check at the boundary
(grant_paths) remains the sole path-scoping authority, exactly as
grants law 7 pins ("the row is checked live"). Pinned by
tests/m11_tests.ml test_program_route_scoped (pre-fix: the 500;
post-fix: 200 + the effect inside the prefix + law-4 finish). The
board chapter's forged-path acceptance (13.5) rides the same fix:
a scoped grant on a run whose input path escapes the prefix now
proceeds to the prim boundary and is denied THERE, as a journaled
error answer with denial_count >= 1 - never a silent pass and never
a submission refusal either.

---

## NON-F — HEALTH-GATE AUTOPSY, fe8515e (g5, the run-primitive health
## mystery)

fe8515e is a PRICKLYPEAR commit (merge of wyo/master, 2026-10-15
03:29Z, bringing libs/tuna FED3 PP-half v0 plus a lisp/lib/env.ml
change), not a tuna commit — the gate it is blamed against lives in
PP's openbsd deploy machinery.  What the autopsy found:

1. THE GATE CLASS.  `pricklypear-promote.ksh` gates a flip on
   `http://127.0.0.1:18080/health`; `pricklypear-watchdog.ksh` liveness
   is rc_check/health-only ("a healthy listener IS a healthy service",
   2026-08-21 lesson) with a documented ghost-listener failure mode:
   "health was measured against a ghost" (2026-09-03) — a surviving
   orphan answers /health while the pidfile names no live pid, which is
   why the watchdog requires the pidfile cross-check before declaring
   success.

2. THE LOCALLY CONFIRMED INSTANCE (this host, 2026-10-16): a stale
   glochid `bin/main.exe --serve --port 18090` (pid 950, started
   outside dev.sh, no pidfile) held :18090 — the port every tuna dev
   tool defaults to — and answered 404 `no route /health`, while
   tuna's dev server had silently drifted to :18091.  Any health gate
   or probe pointed at :18090/health measured the ghost: a hard 404
   against a healthy (or absent) tuna, or — the dangerous direction —
   a healthy answer from the wrong process.

3. THE CLASS IS PRE-REGISTERED ON THE PP SIDE TOO: the D4 recon's
   resolved mystery ("(schedules) -> [] ... the probe hit town (wrong
   instance)") is the same shape — a probe measured an instance that
   was not the one under test.

DISPOSITION: not a tuna defect; a probe-ownership defect.  The settle
check on any host where fe8515e's gate is in question is the
watchdog's own rule applied at the gate: confirm the pid answering
:PORT/health is the pid the service's pidfile (or the just-promoted
release) names before believing either verdict.  Tuna-side hygiene
recorded: dev.sh's pidfile discipline is correct, but nothing kills an
out-of-band squatter — the port-squat was only found because /health
404ed with a foreign error page.

---

## F15 — STAGING DOES NOT PAY ON ITS OWN (eval-modes EM1)

> borg/eval-modes.borg pre-registration (EM1): on the target page,
> request-path v0 steps of the staged template exceed 1% of the live
> (unstaged) render.

**Status: OBSERVED 2026-11-25** (eval-modes E1 make). The page is a
`<ul>` of todo items through the staged template
`<li>{2} <small>by {3}</small></li>` (stdlib v1.3 `tmpl-compile`,
`tmpl-each`). Live = the template supplied at request time; staged =
`(page-fn "<template>")` specialised at compile time; same output
bytes in every case.

| todos | live steps | staged request steps | ratio | parse moved to compile |
|---|---|---|---|---|
| 2 | 1,367,842 | 538,319 | 39% | 829,525 |
| 20 | 6,456,586 | 5,627,063 | 87% | 829,525 |

What staging DID: the whole parse (829,525 steps) left the request
path, exactly as the chapter's 2026-11-25 int-mul experiment
predicted (13.47M -> 7,029 there). What it could not do: the residual
is data-proportional work - about 270k steps per item, roughly 10k per
escaped data character (five in-calculus `tree-eq` compares per char
in `html-esc-char`, plus `list-append` per segment). No binding-time
rearrangement removes work that depends on the request's data.

A second observation, not a failure definition: a renderer written
naively (parse interleaved with the data walk) and then specialised at
compile time is 1.7x-3.6x SLOWER than the live staged library - the
compiler lifts closed subterms but does not stage an interleaved
recursion. Staging stays a property of how the vocabulary is written.

CONSEQUENCE (recorded, as the chapter planned): E2 accelerators are the
answer to the residual, not more staging. The first set the chapter
names (tree-eq, list-append, int-*) covers exactly the per-char cost
measured here; html-escape is added as a candidate. EM1 stands as
observed; the 1% line is not retuned.

---

## F16 — ACCELERATORS DO NOT PAY FOR THE GATE VERDICT (eval-modes EM3, half)

> borg/eval-modes.borg pre-registration (EM3): the gate verdict and the
> naive renderer under fast mode are not at least 10x faster in wall
> clock than v0.

**Status: OBSERVED for the verdict half, 2026-11-25** (eval-modes E2
make; registry fast1, nine accelerators). Same answers in every row;
wall clock on this laptop, single run each.

| workload | v0 | fast1 | wall |
|---|---|---|---|
| verdict.sabra (helpers inline) | 67,192 steps, 0.566s | 67,192 steps, 0 native | 1.0x |
| verdict-std.sabra (stdlib names) | 55,195 steps, 0.099s | 18,798 steps, 65 native | 2.5x |
| todo page, staged, 20 items | 5,627,063 steps, 11.1s | 133,139 steps, 261 native | 46.7x |
| todo page, live, 20 items | 6,456,586 steps, 12.7s | 161,314 steps, 668 native | 31.4x |
| int-mul experiment (naive renderer) | 13,465,953 steps, 24.6s | 12,431 steps, 17 native | 460x |

FAST2 UPDATE (same day, registry fast2: + rec-get/val/has, list-fold/
map/filter, compile-time partials; best of 3, machine under load so
ratios are the comparable figure):

| workload | v0 steps | fast2 steps | wall vs v0 |
|---|---|---|---|
| verdict.sabra (inline) | 67,192 | 67,192 (0 native) | 0.9-1.0x |
| verdict-std.sabra | 55,195 | 1,848 (11 native) | 6.7-8.0x |
| todo page, staged, 20 | 5,627,063 | 711 (203 native) | 480-670x |
| int-mul experiment | 13,465,953 | 5,655 (34 native) | 580x |

Still OBSERVED for the verdict half. verdict-std now takes 30x fewer
steps than v0 but wall clock is held by two fixed costs: the fast2
start walk (~13ms on 13.4k program+input nodes once the registry is
cached; 37ms on first use) and the per-step O(size) check in
deliver_checked (the ASIDE below), ~11us/step on a 3.5k-node
observation. The inline verdict cannot be accelerated (no registered
hash inside) and pays the start walk for nothing.

SIZE-CHECK FIX (2026-11-25, after the owner rescoped stdlib acceptance
6): deliver_checked now reads subtree sizes from a direct-mapped,
weakly keyed cache (interpreter/lib/flat.ml, cached_size) instead of
walking the whole new value on every Stem/Fork build. Size is a pure
function of the tree, so answers, step counts and the cap decision are
unchanged (runtest, accel-full, differential 103/103, Jay's 30 proved
vectors all identical). A chained physical-identity table was tried
first and lost: Jay's checker makes thousands of distinct copies of
EQUAL trees, which share a hash, so buckets reached ~6k. Effect on
Jay's checker: 100-130us/step -> ~1us/step (infer typing itself
668s -> 3.4s; an expression typing 160s -> 1.6s); whole runtest
~9 min -> 36s.

The renderer half clears 10x by one to two orders of magnitude; the
verdict half does not. Two causes, both structural:
1. verdict.sabra carries its helpers INLINE (let-bound), so no subtree
   carries a registered hash and nothing is accelerated - the
   tool-language T2 finding (dictionary literals and let-bound helpers
   are different trees) now has a price. verdict-std.sabra, which
   pins stdlib names, is accelerated.
2. What remains in verdict-std is record lookup and list walking
   (rec-get, list-fold, list-filter), which fast1 does not cover: the
   higher-order and record families are the chapter's second set.

ASIDE (not a failure definition): v0 costs ~8us per step on
verdict.sabra but ~1.8us on verdict-std.sabra. Flat.deliver_checked
computes Tree.size - O(n) - on every delivered value, so per-step cost
grows with value size. Pre-existing; recorded, not changed here.

---

## F17 — V1 INT CODE FORGES ONE OF JAY'S LABELS (typing TY-F4)

> borg/typing.borg pre-registration (TY-F4): a v1 stdlib def, corpus
> program or compiler output is not concrete in Jay's sense (contains
> a label-shaped subtree S2[K, K1 n] where no tag was meant).

**Status: OBSERVED, 2026-11-25** (typing TY1, the first run of Jay's
own `is_concrete` over tuna's trees).

Jay's `is_concrete` (vendored from his development, checked against his
spec's vectors) answers KI for five frozen stdlib v1 rows: `mag-cmp-fn`,
`mag-cmp`, `int-add`, `int-sub`, `int-cmp` (and their five corpus
entries); every other v1 def and every other corpus program is concrete.
The offending subtree, found by a shape scan of `mag-cmp-fn` (three
copies): `Fork(Stem(Stem Leaf), Fork(Leaf, Stem Leaf))` - in v1 terms a
data constant pairing the compare result `%110` with `K %10`; in Jay's
terms exactly `label[K]`, the name of an abstract type.

Why it matters: Jay's type application treats a label as an abstract
type and refuses what the ontology does not declare, so any program
calling v1 int-add / int-sub / int-cmp cannot be typed. It is the
representation question of the typing chapter in miniature: untagged
v1 data can collide with the encoding his types use.

Not fixed in v1: rows are immutable (stdlib.borg), and a changed
`mag-cmp-fn` would move five hashes plus the fast1/fast2 accelerators
keyed on them. The fix belongs to stdlib v2 (typing TY5), whose defs
must all pass `is_concrete`. tests/jay_tests.ml pins the non-concrete
set to exactly these names, so a new forged label - or one of these
disappearing - fails the test.

---

## F18 — THE JAY-NATIVE BINARY INT IS SOUND BUT ~45x BIGGER (typing TY-F1, first encoding)

> borg/typing.borg pre-registration (TY-F1): the tagged representation
> costs more than 2x v1 on the int workloads.

**Status: OBSERVED for the first encoding, 2026-11-25** (typing TY2
spike). Arithmetic on it is not written; the size and typing cost
alone decide this encoding.

Encoding: `Int = wrap[int_nm, sign * magnitude]`, payload type
`Bool * List Bool` - v1's shape (sign, LSB-first bits) rebuilt from
Jay's tagged Bool and tagged List cells, declared with his `D_wrap`.
`reference/jay-typing-rules/export/TyInt.v` defines it inside his
development and proves `O_int_sound` / `O_int_typing_sound` by his
`wrap_decl_sound` (Print Assumptions: closed under the global context).
Nine proved Examples (five, -6, zero, a 21-bit value : Int; a bad
payload refused; sign and magnitude projections typed Bool and
List Bool; sign evaluation) all agree on tuna's v0 engine.

Cost, measured in tuna:

| value | tagged Int nodes | v1 nodes | typing steps |
|---|---|---|---|
| 0 | 176 | ~3 | 58,693 |
| 5 | 476 | ~9 | 192,993 |
| 1,048,579 (21 bits) | 2,342 | ~55 | 1,022,773 (0.8s) |

~107 nodes and ~45k typing steps per bit: every bit is a tagged Bool
plus a tagged list cell, and every list cell carries the code of its
element type. Soundness comes free from his framework; the price is
the representation.

Next encodings to measure (owner decision which): a dedicated binary
type declared like his Nat with two successors (one tag per bit, its
own soundness proof modelled on `nat_interp_sound`), or one tag around
a v1 carrier with a declared table of trusted operations (smallest,
but its soundness rests on proofs about v1 arithmetic).

SECOND ENCODING (owner chose the dedicated binary type, same day):
`export/TyBin.v` declares `Bin` like his Nat with two successors -
`bzero`, `beven[m]` = 2m, `bodd[m]` = 2m+1, consumed as `B * (E * O)` -
one tag per bit under the 3-node name `Fk Lf Lf`, and `Int` as his
wrapped type over `Bool * Bin`. OUR PROOF: `bin_interp_sound` (the Bin
declaration is sound by values, for every ontology) after his
`nat_interp_sound`, via `bin_guard_spec`, `bin_eliminator_inv` and a
generalised `carrier_sel_inv`; then `O_bin_sound` / `O_bin_typing_sound`
(Print Assumptions: closed under the global context). Ten proved
Examples agree on tuna.

| value | wrapped Int (1st) | Bin-based Int (2nd) | v1 |
|---|---|---|---|
| 5 | 476 nodes / 193k typing steps | 261 / 105k | ~9 |
| 1,048,579 (21 bits) | 2,342 / 1.02M | 1,185 / 507k | ~55 |

About half the first encoding; still ~20x v1 and ~23k typing steps per
bit, so TY-F1 stays observed. The remaining overhead is fixed per bit
and comes from his pair/selector encoding (`fst` 11 nodes, `snd` 16,
the nested selector `fst 'C snd` 31). Arithmetic on Bin is not yet
written; typing at ~1us/step costs ~0.5s for a 21-bit input, too slow
for a request path until his checker is accelerated.

---

## F19 — JAY'S CHECKER UNDER fast3: ~150x FEWER STEPS, NOT ON THE TIGHT LAW (eval-modes E3)

Date: 2026-11-25. Chapter: borg/eval-modes.borg E3; typing.borg TY2/TY3.

EXACT natives first (E2's law: answer exactly what the tree answers,
or decline): his helpers `equal`, `is_concrete`, `get_tagged`,
`get_label`, `row_prog`, and `infer` on concrete trees (his
`concrete_is_self_typed`) all agree with his spec lemmas (222/222
vectors) but cut the 21-bit Bin from 482k typing steps to 326k (~1.5x).
WHY: his checker is bracket-compiled, so it works at partial
application (`equal @ p` builds `equal1 p`; `tp @ F` dispatches on F)
and evaluates results it discards - his own `rows_f_hit` needs the
fallback `fam @ n` to terminate even on a hit, so every ontology lookup
(~3.2k steps) walks every row. A native entered at full arity sees
none of that work.

OWNER DECISION (option B): fast3 natives follow his clause-by-clause
characterisations (typing.v `TA` / `INF`, rows.v `row_head`) and may
skip discarded work - same answer whenever the reference terminates,
decline on a prim-carrying ontology; v0, fast1, fast2 unchanged.
Mechanisms: families (template-keyed), continuation plans, wrapper
partials, and one EXACT closed form (`equal @ p` = `equal1 p`, his
`equal_eval`), which removed the last big cost (the Bin guard's three
`equal` partials: 11.8k -> 86 steps).

| vector | v0 steps / wall | fast3 steps / wall |
|---|---|---|
| 21-bit Bin | 482,048 / 0.38s | 3,229 (0.67%) / 0.077s (20%) |
| 21-bit Int | 506,616 / 0.38s | 5,714 (1.1%) / 0.084s (22%) |
| infer typing itself | 5,167,788 / 3.5s | 31,414 (0.61%) / 0.033s (0.9%) |
| typing list append | 3,162,749 / 3.1s | 158,019 (5.0%) / 0.47s (15%) |

EM5 (mismatch) NOT TRIGGERED: all 278 vendored vectors give his stated
result under fast3; generated cases on four of his ontologies (440
infer / tp cases, v0 finished on all) agree; 200 random `equal1 p`
equal v0's `equal @ p`; a prim-carrying ontology is declined; the
differential corpus answers identically under fast3. EM6 (does not
pay) OBSERVED FOR WALL CLOCK ONLY: steps are under 1% for Bin/Int, but
wall stays 15-22% on mid-size inputs because fast2's once-per-run
recognition walk over the 125k-node checker program (~0.06s) now
dominates; the natives' own run is ~0.012s. Fix candidates: cache the
prepared program per hash across runs (a typed tool reuses one checker
program), or a cheaper walk. Not done here.

## F20 — INDEPENDENT REVIEW OF E2/E3: TWO CAP ESCAPES AND A SKIPPED PRIM (2026-11-25)

Independent review (Fable, fresh context) of tuna/language-tooling before
it landed. Three findings about the accelerators, all reproduced:

1. SIZE CAP ESCAPE (fast2, fast3) - a value a native answered through a
   plan (`Build`) or a continuation (`Value` in `run_answer`) was handed
   to bare `deliver`, skipping the size check. list-map `\x. pair x x`
   over a 20-element list at cap 1600: v0 Size_exhausted, fast2 Normal
   with a 2,561-node result. fast3 `tp[O] @ triage[Lf, Sm G, Lf] @ Sm X`
   at cap 100,000: v0 Size_exhausted, fast3 Normal with 131,072 nodes.
   That is EM2 / EM5 as written. FIXED IN PLACE (owner decision: neither
   semantics had landed): both paths use `deliver_checked`
   (interpreter/lib/flat.ml). Tests: accel_tests "a plan's built value is
   size-capped like v0's (EM2)" and fast3_tests "a value a native builds
   is size-capped like v0's (EM5)" - both FAIL with the bare `deliver`
   restored, both pass with the fix.
2. A PRIM BUILT AT RUN TIME IN DISCARDED WORK IS SKIPPED (fast3) - the
   decline test is syntactic. A row whose `rest` builds `Fork(gate,
   callsite)` and applies it: on a hit, v0 evaluates `rest @ n` (rows are
   eager) and fires the prim; the native does not. Same answer, one
   journal row fewer. Law E3 (d) said "no effect is ever skipped"; the
   text was wrong, not the native (owner decision: correct the text, keep
   the syntactic check). Pinned by fast3_tests "law (d) as written".
   Does not touch TY3: O_bin is fixed and builds no prim calls.
3. TESTS COULD MISS A SILENT FALLBACK - the fast3 differential compared
   only Normal answers and never asserted that natives fired, which is
   how 1 and 2 got through. It now compares size-capped runs and asserts
   native calls; EM6 in steps is a test (ibig_Int: fast3 <= v0 / 10).
