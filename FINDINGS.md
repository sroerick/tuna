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
