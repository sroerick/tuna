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

**Status: not triggered** (no optimizer exists in v0).

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
