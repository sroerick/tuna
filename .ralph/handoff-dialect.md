# HANDOFF — sabra dialect v0.1 (borg/dialect.borg, all statuses PLANNED)

Date: 2026-10-02. State: chapter authored and committed (`59569a3`),
NOTHING implemented. Work tree clean. This note is the code-facing
companion to the chapter — read `borg/dialect.borg` FIRST, it is the
law; this file is orientation + pin facts + the order of work.
`borge lint && borge report` green at handoff. NB: `borge fmt
--check` fails even on stdlib.borg — the repo is not fmt-canonical;
do NOT run `borge normalize` here, lint is the gate per AGENTS.md.

## 0. The mission

Make `borg/dialect.borg`: reader-level construction sugar (brackets,
number literals, let; letrec gated) + the record vocabulary (stdlib
v1.1 additive rows) + the TODO BRIDGE dogfood (the todo item logic
in sabra, against tree/* effects) + `scripts/verify-12-dialect.sh`
green over 12.1–12.6. Research bet, falsifiable: every sugar form
folds into the grammar the reader accepts TODAY — zero IR node
kinds, zero interpreter change; compiler delta = `sexp.ml` ONLY
plus one `common/` codec beside `Tuna.Cstr`. Violations are F13
(drop the form, never grow the core), record in FINDINGS.md.
F14 is the writability ceiling — the port must close at 512 items
under fuel 1e7 / default size cap, compiled tree under 100k nodes,
no prim family beyond tree/* value/* math/*. F13/F14 stubs already
sit in FINDINGS.md with their THEN clauses — do not pre-trigger
them; measure, never massage.

Why now: the stdlib made (verify-8), math prims made (verify-9),
the Forth freeze lifted; the owner's todo probe bounced ("language
not mature enough") and left no artifact, so the CHAPTER's own
bridge is the citation. Writability is measured at make time, not
remembered.

## 1. Facts pinned at handoff (verified by reading, not running)

READER — `compiler/lib/sexp.ml` is the entire grammar:
- tokens: LP / RP / Atom(off,text); delims in `is_delim`; `%` starts
  a ternary atom; quoted strings lex as single atoms with spaces
  allowed inside. CREATE LBR/RBR the same way LP/RP are
  made, but be careful: quoted-string scanning happens in the
  catch-all branch — brackets inside `"..."` must stay inert.
- `is_var_atom`: identifier chars are [-_a-zA-Z0-9], with all-digits
  and all-ternary-digit runs REJECTED (that's why bare `42` is a
  parse error today, "tree literals need %": the positive flip is
  purely additive). `-7` IS currently a legal var atom (`-` is an
  identifier char) — reserving `-<digits>` is the one real compat
  change; audit it (12.2): `rg -- '-[0-9]+' scripts/diff-corpus/
  stdlib/ tools/stdprobe/` and any stored program sources (rows in
  `programs` with IRs); `-`-digits-then-letters STAYS an identifier.
- reserved head atoms TODAY: lambda, runtime, prim (handled in
  `parse_term`). `let` joins them; `(runtime (prim ...))`'s
  K-threading pattern (parse_runtime) is the precedent for desugaring
  INSIDE parse_term — bracket/number/let desugar can land the same
  way: expand to existing IR nodes (App/Lam/Tree_lit) and re-use the
  form's span so IR paths stay identical to the hand twin (12.1
  pins THAT, not vibes).
- `builtin_tree`: pair/cons → Leaf literal. So `[a b c]` →
  `(pair a (pair b (pair c 0)))` is literally the hand twin today.
- literal 2 must tree-equal the stdlib's int-two: seed rows ARE the
  reference (`(int-add int-one int-one)`); the corpus row is the
  referee (differential-pinned like the math prims were).
- int law 5 (stdlib conventions): fork of bool sign + magnitude
  bool-list LSB-first, canonical high falses stripped, both zeros =
  `200` (fork false nil); `-0` must decode to exactly `%200`.

STDLIB PIPELINE (must stay the only path to seeded defs; no second
grammar ever): `tools/stdprobe/gen_defs.py` (author defs; emits
/tmp/stdlib_defs.json) → `freeze.ml` (stdlib/v1/core.defs +
stdlib/v1/manifest: name -> ternary size -> program hash) → dune
rule embeds (`embed.py`; server reads the generated module, never
core.defs at runtime). `server/lib/stdlib_seed.ml` boots sabralib
and replays records through the ORDINARY def round; seeding is
ADDITIVE skip-if-exists — v1.1 rows must be NEW names only (rec-*);
never touch the 53 existing rows' names. verify-8 re-runs over
53+n. NB `Repl_cmd`'s ternary fold conses — caller rows first in
the list if you touch assembly.

EFFECTS the bridge rides: prims `tree/get tree/put tree/cas
tree/list tree/del ns/fork` (server/lib/tree_prims.ml; paths
printable ASCII ≤ 512, no leading/trailing slash; the LIST prefix
arg allows trailing slash). Math prims `math/add sub mul cmp neg`
(server/lib/math_prims.ml; args = 2-list of law-5 ints, cmp answers
small nat 0/10/110). Prim calls in programs: `(prim "name" args)`
with closed constant args must be deferred via `(runtime ...)` or
the compile guard rejects — the bridge should take paths from
GRANTED ARGUMENTS anyway (grants law 7: paths arrive as args,
never resolved; the grant covers the prefix, the run row points
at it). Grants minted via the admin surface (`scripts/verify-1/3`
show the shape).

RUNS: POST /api/runs takes fuel (default 10_000) + size_cap; the
bridge budget is PINNED fuel 1e7 (12.4) at 4/64/512 items. Run
boundary wall clock `TUNA_RUN_MAX_SECONDS` default 10s — a slow
512-item fold could trip deadline_exceeded, which is exactly an
F14 arm; if you must, note the env in the verify script like
other chapters do (see scripts/verify-11).

CORPUS: `scripts/diff-corpus/` — add `dialect_*` rows; house
naming is `stdlib_*`. `verify-differential.sh` is three-engine
(both engines + CL reference) result+steps agreement. Reader unit
tests live in `tests/compiler_tests.ml`; property pins go in
`tools/stdprobe` (golden.ml / fuelprobe.exe style; 0 runtime steps
there = compile-IS-reduction).

NAMESPACES: the host page writes `todo/<epoch>-<hex>` (flat ids
under `todo/`); the bridge rides `todo-cal/` — disjoint from both
`todo/`'s page listing AND the routes.borg reserved-prefix law, so
no UI noise and no reserved-name interaction. The OCaml page is
untouched by this chapter.

## 2. Order of work (small commits, battery per AGENTS)

1. S0 reader sugar: LBR/RBR + number literal decode + the common/
   codec; desugar rows (12.1) + reader unit tests. Note letrec is
   GATED-OFF by default — ship only if the port trips on the
   rec-fix threading (record the citation in the chapter then).
2. `let` + its twin rows; compat audit greps (12.2) + full corpus
   re-hash (every existing corpus entry re-compiles to its recorded
   hash — verifies additivity).
3. Write the PORT (`todo-cal` bridge) in sabra. It names the record
   defs; only then add v1.1 rows (rec-get option-typed, rec-upd,
   rec-has + what the port actually reached for) via the gen
   pipeline; manifest bump; seeds additive.
4. Key-domain decision fuel row (tags vs cstr strings) — the
   numbers decide, ties to tags; record in 12.4 with both engines.
5. `scripts/verify-12-dialect.sh` (12.1–12.6) + fuel table rows +
   deriv-seal the bridge; flip chapter statuses + FINDINGS F13/F14
   statuses in the SAME commits as the evidence; borge lint/report;
   then the full battery: dune build+runtest, verify-differential,
   smoke-api, smoke-ui, verify-1..12.

## 3. Traps (read these twice)

- DO NOT grow Ir.ml / bracket.ml / interpreter/ — a single byte of
  movement there is F13 with a pre-written THEN. If a form truly
  cannot fold, drop the form and say so in the book.
- The desugar-equality law is checked, not trusted: hand-write the
  expansion twins and diff TREES + STEP COUNTS (all three engines),
  because a reader bug that silently produced a different tree
  would otherwise ship as a feature.
- Spans: desugared nodes inherit the form's span, so provenance
  and callsite paths (which are canonical in the compiled tree,
  AGENTS 8) do not move vs the hand twin.
- do not "improve" is_var_atom beyond the reservation — identifier
  grammar stays as-is (var atoms with digits-and-letters mixed
  keep working).
- skip-if-exists seeding means a LOCAL shadow of a rec-* name in a
  user identity is legal and wins (resolution order: param >
  identity > sabralib > builtins). That's the whole design; test
  it, don't forbid it.
