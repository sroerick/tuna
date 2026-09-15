# Tuna - a standalone tree evaluator

**Status:** spec v0, implementation handoff. **Author:** Gregor (design conversation with roerick, 2026-09-14). **Implementer:** TBD. Name: Tuna (the fruit of the pricklypear). Sabra is the runner-up - roerick's call between the two; rename is trivial.

## 1. Purpose

Tuna is a small standalone evaluator for tree calculus (Barry Jay /
Johannes Bader; reference implementation olydis/tree-calculus). It is
Pricklypear's architecture pointed at a different seam: instead of
evaluating text against ambient habitat state, it runs content-addressed
trees under explicit grants and treats every run as a durable, replayable
record.

One sentence: a store-backed daemon where the unit of execution is a
hash-addressed tree, executed by five rewrite rules under a fuel budget,
with all effects at a journaled, capability-gated boundary.

## 2. Computational model

- Values: leaf, stem, fork (delta, delta-x, delta-xy). Programs and data
  are the same shape; there is no distinction to maintain.
- Semantics: the five triage reduction rules. Normative references:
  olydis/tree-calculus (implementation/ocaml/lib/tree.ml) and the validated
  Common Lisp reference at ~/tree-calc/tree-calc.lisp, which is faithful to
  the rules and counts steps.
- Determinism is a tested invariant, not a hope: same input tree, same
  result tree, same step count, every time.
- Bounded execution: every run carries fuel (max reduction steps) and a
  size cap (max live tree size). Exhaustion is a first-class normal
  result, never an exception, watchdog, or timeout.

## 3. Architecture

A single daemon over a durable store, in Pricklypear's shape: HTTP API in
front, Postgres behind. Embeddable as a library later (the Nopales
pattern); v0 is standalone.

- **Interpreter.** Pure, in-process, fuel- and size-bounded. The whole
  trusted core is a few hundred lines; keep it that way.
- **Store.** Content-addressed programs (hash of canonical serialization),
  run records, effect journals.
- **API.** Program create/fetch by hash; structural patch (CAS); run;
  journal and run queries.
- **Auth.** Tokens, every request attributable to an identity (PP's
  author-prim model).

## 4. The seam (where it differs from PP)

PP's seam is text: /api/eval parses a string and runs it with ambient
authority. Tuna's seam is structural:

1. **Programs are trees, addressed by content hash.** No reader exists on
   the execution path; hostile bytes never meet a parser.
2. **Runs are rows.** A run record: program hash, input trees, fuel, size
   cap, result tree, step count, status, journal reference, caller.
   Replay = re-execute; the recorded step count doubles as a free
   integrity check on both interpreter and data.
3. **Effects are boundary events only.** The calculus has no I/O. A run
   touches the world exclusively through primitive calls handed to it as
   explicit capability grants. Each call journals: call-site path in the
   program, prim, tree args, tree result or error, wall time.
4. **Grants are the security model.** A run can reference only what is
   physically passed to it - no name resolution, no globals, no ambient
   store access. The evaluator guarantees nothing else is reachable;
   scoping the grants correctly is host policy and stays a review matter.
5. **Program updates are structural patches with CAS.**
   Patch = {program hash, path, expected old-subtree hash, new subtree}.
   No string surgery, no text diff; concurrent edits collide on hash
   mismatch. Content hash + path + old-hash also makes "what changed
   between two versions" a cheap tree walk.

## 5. Front end and REPL

- Surface language: a small lambda-calculus-style s-expression source,
  compiled to trees by bracket abstraction with eta (as in ~/tree-calc and
  upstream tree_builder.ml). Compilation IS reduction: closed expressions
  already evaluate during compile.
- The named lambda IR is retained alongside the compiled artifact. Each IR
  node knows its source span and its compiled-tree path. Diagnostics
  address programs by tree path; source spans are a human courtesy only.
- Compile-time checks: unbound variables and arity errors reported with IR
  paths. A static type check on the IR is a later phase, not v1.
- REPL: define/eval/iterate against a name-to-tree dictionary, with
  structural commands (get, eval, patch by path; first-diff between
  versions). Because any closed subterm evaluates in isolation, the REPL
  doubles as a structural debugger. The transcript is itself a run row.
  Agents use the same primitives through the API instead of a prompt.

## 6. Validation (acceptance criteria)

1. **Differential harness:** reduction results AND step counts match the
   CL reference on a shared corpus; corpus includes upstream's published
   values (size(not) = 8; not true -> delta in 2 steps; etc.).
2. **Replay identity:** re-running any stored run reproduces the result
   tree and step count exactly; the store checks this.
3. **Boundedness:** a divergent program (omega) stops at exactly fuel with
   a normal status; no timeout machinery anywhere.
4. **Patch CAS:** wrong expected-hash patches rejected; correct ones apply
   atomically.
5. **Journal completeness:** for an effectful sample program, replaying
   the pure core over the recorded journal reproduces the effects.

## 7. Non-goals

- No effects inside the calculus; no closures, environments, or name
  resolution at runtime.
- No static types in v1. No machine-checked proofs in v1 (later, optional;
  upstream Rocq/Lean formalizations are the answer keys).
- Not a Nopales replacement. Nopales keeps running trusted code; Tuna
  is the untrusted, auditable tier. lib.pp does not migrate.
- Grant scoping correctness is not an evaluator guarantee; do not claim it
  in the docs.

## 8. Phases

- **v0:** interpreter, store, hash/patch/run API, REPL, differential
  harness (acceptance 1-3).
- **v1:** grants, journals, replay verification, path-tagged call sites
  (acceptance 4-5).
- **v2, optional:** HM type check on the IR; formal faithfulness proof;
  embedding as a PP library.

## 9. Open questions for the implementer

- Canonical serialization and hash stability across versions.
- Store choice for v0 (Postgres from day one vs sqlite-then-port).
- Grant token shape (row vs bearer-style token) and revocation.
- Whether the REPL ships in v0 or lands in v1.

