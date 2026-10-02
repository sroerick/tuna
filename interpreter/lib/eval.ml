(* Tuna interpreter: the triage rules + a fuel/size-bounded stepper.

   Normative source: reference/tree-calculus/implementation/ocaml/lib/tree.ml
   (`apply`, ported verbatim) and AGENTS.md domain rule 4:

   - A *step* is one firing of a triage rule — the Fork cases:
     fork(leaf,x), fork(stem,_), fork(fork,_). The two "wrapper"
     applications (`apply Leaf b = Stem b`, `apply (Stem a) b =
     Fork (a, b)`) are application, not steps, and consume no fuel.
   - Strategy is leftmost-innermost in exactly the order the OCaml
     reference evaluates `apply (apply a1 b) (apply a2 b)`: inner a1
     first, then a2, then the outer. Step totals are therefore an
     invariant under confluence.
   - Fuel (max rule firings) and size_cap (max live tree size) bound
     every run; exhaustion is a normal result, never an exception at
     the API boundary. The budget is exact: a divergent term like
     omega halts with Fuel_exhausted at exactly [fuel] steps.
   - A wall-clock deadline exists at the run boundary only (server env
     TUNA_RUN_MAX_SECONDS); pure evaluation passes none, so step
     counts stay an invariant of the calculus.

   Since M7 this module was the IDENTITY-monad instantiation of the
   monadic engine (Prim_eval.Make).  As of the RUNTIME PURITY THESIS
   migration (borg/purity.borg) it delegates to the PURE-STEP core
   (Flat) through the identity-monad driver (Flat_drive.Make(Id)): the
   core no longer carries a monad parameter, and this public API keeps
   its exact shape and step counts.  The recursive monadic engine
   (Prim_eval.Make) remains available as the CORPUS REFEREE the thesis
   designates, and tests/flat_tests.ml cross-checks the two. *)


module Id = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end

module Drive = Flat_drive.Make (Id)

type result = Prim_eval.result =
    Normal of Tuna.Tree.t * int
  | Loop of int
  | Fuel_exhausted of int
  | Size_exhausted of int
  | Deadline_exceeded of int

type mode = Prim_eval.mode = Canonical | Sharing

(* Same API and behavior as before.  [~mode:Sharing] opts into the
   distinct-work law (borg/sharing.borg).  [~trace] attaches a
   firing-event collector (borg/trace.borg): observation only. *)
let eval ?host ?deadline ?(mode = Canonical) ?trace ~fuel ~size_cap ~program args =
  Drive.eval ?host ?deadline ~mode ?trace ~fuel ~size_cap ~program args

(* Back-compat alias: callers that reached the identity-monad engine as
   [Eval.Engine] (sharing/trace test suites) get the flat driver, which
   has the same [eval]/[result]/[trace] API. *)
module Engine = Drive
