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

   Since M7 this module is the IDENTITY-monad instantiation of the
   monadic engine (Prim_eval.Make): one verbatim port of the rules
   serves both the pure evaluator and the journalling host boundary,
   so step counts and evaluation order cannot drift apart. The
   identity instantiation sequences exactly like the original lets,
   and the default host answers every (never occurring in a pure
   corpus) prim call with an error tree. *)



module Id = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end

module Pure_host = struct
  let prim ~site:_ ~name:_ ~args:_ : [ `Ok of Tuna.Tree.t | `Error of string ] Id.t =
    `Error "no prim host in pure evaluation (prim calls need a run boundary)"
end

module Engine = Prim_eval.Make (Id)

type result = Engine.result =
    Normal of Tuna.Tree.t * int
  | Loop of int
  | Fuel_exhausted of int
  | Size_exhausted of int
  | Deadline_exceeded of int

type mode = Engine.mode = Canonical | Sharing

(* Same API and behavior as the pre-M7 pure engine.  [~mode:Sharing]
   opts into the distinct-work law (borg/sharing.borg): memoized
   firings, distinct step counts, finite [Loop] divergence. *)
let eval ?host ?deadline ?(mode = Canonical) ~fuel ~size_cap ~program args =
  Engine.eval ?host ?deadline ~mode ~fuel ~size_cap ~program args
