(* Tuna interpreter: the triage rules + a fuel/size-bounded stepper.

   Normative source: reference/tree-calculus/implementation/ocaml/lib/tree.ml
   (`apply`, ported verbatim below) and AGENTS.md domain rule 4:

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
     omega halts with Fuel_exhausted at exactly [fuel] steps. *)

open Tuna.Tree

type result =
  | Normal of t * int  (* normal form, steps taken *)
  | Fuel_exhausted of int  (* steps taken before fuel ran out *)
  | Size_exhausted of int  (* steps taken before size_cap was hit *)

(* Internal evaluation state: fuel remaining, steps taken, size cap.
   Unwinding on exhaustion uses local exceptions, caught in [eval];
   they never escape the API boundary. *)
type budget = { mutable fuel : int; mutable steps : int; size_cap : int }

exception Fuel_out
exception Size_out

let check_size b t =
  if size t > b.size_cap then raise Size_out

(* Fire one triage rule: consume fuel or abort. *)
let fire b =
  if b.fuel = 0 then raise Fuel_out;
  b.fuel <- b.fuel - 1;
  b.steps <- b.steps + 1

(* Verbatim port of upstream `apply`, instrumented with step counting
   and size checks. Subterm evaluation order matches the reference
   exactly (see module comment). *)
let rec apply b a c =
  match a with
  | Leaf ->
      (* application, not a step *)
      let r = Stem c in
      check_size b r;
      r
  | Stem a1 ->
      (* application, not a step *)
      let r = Fork (a1, c) in
      check_size b r;
      r
  | Fork (Leaf, a1) ->
      (* triage rule fork(leaf,x) -> x *)
      fire b;
      a1
  | Fork (Stem a1, a2) ->
      (* triage rule fork(stem,_) : apply (apply a1 c) (apply a2 c);
         inner a1 first, then a2, then the outer *)
      fire b;
      let l = apply b a1 c in
      let r = apply b a2 c in
      apply b l r
  | Fork (Fork (a1, a2), a3) ->
      (* triage rule fork(fork,_) *)
      fire b;
      (match c with
       | Leaf -> a1
       | Stem u -> apply b a2 u
       | Fork (u, v) ->
           let l = apply b a3 u in
           apply b l v)

(* Evaluate [program] against [args], folding application left to
   right: r0 = program, r1 = apply r0 arg1, r2 = apply r1 arg2, ...
   size_cap bounds the live tree: the program, every argument, and
   every intermediate result are checked.

   NOTE: [steps] is read into a plain int *after* evaluation. Do not
   inline it into the [Normal] constructor together with [go ...] —
   native OCaml evaluates constructor arguments right-to-left, which
   would snapshot the counter before the run. *)
let eval ~fuel ~size_cap ~program args : result =
  let b = { fuel; steps = 0; size_cap } in
  try
    check_size b program;
    List.iter (check_size b) args;
    let rec go acc = function
      | [] -> acc
      | arg :: rest ->
          let r = apply b acc arg in
          check_size b r;
          go r rest
    in
    let t = go program args in
    Normal (t, b.steps)
  with
  | Fuel_out -> Fuel_exhausted b.steps
  | Size_out -> Size_exhausted b.steps
