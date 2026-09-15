(* Tuna_interp.Prim_eval: the answer-monadic engine.

   ONE verbatim port of the upstream triage rules (same match arms,
   same evaluation order as reference/tree-calculus/implementation/
   ocaml/lib/tree.ml), parameterized over the monad in which the host
   answers prim calls.  The pure engine (Eval) and the journalling
   host boundary (server Run.execute) therefore share a single
   evaluation order and a single step counter — faithful replay is
   an identity by construction, not by discipline.

   Step counting (AGENTS.md rule 4): a step is one firing of a triage
   rule (the Fork cases).  The two wrapper applications and prim
   calls consume no fuel: the boundary is not part of the calculus.
   Prim arguments are reduced first (innermost), under the same
   fuel/size budget; the answer tree is size-checked against the cap.

   Prim convention (Tuna.Cprim): a prim call is the application of a
   gate tree; when [apply] is entered with a gate tree in function
   position, the host answers: [`Ok tree] becomes the value, [`Error
   msg] becomes the canonical error tree (Stem (Cstr.encode msg)) so
   the calculus keeps computing deterministically.  Prim calls may
   also appear purely as data (a gate tree is inert until applied). *)

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* [catch run handler] — [run] is a thunk so that synchronous (eager
     monad) raises inside it are seen by the handler too. *)
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module Make (M : MONAD) = struct
  open Tuna.Tree

  (* The host boundary: answers one prim call.  Never raises — denial
     and failure are answers (AGENTS.md rule 7). *)
  type host =
    site:int -> name:string -> args:t -> [ `Ok of t | `Error of string ] M.t

  type result =
    | Normal of t * int  (* normal form, steps taken *)
    | Fuel_exhausted of int  (* steps taken before fuel ran out *)
    | Size_exhausted of int  (* steps taken before size_cap was hit *)

  type budget = { mutable fuel : int; mutable steps : int; size_cap : int }

  exception Fuel_out
  exception Size_out

  let check_size b t = if size t > b.size_cap then raise Size_out

  (* Fire one triage rule: consume fuel or abort. *)
  let fire b =
    if b.fuel = 0 then raise Fuel_out;
    b.fuel <- b.fuel - 1;
    b.steps <- b.steps + 1

  let rec apply b host a c =
    match Tuna.Cprim.shape a with
    | Some (name, site) -> prim_call b host ~site ~name c
    | None -> (
        match a with
        | Leaf ->
            (* application, not a step *)
            let r = Stem c in
            check_size b r;
            M.return r
        | Stem a1 ->
            (* application, not a step *)
            let r = Fork (a1, c) in
            check_size b r;
            M.return r
        | Fork (Leaf, a1) ->
            (* triage rule fork(leaf,x) -> x *)
            fire b;
            M.return a1
        | Fork (Stem a1, a2) ->
            (* triage rule fork(stem,_): inner a1 first, then a2, then
               the outer *)
            fire b;
            M.bind (apply b host a1 c) (fun l ->
                M.bind (apply b host a2 c) (fun r -> apply b host l r))
        | Fork (Fork (a1, a2), a3) ->
            (* triage rule fork(fork,_) *)
            fire b;
            (match c with
             | Leaf -> M.return a1
             | Stem u -> apply b host a2 u
             | Fork (u, v) ->
                 M.bind (apply b host a3 u) (fun l -> apply b host l v)))

  (* A prim call: the args tree is passed AS REDUCED BY THE STRATEGY
     (trees are values; there is no separate argument normalizer in
     the calculus).  The host's answer becomes the value of the
     application; errors become the canonical error tree. *)
  and prim_call b host ~site ~name c =
    M.bind (M.return ()) (fun () ->
        M.bind (host ~site ~name ~args:c) (function
          | `Ok r ->
              check_size b r;
              M.return r
          | `Error msg ->
              let e = Stem (Tuna.Cstr.encode msg) in
              check_size b e;
              M.return e))

  (* Evaluate [program] against [args], folding application left to
     right: r0 = program, r1 = apply r0 arg1, r2 = apply r1 arg2, ...
     size_cap bounds the live tree: the program, every argument, and
     every intermediate result are checked.

     Everything that can raise (budget checks, the host) happens
     inside a bind callback so [catch] sees it for both monads. *)
  let eval ?(host : host =
              fun ~site:_ ~name:_ ~args:_ ->
                M.return (`Error "no prim host at this boundary"))
      ~fuel ~size_cap ~program args : result M.t =
    let b = { fuel; steps = 0; size_cap } in
    let rec go acc = function
      | [] -> M.return acc
      | arg :: rest ->
          M.bind (apply b host acc arg) (fun r ->
              check_size b r;
              go r rest)
    in
    M.catch (fun () ->
        check_size b program;
        List.iter (check_size b) args;
        M.bind (go program args) (fun t -> M.return (Normal (t, b.steps))))
      (function
        | Fuel_out -> M.return (Fuel_exhausted b.steps)
        | Size_out -> M.return (Size_exhausted b.steps)
        | e -> M.bind (M.return ()) (fun () -> raise e))
end
