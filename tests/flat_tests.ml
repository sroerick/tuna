(* RUNTIME PURITY THESIS (tuna.borg docstring; .ralph/plan.md Open
   Questions) — the M10+ flat-machine experiment.

   Tuna_interp.Flat is an INDEPENDENT pure-step core: an explicit-stack
   (CEK-style) machine whose state is a value, with no monad parameter
   and no Lwt/scheduler types.  This suite is the corpus referee the
   thesis requires: the flat machine's step counts must be bit-for-bit
   identical to the recursive Prim_eval engine (Canonical v0 and
   Sharing v1) on every differential-corpus entry, plus prim-heavy
   synthetic programs (the boundary the thesis specifically probes).

   If the flat machine diverges from the recursive engine on any corpus
   entry, that is a spec violation (corpus red), not a design variant —
   per the RUNTIME PURITY THESIS failure definition. *)

module Flat = Tuna_interp.Flat
module Eval = Tuna_interp.Eval

(* -- corpus reading (mirrors tools/gen/gen.ml's reader) -------------- *)

type corpus = {
  c_name : string
; c_program : string
; c_args : string list
; c_fuel : int
; c_size_cap : int
}

let read_corpus path =
  let name = ref ""
  and program = ref ""
  and args = ref []
  and fuel = ref 1000
  and cap = ref 1000 in
  let ic = open_in path in
  (try
     while true do
       let line = String.trim (input_line ic) in
       if line <> "" && line.[0] <> '#' then
         match String.index_opt line ' ' with
         | None -> ()
         | Some sp ->
             let key = String.sub line 0 sp in
             let v =
               String.trim
                 (String.sub line (sp + 1) (String.length line - sp - 1))
             in
             (match key with
              | "name" -> name := v
              | "program" -> program := v
              | "arg" -> args := v :: !args
              | "fuel" -> fuel := int_of_string v
              | "size_cap" -> cap := int_of_string v
              | _ -> ())
     done
   with End_of_file -> close_in ic);
  { c_name = !name
  ; c_program = !program
  ; c_args = List.rev !args
  ; c_fuel = !fuel
  ; c_size_cap = !cap }

let tree_of ternary =
  match Tuna.Canon.of_string ternary with
  | Ok t -> t
  | Error (off, msg) ->
      Alcotest.failf "corpus ternary parse at %d: %s" off msg

(* status+result+steps as a comparable triple *)
let observe (r : Eval.result) =
  match r with
  | Eval.Normal (t, s) -> ("normal", Tuna.Canon.encode t, s)
  | Eval.Loop s -> ("loop", "-", s)
  | Eval.Fuel_exhausted s -> ("fuel_exhausted", "-", s)
  | Eval.Size_exhausted s -> ("size_exhausted", "-", s)
  | Eval.Deadline_exceeded s -> ("deadline_exceeded", "-", s)

let flat_run ~mode ~fuel ~size_cap ~program ~args =
  let m = Flat.start ~mode ~fuel ~size_cap ~program ~args () in
  Flat.run
    ~host:(fun ~site:_ ~name:_ ~args:_ -> `Error "no prim host in this test")
    m

(* recursive engine, same host *)
let rec_run ~mode ~fuel ~size_cap ~program ~args =
  Eval.eval ~mode ~fuel ~size_cap ~program args

(* Every corpus entry must agree between the two engines under both
   laws.  The corpus is v0 (canonical) so the checked-in expect lines
   are the canonical law; Sharing is compared flat-vs-recursive only
   (both compute the same distinct-work count). *)
let check_agrees ~mode label c =
  let program = tree_of c.c_program in
  let args = List.map tree_of c.c_args in
  let rec_obs =
    observe (rec_run ~mode ~fuel:c.c_fuel ~size_cap:c.c_size_cap ~program ~args)
  in
  let flat_obs =
    observe (flat_run ~mode ~fuel:c.c_fuel ~size_cap:c.c_size_cap ~program ~args)
  in
  Alcotest.(check (triple string string int))
    (Printf.sprintf "%s [%s] flat==recursive" c.c_name label)
    rec_obs flat_obs

let corpus_files () =
  let dir = "../scripts/diff-corpus" in
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".corpus")
  |> List.sort compare
  |> List.map (fun f -> Filename.concat dir f)

let test_corpus_canonical () =
  List.iter (fun f -> check_agrees ~mode:Eval.Canonical "v0" (read_corpus f))
    (corpus_files ())

let test_corpus_sharing () =
  List.iter (fun f -> check_agrees ~mode:Eval.Sharing "v1" (read_corpus f))
    (corpus_files ())

(* -- prim-heavy synthetic cases (the boundary the thesis probes) ------ *)

(* a prim call shaped exactly like the compiler emits: gate applied to
   [name site]; the host answers a constant. *)
let prim_call ~name ~site =
  Tuna.Cprim.call_tree ~name ~site

(* program: a prim call in function position, applied to an arg.  The
   recursive and flat engines must agree (both fuel-free at the gate). *)
let test_prim_boundary () =
  let site n = Tuna.Cstr.unary n in
  ignore site;
  (* one prim call, one argument: program = (prim "echo") arg *)
  let program = prim_call ~name:"echo" ~site:0 in
  let arg = tree_of "22102000" in
  let host ~site:_ ~name:_ ~args:_ =
    `Error "denied for test"
  in
  let rec_r =
    Eval.eval ~host:(fun ~site:_ ~name:_ ~args:_ -> `Error "denied for test")
      ~fuel:100 ~size_cap:10000 ~program [ arg ]
  in
  let flat_m = Flat.start ~fuel:100 ~size_cap:10000 ~program ~args:[ arg ] () in
  let flat_r = Flat.run ~host flat_m in
  Alcotest.(check (triple string string int))
    "prim boundary flat==recursive" (observe rec_r) (observe flat_r)

(* a program that fires triage rules AND calls a prim inside a firing
   (dirty firing under v1): both engines must agree on steps and, under
   Sharing, on the dirty rule. *)
let test_dirty_firing () =
  (* not applied to a prim result: program = not, arg = a prim call that
     the host answers with `true` (10).  The prim is in function position
     of the arg?  Actually: apply not (prim echo) — the prim answers. *)
  let not_tree = tree_of "22102000" in
  let prim = prim_call ~name:"echo" ~site:1 in
  let host ~site:_ ~name:_ ~args:_ = `Done (tree_of "10") in
  let rec_r =
    Eval.eval
      ~host:(fun ~site:_ ~name:_ ~args:_ -> `Ok (tree_of "10"))
      ~mode:Eval.Sharing ~fuel:100 ~size_cap:10000 ~program:not_tree [ prim ]
  in
  let m =
    Flat.start ~mode:Eval.Sharing ~fuel:100 ~size_cap:10000 ~program:not_tree
      ~args:[ prim ] ()
  in
  let flat_r = Flat.run ~host m in
  Alcotest.(check (triple string string int))
    "dirty firing flat==recursive" (observe rec_r) (observe flat_r)

(* loop law: a self-applying term diverges; both engines must answer
   Loop at the same step count under v1.  s i i = 212110021100 (from the
   M4 corpus) re-enters its own firing in flight. *)
let test_loop_law () =
  let m_term = tree_of "212110021100" in
  let host ~site:_ ~name:_ ~args:_ : [ `Done of Tuna.Tree.t | `Error of string ] =
    `Error "x"
  in
  let rec_r =
    Eval.eval ~host:(fun ~site:_ ~name:_ ~args:_ -> `Error "x")
      ~mode:Eval.Sharing ~fuel:1000 ~size_cap:10000 ~program:m_term [ m_term ]
  in
  let m =
    Flat.start ~mode:Eval.Sharing ~fuel:1000 ~size_cap:10000 ~program:m_term
      ~args:[ m_term ] ()
  in
  let flat_r = Flat.run ~host m in
  Alcotest.(check (triple string string int))
    "loop law flat==recursive" (observe rec_r) (observe flat_r);
  (match rec_r with
   | Eval.Loop _ -> ()
   | _ -> Alcotest.fail "expected Loop from both engines")

(* determinism: the flat machine is pure; two runs agree exactly *)
let test_determinism () =
  let program = tree_of "22102000" in
  let args = [ tree_of "10"; tree_of "0" ] in
  let r1 = observe (flat_run ~mode:Eval.Canonical ~fuel:100 ~size_cap:1000 ~program ~args) in
  let r2 = observe (flat_run ~mode:Eval.Canonical ~fuel:100 ~size_cap:1000 ~program ~args) in
  Alcotest.(check (triple string string int)) "deterministic" r1 r2

(* stepping is a value transformation: stepping to completion one
   elementary reduction at a time is the same as run().  Exercises the
   suspension/inspection property the thesis is about. *)
let test_step_by_step () =
  let program = tree_of "22102000" in
  let args = [ tree_of "10" ] in
  let host ~site:_ ~name:_ ~args:_ : [ `Done of Tuna.Tree.t | `Error of string ] =
    `Error "no prim"
  in
  let m = Flat.start ~fuel:100 ~size_cap:1000 ~program ~args () in
  (* drive one elementary reduction at a time; if a prim suspends, answer
     it, all WITHOUT a scheduler — the state is a value *)
  let rec drive n =
    if Flat.result m <> None then n
    else begin
      ignore (Flat.step m);
      (match Flat.pending m with
       | Some (site, name, args) -> Flat.answer m (host ~site ~name ~args)
       | None -> ());
      drive (n + 1)
    end
  in
  let reductions = drive 0 in
  let via_run =
    Flat.run ~host (Flat.start ~fuel:100 ~size_cap:1000 ~program ~args ())
  in
  Alcotest.(check bool) "step-by-step reduction count > 0" true (reductions > 0);
  Alcotest.(check (triple string string int))
    "step-by-step == run" (observe via_run)
    (observe
       (Option.value (Flat.result m) ~default:(Eval.Fuel_exhausted (-1))))

(* async suspension: the machine parks on a prim and the DRIVER answers
   out of band — no monad, no callback, no scheduler in the core.  This
   is the property the thesis names (suspension = a state value). *)
let test_async_suspension () =
  (* program = (prim "echo") applied to leaf; the host answers `true` *)
  let program = prim_call ~name:"echo" ~site:0 in
  let arg = Tuna.Tree.Leaf in
  let m = Flat.start ~fuel:100 ~size_cap:1000 ~program ~args:[ arg ] () in
  ignore (Flat.step m);
  (* the machine suspended on the prim call: a value the driver holds *)
  (match Flat.pending m with
   | Some (site, name, _args) ->
       Alcotest.(check int) "pending site" 0 site;
       Alcotest.(check string) "pending name" "echo" name
   | None -> Alcotest.fail "expected the machine to suspend on the prim");
  (* answer out of band, then drive to completion *)
  Flat.answer m (`Done (tree_of "10"));
  ignore (Flat.advance m);
  (match Flat.result m with
   | Some (Eval.Normal (t, _)) ->
       Alcotest.(check string) "async answer became the value"
         (Tuna.Canon.encode (tree_of "10")) (Tuna.Canon.encode t)
   | _ -> Alcotest.fail "expected a normal form after async answer")

let () =
  Alcotest.run "flat"
    [ ( "purity-thesis"
      , [ Alcotest.test_case "corpus v0: flat == recursive" `Quick
            test_corpus_canonical
        ; Alcotest.test_case "corpus v1: flat == recursive" `Quick
            test_corpus_sharing
        ; Alcotest.test_case "prim boundary" `Quick test_prim_boundary
        ; Alcotest.test_case "dirty firing (v1)" `Quick test_dirty_firing
        ; Alcotest.test_case "loop law (v1)" `Quick test_loop_law
        ; Alcotest.test_case "determinism" `Quick test_determinism
        ; Alcotest.test_case "step-by-step == run" `Quick test_step_by_step
        ; Alcotest.test_case "async suspension (no scheduler)" `Quick
            test_async_suspension
        ] ) ]
