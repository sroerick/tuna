(* flatbench: the M10+ flat-machine cost probe (RUNTIME PURITY THESIS,
   borg/deriv of .ralph/plan.md Open Questions).  Compares the recursive
   monadic engine (Prim_eval via Eval) with the pure-step explicit-stack
   machine (Flat) on the two workloads the thesis names: SMALL-PRIM
   (mostly pure calculus, few boundary crossings) and PRIM-HEAVY (many
   boundary crossings).  Reports wall time and asserts step-count
   identity — the criterion is measured cost on equal step counts,
   nothing aesthetic.

   Build/run: dune exec tools/flatbench/flatbench.exe [-- fuel cap] *)

module Flat = Tuna_interp.Flat
module Eval = Tuna_interp.Eval

let now () = Unix.gettimeofday ()

let time_it f =
  let t0 = now () in
  let r = f () in
  (r, now () -. t0)

(* -- workloads -------------------------------------------------------- *)

(* SMALL-PRIM: a long chain of pure applications (not . not . ...) with
   one prim call at the end.  Exercised at several depths. *)
let small_prim ~depth ~prims =
  let not_tree = match Tuna.Canon.of_string "22102000" with Ok t -> t | _ -> assert false in
  let true_tree = match Tuna.Canon.of_string "10" with Ok t -> t | _ -> assert false in
  (* program = not applied `depth` times to a chain of prim calls *)
  let prim = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  let args = List.init prims (fun _ -> prim) in
  (not_tree, true_tree :: List.init depth (fun _ -> not_tree) @ args)

(* PRIM-HEAVY: many prim calls, each answered in place, interleaved with
   a little calculus.  The boundary is crossed `n` times. *)
let prim_heavy ~n =
  let not_tree = match Tuna.Canon.of_string "22102000" with Ok t -> t | _ -> assert false in
  let prim = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  (not_tree, List.init n (fun _ -> prim))

let host_done ~site:_ ~name:_ ~args:_ : [ `Done of Tuna.Tree.t | `Error of string ] =
  `Done Tuna.Tree.Leaf

let host_ok ~site:_ ~name:_ ~args:_ : [ `Ok of Tuna.Tree.t | `Error of string ] =
  `Ok Tuna.Tree.Leaf

let run_flat ~mode ~fuel ~cap program args =
  let m = Flat.start ~mode ~fuel ~size_cap:cap ~program ~args () in
  Flat.run ~host:host_done m

let run_rec ~mode ~fuel ~cap program args =
  Eval.eval ~host:host_ok ~mode ~fuel ~size_cap:cap ~program args

let observe (r : Eval.result) =
  match r with
  | Eval.Normal (_, s) -> ("normal", s)
  | Eval.Loop s -> ("loop", s)
  | Eval.Fuel_exhausted s -> ("fuel", s)
  | Eval.Size_exhausted s -> ("size", s)
  | Eval.Deadline_exceeded s -> ("deadline", s)

let bench label ~mode ~fuel ~cap (program, args) =
  let (flat_r, flat_t) = time_it (fun () -> run_flat ~mode ~fuel ~cap program args) in
  let (rec_r, rec_t) = time_it (fun () -> run_rec ~mode ~fuel ~cap program args) in
  let fobs = observe flat_r and robs = observe rec_r in
  let same = fobs = robs in
  Printf.printf
    "%-22s steps=%-9d flat=%8.4fs rec=%8.4fs ratio=%.2f  %s\n%!"
    label (snd robs) flat_t rec_t
    (if rec_t > 0. then flat_t /. rec_t else 0.)
    (if same then "step-count-identical" else
       Printf.sprintf "DIVERGED flat=%s rec=%s" (fst fobs) (fst robs));
  if not same then exit 1

let () =
  let args = Array.to_list Sys.argv in
  ignore args;
  Printf.printf "== flatbench: RUNTIME PURITY THESIS cost probe ==\n%!";
  List.iter
    (fun depth ->
      bench (Printf.sprintf "small-prim d=%d" depth)
        ~mode:Eval.Canonical ~fuel:100_000_000 ~cap:10_000_000
        (small_prim ~depth ~prims:1))
    [ 100; 500; 2000 ];
  List.iter
    (fun n ->
      bench (Printf.sprintf "prim-heavy n=%d" n)
        ~mode:Eval.Canonical ~fuel:100_000_000 ~cap:10_000_000
        (prim_heavy ~n))
    [ 1000; 5000; 20000 ];
  (* one sharing-mode sanity row *)
  bench "sharing small-prim d=500"
    ~mode:Eval.Sharing ~fuel:10_000_000 ~cap:10_000_000
    (small_prim ~depth:500 ~prims:1);
  ()
