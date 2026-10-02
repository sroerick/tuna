(* Demand-memo acceptance tests (borg/sharing.borg §demand-memo).  PG-gated
   integration (scripts/test-store.sh, TUNA_TEST_PG=1); own scratch db
   (tuna_test_demand).  Exercises Run.execute_run directly, like
   store_tests, so the acceptance is about store behavior + the pure
   core reading a garden, not HTTP plumbing.

   Pins under test:
   1. a CLEAN firing computed by run 1 is cached (caller's garden) and
      reused by a LATER run of the same program -> the later run does
      fewer distinct firings (garden_hits > 0) and lands the same result;
   2. the dirty rule: a firing whose evaluation answered a prim is NEVER
      written to the garden (grant liveness / audit survive);
   3. own-garden scope: run B (a different identity) does NOT see run
      A's cache;
   4. off by default: without demand:true the garden is never consulted
      or written. *)

open Lwt.Infix

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Run = Tuna_server.Run

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> Lwt.return p

let who p name = S.bootstrap_identity p ~name ~token:(name ^ "-token") ()

let upsert p ~ternary =
  let hash = Tuna.Hash.hex_of_string ternary in
  S.upsert_program p ~hash ~ternary ~ir:None ~created_by:None
  >>= fun _ -> Lwt.return hash

(* T-family unfolded at run time (the same shape sharing_tests uses):
   the compiled artifact's v1 firing count is O(d) distinct.  A fixed
   d gives a stable, non-trivial clean firing set. *)
let rec t_src d =
  if d = 0 then "(lambda (z) z)"
  else
    Printf.sprintf "(lambda (z) (%s (%s z)))" (t_src (d - 1)) (t_src (d - 1))

let compile src =
  match
    (try Ok (Tuna_compiler.Bracket.compile_source src)
     with Tuna_compiler.Bracket.Compile_failed msg -> Error msg)
  with
  | Error msg -> Alcotest.fail ("compile: " ^ msg)
  | Ok art -> art

(* run a program under v1 with demand on/off; return the run row *)
let run_once p ~caller ~program_hash ~demand =
  Run.execute_run p ~caller ~program_hash ~input_trees:[] ~grant_ids:[]
    ~fuel:100_000 ~semantics:"v1" ~demand ~size_cap:1_000_000 ()
  >>= function
  | Error (_, msg) -> Alcotest.fail ("run failed: " ^ msg)
  | Ok (row, _js) -> Lwt.return row

let test_clean_firing_cached_and_reused () =
  setup () >>= fun p ->
  who p "demand-a" >>= fun a ->
  (* the compiled T-tree applied to identity (as in sharing_tests): the
     T-family unfolds at run time, ~2d+2 distinct v1 firings, all clean *)
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 4)) in
  let ternary = Tuna.Canon.encode art.Tuna_compiler.Bracket.tree in
  upsert p ~ternary >>= fun ph ->
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  (* run 1 with demand: cold garden, computes everything, caches *)
  Run.execute_run p ~caller:a.S.i_id ~program_hash:ph ~input_trees:[ ii ]
    ~grant_ids:[] ~fuel:100_000 ~semantics:"v1" ~demand:true
    ~size_cap:1_000_000 ()
  >>= (function
        | Error (_, msg) -> Alcotest.fail ("run failed: " ^ msg)
        | Ok (row, _) -> Lwt.return row)
  >>= fun r1 ->
  Alcotest.(check int) "run1: no garden hits (cold)" 0 r1.S.r_demand_hits;
  Alcotest.(check bool) "run1: demand_sharing recorded" true
    r1.S.r_demand_sharing;
  let s1 = Option.value r1.S.r_step_count ~default:(-1) in
  Alcotest.(check bool) "run1: actually fired (sanity)" true (s1 > 0);
  S.demand_memo_count p >>= fun cached ->
  Alcotest.(check bool) "run1: clean firings cached" true (cached > 0);
  (* run 2 with demand: warm garden, reuses the cache *)
  Run.execute_run p ~caller:a.S.i_id ~program_hash:ph ~input_trees:[ ii ]
    ~grant_ids:[] ~fuel:100_000 ~semantics:"v1" ~demand:true
    ~size_cap:1_000_000 ()
  >>= (function
        | Error (_, msg) -> Alcotest.fail ("run2 failed: " ^ msg)
        | Ok (row, _) -> Lwt.return row)
  >>= fun r2 ->
  let s2 = Option.value r2.S.r_step_count ~default:(-1) in
  Alcotest.(check bool) "run2: garden hits > 0" true (r2.S.r_demand_hits > 0);
  Alcotest.(check bool) "run2: fewer distinct firings than run1" true
    (s2 < s1);
  (* same result hash: clean firings are pure, so the answer is identical *)
  Alcotest.(check (option string)) "same result" r1.S.r_result_hash
    r2.S.r_result_hash;
  Lwt.return ()

let test_dirty_firings_never_cached () =
  setup () >>= fun p ->
  who p "demand-dirty" >>= fun a ->
  (* a RAW prim-call tree applied to one input: the reduction contains
     exactly ONE firing, and its evaluation answers a prim -- so it is
     dirty and must never enter the garden (no compiler glue firings to
     muddy the assertion).  Grant liveness: run 2 must re-execute the
     prim call, not replay it from a cache. *)
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  let ternary = Tuna.Canon.encode call in
  upsert p ~ternary >>= fun ph ->
  let input = match Tuna.Canon.of_string "10" with
    | Ok t -> t
    | Error e -> Alcotest.fail ("parse input: " ^ snd e)
  in
  (* grant the prim so it answers (a denial would be an equally-dirty
     host answer, but the granted path is the liveness case) *)
  S.mint_grant p ~prim:"echo" ~args_attenuation:"null" ~caller:a.S.i_id
    ~minted_by:(Some a.S.i_id) ()
  >>= fun g ->
  let run_demand () =
    Run.execute_run p ~caller:a.S.i_id ~program_hash:ph
      ~input_trees:[ input ] ~grant_ids:[ g.S.g_id ] ~fuel:100_000
      ~semantics:"v1" ~demand:true ~size_cap:1_000_000 ()
    >>= (function
          | Error (_, msg) -> Alcotest.fail ("dirty run failed: " ^ msg)
          | Ok (row, _) -> Lwt.return row)
  in
  run_demand () >>= fun r1 ->
  (* the only firing is dirty: zero clean firings, nothing persisted *)
  S.demand_memo_list p ~caller:a.S.i_id () >>= fun rows ->
  Alcotest.(check int) "dirty program wrote no garden rows" 0
    (List.length rows);
  (* the prim was answered live (journal carries it), result is echo *)
  (match r1.S.r_result_ternary with
   | Some rt -> Alcotest.(check string) "echo answered live" "10" rt
   | None -> Alcotest.fail "dirty run must produce a result");
  (* run 2: still nothing cached -- the prim re-executes *)
  run_demand () >>= fun r2 ->
  Alcotest.(check int) "run2: still no garden hits" 0 r2.S.r_demand_hits;
  S.demand_memo_list p ~caller:a.S.i_id () >>= fun rows2 ->
  Alcotest.(check int) "run2: still no garden rows" 0 (List.length rows2);
  Lwt.return ()

let test_own_garden_scope () =
  setup () >>= fun p ->
  who p "demand-owner" >>= fun a ->
  who p "demand-other" >>= fun b ->
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 3)) in
  let ternary = Tuna.Canon.encode art.Tuna_compiler.Bracket.tree in
  upsert p ~ternary >>= fun ph ->
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let runDemand caller =
    Run.execute_run p ~caller ~program_hash:ph ~input_trees:[ ii ]
      ~grant_ids:[] ~fuel:100_000 ~semantics:"v1" ~demand:true
      ~size_cap:1_000_000 ()
    >>= (function
          | Error (_, msg) -> Alcotest.fail ("run failed: " ^ msg)
          | Ok (row, _) -> Lwt.return row)
  in
  (* a computes and caches into ITS OWN garden *)
  runDemand a.S.i_id >>= fun ra1 ->
  Alcotest.(check bool) "a: cold, then fires" true
    (ra1.S.r_demand_hits = 0 && Option.value ra1.S.r_step_count ~default:0 > 0);
  (* b must NOT see a's cache: cold garden, full firing count *)
  runDemand b.S.i_id >>= fun rb1 ->
  Alcotest.(check int) "b: a's cache is invisible" 0 rb1.S.r_demand_hits;
  Alcotest.(check bool) "b: fired the full work" true
    (rb1.S.r_step_count = ra1.S.r_step_count);
  (* b's second run: b's OWN garden is now warm *)
  runDemand b.S.i_id >>= fun rb2 ->
  Alcotest.(check bool) "b: own garden warm on repeat" true
    (rb2.S.r_demand_hits > 0);
  Lwt.return ()

let test_off_by_default () =
  setup () >>= fun p ->
  who p "demand-default" >>= fun a ->
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 3)) in
  let ternary = Tuna.Canon.encode art.Tuna_compiler.Bracket.tree in
  upsert p ~ternary >>= fun ph ->
  (* demand absent: demand_sharing false, no garden read/write *)
  run_once p ~caller:a.S.i_id ~program_hash:ph ~demand:false >>= fun r ->
  Alcotest.(check bool) "demand_sharing false" false r.S.r_demand_sharing;
  Alcotest.(check int) "no hits" 0 r.S.r_demand_hits;
  S.demand_memo_list p ~caller:a.S.i_id ()
  >>= fun rows ->
  Alcotest.(check int) "nothing written when off" 0 (List.length rows);
  Lwt.return ()

let () =
  let lwt name f = Alcotest_lwt.test_case name `Quick (fun _sw () -> f ()) in
  Lwt_main.run
    (Alcotest_lwt.run "demand"
       [ ( "demand-memo"
         , [ lwt "clean firings cached and reused" test_clean_firing_cached_and_reused
           ; lwt "dirty firings never cached" test_dirty_firings_never_cached
           ; lwt "own-garden scope" test_own_garden_scope
           ; lwt "off by default" test_off_by_default
           ] ) ])
