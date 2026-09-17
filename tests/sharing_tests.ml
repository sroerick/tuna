(* Sharing semantics (borg/sharing.borg): the v1 distinct-work law.
   Pure — no Postgres needed.  Pins, all measured (probe harness on
   de42ff6, numbers stable across runs):

   - T-family (T0 = I, Ti = lam z. T(i-1) (T(i-1) z), unfolded at RUN
     time through an outer lambda): canonical v0 = 2^(d+2)-2 raw
     firings while the answer stays 5 nodes — the F1 exponential,
     measured, not extrapolated.  v1 = 2d+2 distinct firings: the two
     textual occurrences of each T(i-1) share both firings.  Same
     normal form under both laws.
   - M M (lam x. x x applied to itself): v0 burns the whole fuel
     budget; v1 detects the in-flight re-entry and answers Loop
     finitely, deterministically.  v1's loop set is a subset of v0's
     exhaustion set — no expressive power lost.
   - prims-twice (the dirty rule): a firing that answered a prim is
     never memoized, so the same textual call re-executes and the host
     answers on every occurrence — grant liveness and the journal
     audit survive memoization. *)

let compile src =
  match
    (try Ok (Tuna_compiler.Bracket.compile_source src)
     with Tuna_compiler.Bracket.Compile_failed msg -> Error msg)
  with
  | Error msg -> Alcotest.fail ("compile: " ^ msg)
  | Ok art -> art

let eval ?(sharing = false) ?host ~fuel
    program args =
  let mode =
    if sharing then Tuna_interp.Eval.Sharing else Tuna_interp.Eval.Canonical
  in
  (match host with
   | Some h ->
       Tuna_interp.Eval.Engine.eval ~host:h ~mode ~fuel ~size_cap:1_000_000
         ~program args
   | None -> Tuna_interp.Eval.eval ~mode ~fuel ~size_cap:1_000_000 ~program args)

let rec t_src d =
  if d = 0 then "(lambda (z) z)"
  else
    Printf.sprintf "(lambda (z) (%s (%s z)))" (t_src (d - 1)) (t_src (d - 1))

let test_t_family () =
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  List.iter
    (fun d ->
      let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src d)) in
      let name = Printf.sprintf "T%d" d in
      match eval ~fuel:100_000_000 art.Tuna_compiler.Bracket.tree [ ii ] with
      | Tuna_interp.Eval.Normal (t0, s0) ->
          Alcotest.(check int) (name ^ ": v0 raw firings")
            ((1 lsl (d + 2)) - 2)
            s0;
          (match
             eval ~sharing:true ~fuel:100_000_000 art.Tuna_compiler.Bracket.tree
               [ ii ]
           with
           | Tuna_interp.Eval.Normal (t1, s1) ->
               Alcotest.(check bool) (name ^ ": same normal form") true (t0 = t1);
               Alcotest.(check int) (name ^ ": v1 distinct firings")
                 ((2 * d) + 2)
                 s1
           | _ -> Alcotest.failf "%s: expected a v1 normal form" name)
      | _ -> Alcotest.failf "%s: expected a v0 normal form" name)
    [ 0; 1; 2; 3; 4; 5 ]

let test_mm_loop () =
  let m = (compile "(lambda (x) (x x))").Tuna_compiler.Bracket.tree in
  (* v1: the firing (M, M) re-enters itself in flight — divergence,
     answered finitely at the first re-entry, 3 steps in *)
  (match eval ~sharing:true ~fuel:1000 m [ m ] with
   | Tuna_interp.Eval.Loop s ->
       Alcotest.(check int) "loop detected at the first re-entry" 3 s
   | _ -> Alcotest.fail "expected Loop");
  (* deterministic across runs *)
  (match eval ~sharing:true ~fuel:1000 m [ m ] with
   | Tuna_interp.Eval.Loop s -> Alcotest.(check int) "loop is deterministic" 3 s
   | _ -> Alcotest.fail "expected Loop");
  (* v0 burns the whole budget on the same term *)
  (match eval ~fuel:1000 m [ m ] with
   | Tuna_interp.Eval.Fuel_exhausted s ->
       Alcotest.(check int) "v0 exhausts the budget" 1000 s
   | _ -> Alcotest.fail "expected Fuel_exhausted")

let test_prims_twice () =
  (* The dirty rule, made exact: the SAME firing pair (g, w) placed at
     two textual occurrences with the same arg tree.  The clean twin
     (g = identity) memo-hits the second occurrence — v1 does the
     whole firing for free (7 raw steps vs 3).  The dirty twin (g
     answers a prim) must re-execute instead: the host answers on BOTH
     occurrences with identical args, the property journal-fed replay
     depends on.  Without the dirty rule the second occurrence would
     be a memo hit: one host call, and the grant re-check plus its
     journal row silently skipped. *)
  let x = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let run ~g ~sharing =
    let calls = ref [] in
    let host ~site:_ ~name ~args =
      calls := (name, Tuna.Canon.encode args) :: !calls;
      `Ok args
    in
    let program =
      (compile (Printf.sprintf "(lambda (w) ((%s w) (%s w)))" g g))
        .Tuna_compiler.Bracket.tree
    in
    let outcome = eval ~sharing ~host ~fuel:100_000 program [ x ] in
    (outcome, List.rev !calls)
  in
  (* clean twin: the second (identity, w) is a free memo hit *)
  let (c0, _) = run ~g:"(lambda (u) u)" ~sharing:false in
  let (cv, _) = run ~g:"(lambda (u) u)" ~sharing:true in
  (match (c0, cv) with
   | Tuna_interp.Eval.Normal (_, s0), Tuna_interp.Eval.Normal (_, sv) ->
       Alcotest.(check int) "clean twin: v0 raw steps" 7 s0;
       Alcotest.(check int) "clean twin: v1 memo-hit steps" 3 sv
   | _ -> Alcotest.fail "expected normal outcomes");
  (* dirty twin: same pair, dirty answer — no memo hit either law *)
  let (d0, calls0) = run ~g:"(lambda (u) (prim \"echo\" u))" ~sharing:false in
  let (dv, callsv) = run ~g:"(lambda (u) (prim \"echo\" u))" ~sharing:true in
  let check_calls what calls =
    Alcotest.(check int) (what ^ ": echo answered twice") 2 (List.length calls);
    (match calls with
     | (n0, a0) :: (n1, a1) :: _ ->
         Alcotest.(check string) (what ^ ": prim name") "echo" n0;
         Alcotest.(check string) (what ^ ": prim name (2nd)") "echo" n1;
         (* the re-executed firing reproduces identical args — the
            journal-replay property *)
         Alcotest.(check string) (what ^ ": identical args") a0 a1
     | _ -> Alcotest.fail (what ^ ": expected two calls"))
  in
  Alcotest.(check int) "dirty twin: v0 raw steps" 10
    (match d0 with Tuna_interp.Eval.Normal (_, s) -> s | _ -> -1);
  Alcotest.(check int) "dirty twin: v1 re-executes (no memo hit)" 8
    (match dv with Tuna_interp.Eval.Normal (_, s) -> s | _ -> -1);
  check_calls "v0" calls0;
  check_calls "v1 (dirty rule)" callsv;
  (* same normal form under both laws, both twins *)
  (match (c0, cv, d0, dv) with
   | Tuna_interp.Eval.Normal (a0, _), Tuna_interp.Eval.Normal (av, _),
     Tuna_interp.Eval.Normal (b0, _), Tuna_interp.Eval.Normal (bv, _) ->
       Alcotest.(check bool) "clean twin: same normal form" true (a0 = av);
       Alcotest.(check bool) "dirty twin: same normal form" true (b0 = bv)
   | _ -> Alcotest.fail "expected normal outcomes")

let () =
  Alcotest.run "sharing"
    [ ( "v1"
      , [ Alcotest.test_case "T-family: v0 exponential, v1 2d+2 distinct, same answer"
            `Quick test_t_family
        ; Alcotest.test_case "M M: v1 Loop at first re-entry, v0 fuel exhaustion"
            `Quick test_mm_loop
        ; Alcotest.test_case "prims-twice: dirty firings never memoize"
            `Quick test_prims_twice
        ] ) ]
