(* Run traces (borg/trace.borg): the firing-event collector.  Pure — no
   Postgres needed.  The law under test: a trace is OBSERVATION ONLY —
   same steps, same normal form, same evaluation order as the untraced
   run — and its counters tell the v0/v1 story exactly:

   - v0: every counted firing emits a [fire] event; recorded = steps.
   - v1: [charge] events are the memo table filling; memo hits are
     aggregated (never events, they would be the 2M-row story); dirty
     firings note themselves and re-execute; in-flight re-entry emits
     the [loop] event and the run answers Loop.
   - the raw counter counts gate ENTRIES under the law that is
     running: raw = charged + hits (+1 when a loop fired).  Under v1 a
     hit short-circuits a whole subtree, so the gate is never re-entered
    for a memoized answer's interior — v1's raw is therefore generally
    BELOW v0's step count (T2 d=2: raw 8 vs v0 14; that gap IS the
    sharing saving, and the F1 pilot measured it from the v0 side). *)

let compile src =
  match
    (try Ok (Tuna_compiler.Bracket.compile_source src)
     with Tuna_compiler.Bracket.Compile_failed msg -> Error msg)
  with
  | Error msg -> Alcotest.fail ("compile: " ^ msg)
  | Ok art -> art

module E = Tuna_interp.Eval
module Eng = E.Engine

let eval ?(sharing = false) ?host ?(trace = None) ~fuel program args =
  let mode = if sharing then E.Sharing else E.Canonical in
  match host with
  | Some h ->
      Eng.eval ~host:h ~mode ?trace ~fuel ~size_cap:1_000_000 ~program args
  | None -> E.eval ~mode ?trace ~fuel ~size_cap:1_000_000 ~program args

let trace_of tr =
  let recorded = tr.Eng.tr_next in
  let events =
    List.rev (Queue.fold (fun acc e -> e :: acc) [] tr.Eng.tr_events)
  in
  (tr, recorded, events)

let new_trace ?cap () = Eng.new_trace ?cap ()

let kinds events = List.map (fun e -> Eng.kind_string e.Eng.e_kind) events

let rec t_src d =
  if d = 0 then "(lambda (z) z)"
  else Printf.sprintf "(lambda (z) (%s (%s z)))" (t_src (d - 1)) (t_src (d - 1))

let test_v0_trace () =
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 2)) in
  let program = art.Tuna_compiler.Bracket.tree in
  (* untraced baseline: 2^(d+2)-2 = 14 canonical firings *)
  match eval ~fuel:100_000 program [ ii ] with
  | E.Normal (form, steps) ->
      Alcotest.(check int) "v0 baseline steps" 14 steps;
      let tr = new_trace () in
      (match eval ~trace:(Some tr) ~fuel:100_000 program [ ii ] with
       | E.Normal (form2, steps2) ->
           Alcotest.(check bool) "trace changes nothing (form)" true
             (form = form2);
           Alcotest.(check int) "trace changes nothing (steps)" steps steps2;
           let tr, recorded, events = trace_of tr in
           Alcotest.(check int) "v0: one event per counted firing" 14 recorded;
           Alcotest.(check bool) "v0: not truncated" false
             tr.Eng.tr_truncated;
           Alcotest.(check int) "v0: raw = steps" 14 tr.Eng.tr_raw;
           Alcotest.(check int) "v0: no hits" 0 tr.Eng.tr_hits;
           Alcotest.(check int) "v0: no dirty" 0 tr.Eng.tr_dirty;
           Alcotest.(check bool) "v0: no loop" false tr.Eng.tr_loop;
           Alcotest.(check bool) "v0: all events are fires"
             true
             (List.for_all (fun k -> k = "fire") (kinds events));
           Alcotest.(check bool) "v0: seqs are 0..n-1"
             true
             (List.map (fun e -> e.Eng.e_seq) events = List.init 14 Fun.id);
           (* small trees get display digests *)
           (match events with
            | e :: _ ->
                Alcotest.(check bool) "v0: fire event carries digests" true
                  (e.Eng.e_fun <> "" && e.Eng.e_arg <> "")
            | [] -> Alcotest.fail "expected events")
       | _ -> Alcotest.fail "expected a traced v0 normal form")
  | _ -> Alcotest.fail "expected a v0 normal form"

let test_v1_trace_t_family () =
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 2)) in
  let program = art.Tuna_compiler.Bracket.tree in
  match (eval ~fuel:100_000 program [ ii ], eval ~sharing:true ~fuel:100_000 program [ ii ])
  with
  | E.Normal (form0, steps0), E.Normal (form1, steps1) ->
      Alcotest.(check int) "v1 charged steps (2d+2)" 6 steps1;
      Alcotest.(check bool) "same normal form" true (form0 = form1);
      let tr = new_trace () in
      (match eval ~sharing:true ~trace:(Some tr) ~fuel:100_000 program [ ii ] with
       | E.Normal (_, steps) ->
           Alcotest.(check int) "trace changes nothing (steps)" steps1 steps;
           let tr, recorded, events = trace_of tr in
           Alcotest.(check int) "v1: one charge event per counted step" steps
             recorded;
           Alcotest.(check bool) "v1: all events are charges"
             true (List.for_all (fun k -> k = "charge") (kinds events));
           (* measured pins (probe run, deterministic): raw = charged +
              hits = 8; the two hits replace whole subtrees, so v1's raw
              sits BELOW v0's 14 — the gap is the sharing saving *)
           Alcotest.(check int) "v1: raw = charged + hits" 8 tr.Eng.tr_raw;
           Alcotest.(check int) "v1: hits (measured pin)" 2 tr.Eng.tr_hits;
           Alcotest.(check bool) "v1: raw below v0 (sharing saved work)"
             true (tr.Eng.tr_raw < steps0);
           Alcotest.(check int) "v1: no dirty" 0 tr.Eng.tr_dirty;
           Alcotest.(check bool) "v1: no loop" false tr.Eng.tr_loop;
           (* charge events carry the memo law's pair digests *)
           (match events with
            | e :: _ ->
                Alcotest.(check bool) "v1: charge event carries digests" true
                  (String.length e.Eng.e_fun = 16 && String.length e.Eng.e_arg = 16)
            | [] -> Alcotest.fail "expected events")
       | _ -> Alcotest.fail "expected a traced v1 normal form")
  | _ -> Alcotest.fail "expected normal forms"

let test_v1_trace_loop () =
  let m = (compile "(lambda (x) (x x))").Tuna_compiler.Bracket.tree in
  let tr = new_trace () in
  (match eval ~sharing:true ~trace:(Some tr) ~fuel:1000 m [ m ] with
   | E.Loop steps ->
       Alcotest.(check int) "loop at first re-entry (3 steps)" 3 steps;
       let tr, recorded, events = trace_of tr in
       Alcotest.(check bool) "loop flagged" true tr.Eng.tr_loop;
       Alcotest.(check int) "raw = charged + 1 loop entry"
         (tr.Eng.tr_hits + steps + 1) tr.Eng.tr_raw;
       Alcotest.(check int) "one loop event appended" (steps + 1) recorded;
       (match List.rev events with
        | last :: _ ->
            Alcotest.(check string) "last event is the loop" "loop"
              (Eng.kind_string last.Eng.e_kind);
            Alcotest.(check string) "loop note" "in-flight re-entry"
              last.Eng.e_note;
            Alcotest.(check bool) "loop event carries the offending pair" true
              (last.Eng.e_fun <> "" && last.Eng.e_arg <> "")
        | [] -> Alcotest.fail "expected events")
   | _ -> Alcotest.fail "expected Loop")

let test_v1_trace_dirty () =
  let x = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let calls = ref [] in
  let host ~site:_ ~name ~args =
    calls := (name, Tuna.Canon.encode args) :: !calls;
    `Ok args
  in
  let program =
    (compile "(lambda (w) (((lambda (u) (prim \"echo\" u)) w) ((lambda (u) (prim \"echo\" u)) w)))")
      .Tuna_compiler.Bracket.tree
  in
    (* v0 baseline: same program+inputs, no memo (sharing pins: 10) *)
    let steps0 =
      match eval ~host ~fuel:100_000 program [ x ] with
      | E.Normal (_, s) -> s
      | _ -> Alcotest.fail "expected a v0 normal form"
    in
    let tr = new_trace () in
    (* fresh ledger for the traced run: the v0 baseline answered the
       same two prims, and only the v1 occurrence count is under test *)
    calls := [];
    (match eval ~sharing:true ~host ~trace:(Some tr) ~fuel:100_000 program [ x ] with
     | E.Normal (_, steps) ->
         let tr, _recorded, events = trace_of tr in
         Alcotest.(check int) "dirty: re-executed (8 charged, per sharing pins)" 8
           steps;
         Alcotest.(check int) "dirty: v0 raw steps" 10 steps0;
         (* measured pins (probe run, deterministic): 9 gate entries =
            8 charged + 1 clean hit; the two dirty twins re-execute AND
            the enclosing firing turns dirty with them — dirt propagates
            to enclosing firings exactly (prim-exemption law) *)
         Alcotest.(check int) "dirty: raw = charged + hits" 9 tr.Eng.tr_raw;
         Alcotest.(check int) "dirty: one clean hit" 1 tr.Eng.tr_hits;
         Alcotest.(check int) "dirty: dirt propagates (2 twins + enclosing)" 3
           tr.Eng.tr_dirty;
         Alcotest.(check int) "dirty: three dirty events"
           3
           (List.length
              (List.filter (fun k -> k = "dirty") (kinds events)));
         Alcotest.(check int) "dirty: no loop" 0
           (if tr.Eng.tr_loop then 1 else 0);
         Alcotest.(check int) "dirty: host answered twice" 2 (List.length !calls)
     | _ -> Alcotest.fail "expected a traced dirty-twin normal form")

let test_trace_cap () =
  let ii = (compile "(lambda (z) z)").Tuna_compiler.Bracket.tree in
  let art = compile (Printf.sprintf "(lambda (w) (%s w))" (t_src 2)) in
  let tr = new_trace ~cap:5 () in
  (match eval ~trace:(Some tr) ~fuel:100_000 art.Tuna_compiler.Bracket.tree [ ii ] with
   | E.Normal (_, steps) ->
       Alcotest.(check int) "cap: steps untouched" 14 steps;
       let tr, recorded, _events = trace_of tr in
       Alcotest.(check int) "cap: exactly cap events recorded" 5 recorded;
       Alcotest.(check bool) "cap: truncated flag" true tr.Eng.tr_truncated;
       Alcotest.(check int) "cap: counters keep counting" 14 tr.Eng.tr_raw
   | _ -> Alcotest.fail "expected a normal form")

let () =
  Alcotest.run "trace"
    [ ( "events"
      , [
          Alcotest.test_case "v0: one fire event per counted firing, nothing changes"
            `Quick test_v0_trace
          ; Alcotest.test_case "v1: charges are the memo table; hits skip subtrees"
              `Quick test_v1_trace_t_family
        ; Alcotest.test_case "v1: the loop event closes a divergent run"
            `Quick test_v1_trace_loop
            ; Alcotest.test_case "v1: dirty firings re-execute; dirt propagates"
                `Quick test_v1_trace_dirty
          ; Alcotest.test_case "cap: events stop, counters do not"
            `Quick test_trace_cap
        ] ) ]
