(* Derivation records (borg/deriv.borg): the seal, the offline checker,
   and the honesty boundary.

   Pure tests need no Postgres. The e2e suite is PG-gated like the
   other store suites (own scratch db tuna_test_deriv): drive a real
   run through the boundary, build the record server-side, verify it
   OFFLINE through Tuna_deriv alone, then attack it. *)
open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Api = Tuna_server.Api
module Rn = Tuna_server.Run
module Drv = Tuna_server.Deriv
module C = Tuna_compiler.Bracket
module D = Tuna_deriv.Deriv

(* -- pure helpers ----------------------------------------------------- *)

let leaf = Tuna.Tree.Leaf

(* a journal row for the echo prim answering [args] with [result] *)
let echo_row ~run_id ~seq ~args ~result ~prev =
  let row =
    { D.Journal.j_seq = seq
    ; j_callsite_path = ""
    ; j_prim = "echo"
    ; j_prim_contract = "2"
    ; j_grant_id = None
    ; j_args_ternary = Some args
    ; j_args_hash = Some (Tuna.Hash.hex_of_string args)
    ; j_result_ternary = result
    ; j_result_hash = Option.map Tuna.Hash.hex_of_string result
    ; j_error = None
    ; j_wall_ms = Some 1
    ; j_host_build = "test"
    ; j_prev_hash = prev
    ; j_row_hash = "" }
  in
  { row with
    D.Journal.j_row_hash = D.Journal.row_fingerprint ~run_id row }

(* build + seal a record from the given journal *)
let mk_record ~run_id ~program_ternary ~journal ~result_ternary ~steps =
  D.build ~run_id ~semantics:"v0"
    ~program_hash:(Tuna.Hash.hex_of_string program_ternary)
    ~program_ternary ~input_hashes:[ Tuna.Hash.hex_of_string "10" ]
    ~input_ternaries:[ "10" ] ~fuel:10000 ~size_cap:100000 ~status:D.Normal
    ~result_ternary ~result_hash:(Option.map Tuna.Hash.hex_of_string result_ternary)
    ~step_count:steps ~parent_run_id:None ~journal

let expect_verified what = function
  | D.Verified -> ()
  | D.Failed msg -> Alcotest.failf "%s must verify: %s" what msg

let expect_failed what = function
  | D.Failed _ -> ()
  | D.Verified -> Alcotest.failf "%s must FAIL" what

(* -- pure: canon, id, roundtrip --------------------------------------- *)

let test_golden_canon () =
  let d =
    D.build ~run_id:"00000000-0000-0000-0000-000000000000" ~semantics:"v0"
      ~program_hash:(Tuna.Hash.hex_of_string "22102000")
      ~program_ternary:"22102000" ~input_hashes:[ Tuna.Hash.hex_of_string "10" ]
      ~input_ternaries:[ "10" ] ~fuel:100 ~size_cap:1000 ~status:D.Normal
      ~result_ternary:(Some "0") ~result_hash:(Some (Tuna.Hash.hex_of_string "0"))
      ~step_count:2 ~parent_run_id:None ~journal:[]
  in
  (* pinned canon: field order, separators, counts.  A change here is a
     FORMAT CHANGE: bump the version string and the book. *)
  let expect =
    "tuna-deriv-v1\x1f00000000-0000-0000-0000-000000000000\x1fv0\x1f"
    ^ Tuna.Hash.hex_of_string "22102000"
    ^ "\x1f22102000\x1f1\x1f10\x1f100\x1f1000\x1fnormal\x1f0\x1f"
    ^ Tuna.Hash.hex_of_string "0"
    ^ "\x1f2\x1f\x1f0\x1f"
  in
  Alcotest.(check string) "canon is pinned" expect d.D.d_deriv_canon;
  Alcotest.(check string) "id is sha256(canon)" d.D.d_deriv_id
    (Tuna.Hash.hex_of_string d.D.d_deriv_canon);
  (* json roundtrip preserves the seal *)
  let d2 =
    match D.of_string (D.to_string d) with
    | Ok x -> x
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check string) "roundtrip keeps canon" d.D.d_deriv_canon d2.D.d_deriv_canon;
  Alcotest.(check string) "roundtrip keeps id" d.D.d_deriv_id d2.D.d_deriv_id;
  (* the effect-free record verifies on the nose: not/true = leaf, 2 steps *)
  expect_verified "not/true record" (D.verify d)

let test_rejects_tampering () =
  let reseal d =
    D.build ~run_id:d.D.d_run_id ~semantics:d.D.d_semantics
      ~program_hash:d.D.d_program_hash ~program_ternary:d.D.d_program_ternary
      ~input_hashes:d.D.d_input_hashes ~input_ternaries:d.D.d_input_ternaries
      ~fuel:d.D.d_fuel ~size_cap:d.D.d_size_cap ~status:d.D.d_status
      ~result_ternary:d.D.d_result_ternary ~result_hash:d.D.d_result_hash
      ~step_count:d.D.d_step_count ~parent_run_id:d.D.d_parent_run_id
      ~journal:d.D.d_journal
  in
  let base =
    D.build ~run_id:"r" ~semantics:"v0"
      ~program_hash:(Tuna.Hash.hex_of_string "22102000")
      ~program_ternary:"22102000" ~input_hashes:[ Tuna.Hash.hex_of_string "10" ]
      ~input_ternaries:[ "10" ] ~fuel:10000 ~size_cap:100000 ~status:D.Normal
      ~result_ternary:(Some "0") ~result_hash:(Some (Tuna.Hash.hex_of_string "0"))
      ~step_count:2 ~parent_run_id:None ~journal:[]
  in
  (* flip the claimed result without resealing: byte check 1 catches it *)
  let lied = { base with D.d_result_ternary = Some "10" } in
  expect_failed "unsealed result edit" (D.verify lied);
    (* flipped output hash, unsealed: canon + id no longer agree *)
    let flipped = { base with D.d_result_hash = Some (Tuna.Hash.hex_of_string "10") } in
    expect_failed "flipped result hash" (D.verify flipped);
    (* wrong program hash, unsealed: canon + id no longer agree *)
    let wrong_prog = { base with D.d_program_hash = Tuna.Hash.hex_of_string "0" } in
    expect_failed "wrong program hash" (D.verify wrong_prog);
  (* claim more steps and RESEAL: canon + id agree again, but replay
     identity (test 5) catches the lie - resealing does not save a lie
     about the calculus *)
  let step_lie = reseal { base with D.d_step_count = 3 } in
  expect_failed "resealed step lie" (D.verify step_lie);
  (* unsealed program edit *)
  let prog_lie = { base with D.d_program_ternary = "2210200" } in
  expect_failed "unsealed program edit" (D.verify prog_lie);
  (* a journal row whose row_hash was never honest, chain resealed
     around it: the per-row re-hash catches it *)
  let bad_row = echo_row ~run_id:"r" ~seq:0 ~args:"10" ~result:(Some "0") ~prev:D.genesis in
  let bad_row = { bad_row with D.Journal.j_row_hash = "deadbeef" } in
  let chain_lie = reseal { base with D.d_journal = [ bad_row ] } in
  expect_failed "forged row_hash" (D.verify chain_lie);
  (* broken linkage: prev_hash not the genesis *)
  let unlinked = echo_row ~run_id:"r" ~seq:0 ~args:"10" ~result:(Some "0")
      ~prev:(String.make 64 'a') in
  let link_lie = reseal { base with D.d_journal = [ unlinked ] } in
    expect_failed "unlinked chain" (D.verify link_lie);
    (* a row the program never calls: hashes honest, chain resealed,
       and the replay still refuses - unconsumed rows are a lie about
       the calculus, not just the bytes *)
    let unused = echo_row ~run_id:"r" ~seq:0 ~args:"2100" ~result:(Some "0") ~prev:D.genesis in
    let unconsumed = reseal { base with D.d_journal = [ unused ] } in
    expect_failed "unconsumed journal row" (D.verify unconsumed)

let test_reseal_is_own_record () =
  (* the honesty boundary, pinned as behavior: recompute every row hash
     and the outer seal over EDITED journal answers, claim exactly what
     the engine derives from the edited answers, and the record
     verifies - it is a valid record of its own content, with a
     DIFFERENT id.  This is why a deriv_id must be bound to a
     real-world event by the PRODUCING store over an authenticated
     surface, not by the document alone. *)
    (* the program must actually CALL the prim the journal answers:
       a pure program with a journal row is correctly refused as
       unconsumed - that refusal is pinned in test_rejects_tampering. *)
    let art = C.compile_source "(lambda (x) (prim \"echo\" x))" in
    let program = art.C.tree in
    let input = Tuna.Canon.parse "10" in
    (* what does the engine actually pass as prim args?  RECORD it from a
       real call instead of guessing: the engine passes the argument tree
       it holds, whose canon encoding here is "2100", not the input text
       "10".  Store journals the same encoding (Tuna.Canon.encode at the
       boundary); guessing this convention was the second bug this test
       shook out, and tools/deriv-debug shows the same fact standalone. *)
    let recorded_args =
      let calls = ref [] in
      let host ~site:_ ~name:_ ~args =
        calls := Tuna.Canon.encode args :: !calls;
        `Ok args
      in
      ignore
        (D.Eng.eval ~host ~mode:D.Eng.Canonical ~fuel:10000 ~size_cap:100000
           ~deadline:Float.infinity ~program [ input ]);
      List.rev !calls
    in
    let args =
      match recorded_args with
      | [ a ] -> a
      | _ -> Alcotest.fail "expected exactly one recorded prim call"
    in
    let derive journal =
      let next = ref 0 in
      let arr = Array.of_list journal in
      let host ~site:_ ~name ~args =
        let row = arr.(!next) in
        incr next;
        match row.D.Journal.j_result_ternary with
        | Some t -> `Ok (match Tuna.Canon.of_string t with Ok t -> t | Error _ -> leaf)
        | None ->
            if name <> "echo" then `Error "unexpected prim"
            else `Ok args
      in
      let outcome =
        D.Eng.eval ~host ~mode:D.Eng.Canonical ~fuel:10000 ~size_cap:100000
          ~deadline:Float.infinity ~program [ input ]
      in
      (D.outcome_ternary outcome, D.outcome_steps outcome)
    in
    let variant answer =
      let row = echo_row ~run_id:"r" ~seq:0 ~args ~result:(Some answer) ~prev:D.genesis in
      let result_ternary, steps = derive [ row ] in
      mk_record ~run_id:"r" ~program_ternary:art.C.ternary ~journal:[ row ]
        ~result_ternary ~steps
    in
    let a = variant "10" in
    let b = variant "0" in
    expect_verified "resealed variant a" (D.verify a);
    expect_verified "resealed variant b" (D.verify b);
    Alcotest.(check bool) "different answers, different ids" true
      (a.D.d_deriv_id <> b.D.d_deriv_id);
    (* truncation caught on the calculus side too: reseal around an empty
       journal and the replay refuses the prim call it has no recorded
       answer for *)
    let truncated_reseal =
      mk_record ~run_id:"r" ~program_ternary:art.C.ternary ~journal:[]
          ~result_ternary:(Some "10") ~steps:a.D.d_step_count
    in
    expect_failed "resealed truncated journal" (D.verify truncated_reseal)

(* -- PG e2e: real run -> server-built record -> offline check --------- *)

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p

let seed_program p ~caller src =
  let art = C.compile_source src in
  let ir_json = Yojson.Basic.to_string (Api.ir_json_of_artifact art) in
  S.upsert_program p ~hash:art.C.hash_hex ~ternary:art.C.ternary
    ~ir:(Some ir_json) ~created_by:caller ~source:None
  >>= fun _ -> return art

let test_e2e_record_verifies_offline () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~name:"deriv-root" ~token:"deriv-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (x) (%22102000 (prim \"echo\" x)))"
  >>= fun art ->
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ]
    ~program_hash:art.C.hash_hex ~program:art.C.tree
    ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:[ Tuna.Canon.parse "10" ] ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, _js) ->
  Alcotest.(check string) "run status normal" "normal"
    (S.Run_status.to_string row.S.r_status);
  (* server-side build *)
  Drv.of_run p ~run_id:row.S.r_id >>= fun (code, body) ->
  Alcotest.(check int) "deriv build 200" 200 code;
  (* OFFLINE verify: parse + verify through tuna_deriv alone *)
  (match D.of_string body with
   | Error e -> Alcotest.failf "served record unparseable: %s" e
   | Ok d ->
       Alcotest.(check string) "record names the run" row.S.r_id d.D.d_run_id;
       expect_verified "server-built record" (D.verify d));
  (* and a store-side journal edit (no re-seal of rows) stops the served
     record from verifying OFFLINE, without any store access *)
  Db.q_unit
    ~params:[ S.p_str row.S.r_id ]
    p
    "UPDATE journals SET result_ternary='10' WHERE run_id=$1::uuid AND seq=0"
  >>= fun () ->
  Drv.of_run p ~run_id:row.S.r_id >>= fun (code2, body2) ->
  Alcotest.(check int) "edited run still serves" 200 code2;
  (match D.of_string body2 with
   | Error e -> Alcotest.failf "edited record unparseable: %s" e
   | Ok d2 ->
       Alcotest.(check bool) "edited store record fails offline" true
         (match D.verify d2 with D.Failed _ -> true | D.Verified -> false));
  return_unit

let test_refusals () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~name:"deriv-root2" ~token:"deriv-token2" () >>= fun me ->
  (* unknown run *)
  Drv.of_run p ~run_id:"00000000-0000-0000-0000-00000000000f"
  >>= fun (code, _body) ->
  Alcotest.(check int) "unknown run 404" 404 code;
  (* a run still running: the row exists, no outcome exists yet *)
  seed_program p ~caller:(Some me.S.i_id) "(lambda (x) x)"
  >>= fun art ->
  S.insert_run p ~program_hash:art.C.hash_hex ~caller:(Some me.S.i_id)
    ~fuel:100 ~size_cap:100 ()
  >>= fun run_id ->
  Drv.of_run p ~run_id >>= fun (code2, _body2) ->
  Alcotest.(check int) "running run 409" 409 code2;
  return ()

let () =
  let lwt name f = Alcotest.test_case name `Quick f in
  let pg_gated =
    match Sys.getenv_opt "TUNA_TEST_PG" with
    | None -> []
    | Some _ ->
        [ lwt "e2e record verifies offline" test_e2e_record_verifies_offline
        ; lwt "refusals" test_refusals ]
  in
  Tuna_test_eio.run "deriv"
       [ ( "pure"
         , [ lwt "golden canon + roundtrip" (fun () ->
                 test_golden_canon (); return ())
           ; lwt "tampering rejected" (fun () ->
                 test_rejects_tampering (); return ())
           ; lwt "reseal is its own record" (fun () ->
                 test_reseal_is_own_record (); return ()) ] )
       ; ("pg", pg_gated) ]
