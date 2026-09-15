(* M5 store-layer tests.  Integration tests require Postgres
   (scripts/dev.sh start-pg); skipped silently otherwise via
   scripts/test-store.sh gating TUNA_TEST_PG=1. *)
open Lwt.Infix

module Db = Tuna_store.Db
module S = Tuna_store.Store

(* a content-addressed scratch program for FK-valid runs *)
let leaf_hash = Tuna.Hash.hex_of_tree (Tuna.Canon.parse "0")

let expect_some what (x : 'a option) : 'a =
  match x with Some v -> v | None -> Alcotest.fail ("expected: " ^ what)

let expect_ok = function
  | `Ok -> ()
  | `Revoked -> Alcotest.fail "expected `Ok, got `Revoked"
  | `Wrong_caller -> Alcotest.fail "expected `Ok, got `Wrong_caller"
  | `Unknown -> Alcotest.fail "expected `Ok, got `Unknown"

let test_ping () =
  Db.init (Db.config_from_env ())
  >>= fun p ->
  Db.apply_migrations p ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations") >>= fun _ ->
  Db.ping p

let test_identities () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"root" ~token:"bootstrap-test-token" ()
  >>= fun root ->
  Alcotest.(check string) "root name" "root" root.S.i_name;
  Alcotest.(check bool) "root admin" true root.S.i_is_admin;
  (* idempotent: second bootstrap is a no-op, same row *)
  S.bootstrap_identity p ~name:"root" ~token:"other-token" () >>= fun root2 ->
  Alcotest.(check string) "same row" root.S.i_id root2.S.i_id;
  (* verify by token *)
  S.verify_token p "bootstrap-test-token" >>= fun found ->
  let i = expect_some "token verify" found in
  Alcotest.(check string) "token->id" root.S.i_id i.S.i_id;
  S.verify_token p "wrong-token" >>= fun wrong ->
  Alcotest.(check int) "wrong token" 0 (Option.fold ~some:(fun _ -> 1) ~none:0 wrong);
  Lwt.return ()

(* jsonb round-trips through pgx with its own spacing; compare parsed *)
let json_equal a b =
  match Yojson.Safe.from_string a, Yojson.Safe.from_string b with
  | `String a, `String b -> String.equal a b
  | a, b -> Yojson.Safe.equal a b

let json_equal_opt a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> json_equal a b
  | _ -> false

let test_programs () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  let not_hash = Tuna.Hash.hex_of_tree (Tuna.Canon.parse "22102000") in
  S.upsert_program p ~hash:not_hash ~ternary:"22102000"
    ~ir:(Some "{\"kind\":\"test\"}") ~created_by:None
  >>= fun prog ->
  Alcotest.(check string) "ternary" "22102000" prog.S.p_ternary;
  (* jsonb text spacing is server-defined; compare parsed JSON *)
  Alcotest.(check bool) "ir" true (json_equal "{\"kind\":\"test\"}" (Option.value prog.S.p_ir ~default:"null"));
  (* upsert is get-or-create: second call with different ir does not clobber *)
  S.upsert_program p ~hash:not_hash ~ternary:"22102000" ~ir:None ~created_by:None
  >>= fun prog2 ->
  Alcotest.(check string) "same hash" prog.S.p_hash prog2.S.p_hash;
  Alcotest.(check bool) "ir kept" true (json_equal_opt prog.S.p_ir prog2.S.p_ir);
  S.fetch_program p not_hash >>= fun fetched ->
  Alcotest.(check bool) "fetch found" true (Option.is_some fetched);
  Lwt.return ()

let test_runs () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  let not_hash = Tuna.Hash.hex_of_tree (Tuna.Canon.parse "22102000") in
  S.upsert_program p ~hash:not_hash ~ternary:"22102000" ~ir:None ~created_by:None
  >>= fun _ ->
  S.insert_run p ~program_hash:not_hash ~inputs:[ "0"; "10" ] ~caller:None
    ~parent_run_id:None ~fuel:500 ~size_cap:100 ()
  >>= fun run_id ->
  S.fetch_run p run_id >>= fun found ->
  let r = expect_some "run row" found in
  Alcotest.(check string) "status running" "running" (S.Run_status.to_string r.S.r_status);
  Alcotest.(check int) "fuel" 500 r.S.r_fuel;
  Alcotest.(check (list string)) "inputs" [ "0"; "10" ] r.S.r_input_hashes;
  (* finish the run: not true = leaf, 2 steps *)
  S.update_run_result p ~id:run_id ~status:S.Run_status.Normal
    ~result_ternary:"0" ~step_count:2 () >>= fun () ->
  S.fetch_run p run_id >>= fun again ->
  let r = expect_some "run row" again in
  Alcotest.(check (option string)) "result" (Some "0") r.S.r_result_ternary;
  Alcotest.(check (option int)) "steps" (Some 2) r.S.r_step_count;
  Alcotest.(check (option string)) "result hash" (Some (Tuna.Hash.hex_of_string "0"))
    r.S.r_result_hash;
  S.list_runs p ~program:(Some not_hash) ~limit:10 () >>= fun rs ->
  Alcotest.(check bool) "listed" true (List.exists (fun r -> r.S.r_id = run_id) rs);
  Lwt.return ()

let test_journals () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.upsert_program p ~hash:leaf_hash ~ternary:"0" ~ir:None ~created_by:None
  >>= fun _ ->
  S.insert_run p ~program_hash:leaf_hash ~caller:None ~parent_run_id:None
    ~fuel:1000 ~size_cap:1000 ()
  >>= fun run_id ->
  let ev seq args result =
    { S.e_callsite_path = "/0/1"
    ; e_prim = "echo"
    ; e_prim_contract = "1"
    ; e_grant_id = None
    ; e_args_ternary = args
    ; e_result_ternary = result
    ; e_error = None
    ; e_wall_ms = Some seq }
  in
  S.append_journal p ~run_id (ev 1 (Some "0") (Some "0")) >>= fun (s0, _h0) ->
  S.append_journal p ~run_id (ev 2 (Some "10") (Some "10")) >>= fun (s1, _h1) ->
  S.append_journal p ~run_id (ev 3 None None) >>= fun (s2, _h2) ->
  Alcotest.(check (list int)) "seqs" [ 0; 1; 2 ] [ s0; s1; s2 ];
  S.fetch_journals p run_id >>= fun js ->
  Alcotest.(check int) "row count" 3 (List.length js);
  Alcotest.(check string) "genesis prev" (String.make 64 '0') (List.hd js).S.j_prev_hash;
  (match S.verify_chain js with
   | `Ok -> ()
   | `Bad msg -> Alcotest.fail ("chain should verify: " ^ msg));
  (match js with
   | [ a; b; _c ] -> Alcotest.(check string) "link" a.S.j_row_hash b.S.j_prev_hash
   | _ -> Alcotest.fail "bad journal shape");
  (* tamper: mutate a payload in place -> chain walk must fail *)
  Db.q_unit
    ~params:[ S.p_str run_id ]
    p
    "UPDATE journals SET args_ternary = '2' WHERE run_id = $1::uuid AND seq = 1"
  >>= fun () ->
  S.fetch_journals p run_id >>= fun js' ->
  (match S.verify_chain js' with
   | `Bad _ -> Lwt.return ()
   | `Ok -> Alcotest.fail "tampered journal verified")

let test_grants () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"root" ~token:"grant-test-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  Alcotest.(check string) "prim" "echo" g.S.g_prim;
  S.check_grant p ~id:g.S.g_id ~caller:me.S.i_id >>= fun ok ->
  expect_ok ok;
  (* wrong caller *)
  S.check_grant p ~id:g.S.g_id ~caller:"not-an-identity" >>= fun wrong ->
  (match wrong with
   | `Wrong_caller | `Unknown -> ()
   | _ -> Alcotest.fail "expected denial for wrong caller");
  (* revoke: forward-only *)
  S.revoke_grant p g.S.g_id >>= fun () ->
  S.check_grant p ~id:g.S.g_id ~caller:me.S.i_id >>= fun revoked ->
  (match revoked with
   | `Revoked -> ()
   | _ -> Alcotest.fail "expected revoked");
  S.fetch_grant p g.S.g_id >>= fun g2 ->
  Alcotest.(check bool) "revoked_at set" true
    (Option.is_some (expect_some "grant row" g2).S.g_revoked_at);
  S.check_grant p ~id:"ffffffff-0000-0000-0000-000000000000" ~caller:me.S.i_id
  >>= fun unknown ->
  (match unknown with
   | `Unknown -> Lwt.return ()
   | _ -> Alcotest.fail "expected unknown")

let () =
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> print_endline "store tests skipped (TUNA_TEST_PG not set)"
  | Some _ ->
    let lwt _name f = Alcotest_lwt.test_case _name `Quick (fun _sw () -> f ()) in
    Lwt_main.run
      (Alcotest_lwt.run "store"
         [ ("ping", [ lwt "live" test_ping ])
         ; ("identities", [ lwt "bootstrap+verify" test_identities ])
         ; ("programs", [ lwt "upsert+fetch" test_programs ])
         ; ("runs", [ lwt "insert/update/fetch/list" test_runs ])
         ; ("journals", [ lwt "append+chain+tamper" test_journals ])
         ; ("grants", [ lwt "mint/check/revoke" test_grants ]) ])
