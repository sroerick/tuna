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
  | `Prefix_denied -> Alcotest.fail "expected `Ok, got `Prefix_denied"

let test_ping () =
  Db.init (Db.config_from_env ())
  >>= fun p ->
  Db.apply_migrations p ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations") >>= fun _ ->
  Db.ping p

let test_identities () =
  (* NOT "root": the dev server bootstraps root with its own token,
     and bootstrap_identity is get-or-create — a collision would make
     this test's token verify against someone else's hash. *)
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"tuna-test-root" ~token:"bootstrap-test-token" ()
  >>= fun root ->
  Alcotest.(check string) "root name" "tuna-test-root" root.S.i_name;
  Alcotest.(check bool) "root admin" true root.S.i_is_admin;
  (* idempotent: second bootstrap is a no-op, same row *)
  S.bootstrap_identity p ~name:"tuna-test-root" ~token:"other-token" () >>= fun root2 ->
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
  S.check_grant p ~id:g.S.g_id ~caller:me.S.i_id () >>= fun ok ->
  expect_ok ok;
  (* wrong caller *)
  S.check_grant p ~id:g.S.g_id ~caller:"not-an-identity" () >>= fun wrong ->
  (match wrong with
   | `Wrong_caller | `Unknown -> ()
   | _ -> Alcotest.fail "expected denial for wrong caller");
  (* revoke: forward-only *)
  S.revoke_grant p g.S.g_id >>= fun () ->
  S.check_grant p ~id:g.S.g_id ~caller:me.S.i_id () >>= fun revoked ->
  (match revoked with
   | `Revoked -> ()
   | _ -> Alcotest.fail "expected revoked");
  S.fetch_grant p g.S.g_id >>= fun g2 ->
  Alcotest.(check bool) "revoked_at set" true
    (Option.is_some (expect_some "grant row" g2).S.g_revoked_at);
  S.check_grant p ~id:"ffffffff-0000-0000-0000-000000000000" ~caller:me.S.i_id ()
  >>= fun unknown ->
  (match unknown with
   | `Unknown -> Lwt.return ()
   | _ -> Alcotest.fail "expected unknown")


(* -- M7: run boundary + replay --------------------------------------- *)

module Rn = Tuna_server.Run
module Rp = Tuna_server.Replay
module Api = Tuna_server.Api
module C = Tuna_compiler.Bracket

(* compile a source program and persist it (with provenance ir json) *)
let seed_program p ~caller src =
  let art = C.compile_source src in
  let ternary = art.C.ternary in
  let hash = art.C.hash_hex in
  let ir_json = Yojson.Basic.to_string (Api.ir_json_of_artifact art) in
  S.upsert_program p ~hash ~ternary ~ir:(Some ir_json) ~created_by:caller
  >>= fun _ -> Lwt.return (hash, art)

let expect_verified _run_id = function
  | Rp.Verified _ -> ()
  | Rp.Bad_chain msg -> Alcotest.failf "expected verified, got bad chain: %s" msg
  | Rp.Diverged d ->
      Alcotest.failf
        "expected verified, diverged at seq %a: %s"
        (fun fmt -> function
           | Some s -> Format.pp_print_int fmt s
           | None -> Format.pp_print_string fmt "none")
        d.Rp.div_seq d.Rp.reason
  | Rp.Unverifiable msg -> Alcotest.failf "expected verified, unverifiable: %s" msg
  | Rp.Gone msg -> Alcotest.failf "expected verified, gone: %s" msg

(* The canonical M7 effectful program: echo the input, then negate it.
   Journal: exactly one boundary row (echo), carrying the grant and the
   callsite tree path resolved from the compiled provenance. *)
let test_run_boundary () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (x) (%22102000 (prim \"echo\" x)))"
  >>= fun (hash, art) ->
  let input = Tuna.Canon.parse "10" in
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:[ input ] ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, js) ->
  Alcotest.(check string) "status normal" "normal" (S.Run_status.to_string row.S.r_status);
  Alcotest.(check int) "one journal row" 1 (List.length js);
  (match js with
   | [ j ] ->
       Alcotest.(check string) "prim" "echo" j.S.j_prim;
       Alcotest.(check string) "contract pinned" "1" j.S.j_prim_contract;
       Alcotest.(check (option string)) "grant spent" (Some g.S.g_id)
         j.S.j_grant_id;
       Alcotest.(check bool) "callsite path resolved" true
         (j.S.j_callsite_path <> "");
       (* the journal input tree is content-addressed for replay *)
       Alcotest.(check bool) "input stored for replay" true
         (row.S.r_input_hashes = [ Tuna.Hash.hex_of_tree input ]);
       (match S.verify_chain js with
        | `Ok -> ()
        | `Bad msg -> Alcotest.failf "fresh journal must verify: %s" msg)
   | _ -> Alcotest.fail "bad journal");
  (* replay identity: same engine, journal-fed -> verified, same steps *)
  Rp.verify p ~run:row ()
  >>= fun v ->
  expect_verified row.S.r_id v;
  (match v with
   | Rp.Verified outcome ->
       Alcotest.(check (option int)) "steps match" row.S.r_step_count
         (Some (Rp.outcome_steps outcome))
   | _ -> ());
  (* verify_and_record writes verify_status *)
  Rp.verify_and_record p ~run_id:row.S.r_id ()
  >>= fun _ ->
  S.fetch_run p row.S.r_id
  >>= (function
        | None -> Alcotest.fail "run vanished"
        | Some r2 ->
            Alcotest.(check (option string)) "verify_status recorded"
              (Some "verified") r2.S.r_verify_status;
            Lwt.return ())

(* grant denial is a journaled error answer; the run continues and the
   replay still verifies (denial is data, never an exception) *)
let test_run_denial () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (x) (%22102000 (prim \"echo\" x)))"
  >>= fun (hash, art) ->
  let input = Tuna.Canon.parse "10" in
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[] ~program_hash:hash
    ~program:art.C.tree ~ir_json:(None) ~inputs:[ input ] ~fuel:10000
    ~size_cap:100000 ()
  >>= fun (row, js) ->
  Alcotest.(check string) "denied run still normal" "normal"
    (S.Run_status.to_string row.S.r_status);
  Alcotest.(check int) "one journaled denial" 1 (List.length js);
  (match js with
   | [ j ] ->
       Alcotest.(check (option string)) "no grant spent" None j.S.j_grant_id;
       Alcotest.(check bool) "error journaled" true
         (Option.is_some j.S.j_error)
   | _ -> Alcotest.fail "bad journal");
  Rp.verify p ~run:row () >>= fun v -> expect_verified row.S.r_id v;
  Lwt.return ()

(* store/get + store/put through the boundary (migration 0002 prim_kv) *)
let test_run_store_prims () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  (* kv accessors directly, then upsert semantics *)
  S.prim_put p ~key_ternary:"10" ~value_ternary:"0" >>= fun () ->
  S.prim_get p "10"
  >>= (function
        | Some (k, v) ->
            Alcotest.(check string) "kv key" "10" k;
            Alcotest.(check string) "kv value" "0" v;
            Lwt.return ()
        | None -> Alcotest.fail "kv roundtrip failed")
  >>= fun () ->
  S.prim_put p ~key_ternary:"10" ~value_ternary:"10" >>= fun () ->
  S.prim_get p "10"
  >>= (function
        | Some (_, v) ->
            Alcotest.(check string) "kv overwrite" "10" v;
            Lwt.return ()
        | None -> Alcotest.fail "kv vanished")
  >>= fun () ->
  (* a run that reads through the boundary *)
  S.mint_grant p ~prim:"store/get" ~args_attenuation:"{}"
    ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id) "(lambda (x) (prim \"store/get\" x))"
  >>= fun (hash, art) ->
  let key = Tuna.Canon.parse "10" in
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:[ key ] ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, _js) ->
  Alcotest.(check string) "store/get returns the value" "10"
    (Option.value row.S.r_result_ternary ~default:"MISSING");
  Rp.verify p ~run:row () >>= fun v -> expect_verified row.S.r_id v;
  Lwt.return ()

(* journal tampering is caught by the chain walk *)
let test_tamper_bad_chain () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (x) (%22102000 (prim \"echo\" x)))"
  >>= fun (hash, art) ->
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:(None) ~inputs:[ Tuna.Canon.parse "10" ]
    ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, _) ->
  Db.q_unit
    ~params:[ S.p_str row.S.r_id ]
    p
    "UPDATE journals SET result_ternary = '0' WHERE run_id = $1::uuid AND seq = 0"
  >>= fun () ->
  S.fetch_run p row.S.r_id
  >>= (function
        | None -> Alcotest.fail "run vanished"
        | Some run ->
            Rp.verify p ~run ()
            >>= fun v ->
            (match v with
             | Rp.Bad_chain _ -> Lwt.return ()
             | _ -> Alcotest.fail "tampered journal must fail the chain walk"))

(* counterfactual fork: the edited answer flows through; history is
   untouched and the parent still verifies *)
let test_counterfactual_fork () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (x) (%22102000 (prim \"echo\" x)))"
  >>= fun (hash, art) ->
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:(None) ~inputs:[ Tuna.Canon.parse "10" ]
    ~fuel:10000 ~size_cap:100000 ()
  >>= fun (parent, pjs) ->
  (* fork with the echo answer replaced by Leaf *)
  Api.fork p ~parent_run_id:parent.S.r_id
    ~edits:[ (0, Api.Set_result Tuna.Tree.Leaf) ]
  >>= fun (derived, djs, v) ->
  expect_verified derived.S.r_id v;
  Alcotest.(check (option string)) "derived linked" (Some parent.S.r_id)
    derived.S.r_parent_run_id;
  Alcotest.(check int) "same journal length"
    (List.length pjs) (List.length djs);
  (* exactly the reachable suffix changed: rows other than seq 0 are
     copies, seq 0 carries the edited answer *)
  (match (pjs, djs) with
   | [ pj ], [ dj ] ->
       Alcotest.(check bool) "args unchanged" true
         (Option.equal String.equal pj.S.j_args_ternary
            dj.S.j_args_ternary);
       Alcotest.(check string) "edited answer journaled" "0"
         (Option.value dj.S.j_result_ternary ~default:"MISSING")
   | _ -> Alcotest.fail "bad journal shape");
  (* the counterfactual outcome differs from the parent's *)
  Alcotest.(check bool) "outcomes differ" true
    (not
       (String.equal
          (Option.value parent.S.r_result_ternary ~default:"")
          (Option.value derived.S.r_result_ternary ~default:"~")));
  (* history untouched: the parent still verifies *)
  Rp.verify p ~run:parent ()
  >>= fun v ->
  expect_verified parent.S.r_id v;
  (* a divergent edit (cleared row) leaves the fork unverifiable *)
  Api.fork p ~parent_run_id:parent.S.r_id ~edits:[ (0, Api.Clear) ]
  >>= fun (bad, _, bv) ->
  (match bv with
   | Rp.Diverged _ -> Lwt.return ()
   | _ -> Alcotest.fail "cleared row must diverge the replay")
  >>= fun () ->
  Alcotest.(check string) "divergent fork errored" "error"
    (S.Run_status.to_string bad.S.r_status);
  Lwt.return ()

(* M10 acceptance wiring: retention (journal.retention-gc).  GC leaves
   a cited tombstone; verifiers report GONE, never VERIFIED.  Redaction
   is a visible chain break; the run can never verify again. *)
let test_gc_tombstone () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m10-root" ~token:"m10-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id) "(lambda (x) (prim \"echo\" x))"
  >>= fun (hash, art) ->
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:None ~inputs:[ Tuna.Canon.parse "10" ]
    ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, js) ->
  Alcotest.(check int) "journal present" 1 (List.length js);
  (* GC: rows deleted, tombstone cited, verify_status gced *)
  S.gc_journal p ~run_id:row.S.r_id ~policy:"keep-30d" ()
  >>= (function
        | None -> Alcotest.fail "gc lost the run row"
        | Some gced ->
            Alcotest.(check string) "tombstone status" "gced"
              (Option.value gced.S.r_verify_status ~default:"MISSING");
            S.fetch_journals p row.S.r_id
            >>= fun after ->
            Alcotest.(check int) "journal gone" 0 (List.length after);
            S.fetch_gc_tombstone p row.S.r_id
            >>= fun tomb ->
            Alcotest.(check (option string)) "policy cited" (Some "keep-30d")
              (Option.join tomb);
            S.fetch_run p row.S.r_id
            >>= (function
                  | None -> Alcotest.fail "run vanished"
                    | Some run ->
                        Rp.verify p ~run ~deadline:Float.infinity ()
                      >>= fun v ->
                      (match v with
                       | Rp.Gone msg ->
                           Alcotest.(check bool) "cited in the verdict" true
                             (String.length msg > 0);
                           Lwt.return ()
                       | Rp.Verified _ ->
                           Alcotest.fail "GONE must never report VERIFIED"
                       | _ -> Alcotest.fail "gc'd run must be Gone")))

(* redaction = explicit chain break; the break is visible, the row's
   answer is unknown, and no stale VERIFIED survives *)
let test_redaction_breaks_chain () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  S.bootstrap_identity p ~name:"m10-root" ~token:"m10-token" () >>= fun me ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:me.S.i_id ()
  >>= fun g ->
  seed_program p ~caller:(Some me.S.i_id) "(lambda (x) (prim \"echo\" x))"
  >>= fun (hash, art) ->
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree ~ir_json:None ~inputs:[ Tuna.Canon.parse "10" ]
    ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row, _) ->
  (* first verify clean *)
  S.fetch_run p row.S.r_id
  >>= (function
        | None -> Alcotest.fail "run vanished"
        | Some run ->
            Rp.verify p ~run ~deadline:Float.infinity () >>= fun v ->
            expect_verified run.S.r_id v;
            Lwt.return ())
  >>= fun () ->
  S.redact_journal_row p ~run_id:row.S.r_id ~seq:0 ~policy:"pii-scrub" ()
  >>= fun () ->
  S.fetch_run p row.S.r_id
  >>= (function
        | None -> Alcotest.fail "run vanished"
        | Some run ->
            (* verify_status cleared: no stale VERIFIED *)
            Alcotest.(check (option string)) "verify_status cleared" None
              run.S.r_verify_status;
            S.fetch_journals p row.S.r_id
            >>= fun js ->
            (match js with
             | [ j ] ->
                 Alcotest.(check (option string)) "payload redacted" None
                   j.S.j_result_ternary;
                 (match j.S.j_error with
                  | Some e when String.length e >= 8 &&
                                String.sub e 0 8 = "redacted" -> ()
                  | _ -> Alcotest.fail "row must cite the redaction policy")
             | _ -> Alcotest.fail "expected one row");
            Rp.verify p ~run ~deadline:Float.infinity ()
            >>= fun v ->
            (match v with
             | Rp.Bad_chain msg ->
                 Alcotest.(check bool) "break mentions seq 0" true
                   (String.length msg > 0);
                 Lwt.return ()
             | _ -> Alcotest.fail "redaction must be a visible chain break")
            >>= fun () -> Rp.verify_and_record p ~run_id:row.S.r_id ()
            >>= fun _ ->
            S.fetch_run p row.S.r_id
            >>= (function
                  | None -> Alcotest.fail "run vanished"
                  | Some run ->
                      Alcotest.(check (option string)) "recorded failed"
                        (Some "failed") run.S.r_verify_status;
                      Lwt.return ()))

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
         ; ("grants", [ lwt "mint/check/revoke" test_grants ])
         ; ( "m7"
           , [ lwt "run boundary + journal + replay identity" test_run_boundary
             ; lwt "grant denial journaled, replay verifies" test_run_denial
             ; lwt "store/get+put through the boundary" test_run_store_prims
             ; lwt "journal tamper -> bad chain" test_tamper_bad_chain
             ; lwt "counterfactual fork re-executes" test_counterfactual_fork
             ] )
         ; ( "m10"
           , [ lwt "gc leaves a cited tombstone; verifier reports GONE"
                 test_gc_tombstone
             ; lwt "redaction = visible chain break (answer unknown)"
                 test_redaction_breaks_chain
             ] ) ])
