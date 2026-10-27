(* M10 tree substrate tests: derived path index, CAS writes, the
   chained op log, prefix grants at the prim boundary, ns_fork, and
   rewind-as-fold.  Integration tests require Postgres (scripts/dev.sh
   start-pg); skipped silently otherwise via scripts/test-store.sh
   gating TUNA_TEST_PG=1. *)
open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Tp = Tuna_server.Tree_prims
module Rw = Tuna_server.Rewind
module Rn = Tuna_server.Run
module Rp = Tuna_server.Replay
module Api = Tuna_server.Api
module C = Tuna_compiler.Bracket

(* every test opens with this: connect to the scratch db the gate
   script selected and bring the migrations up before touching tables *)
let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p


let expect_cas_ok what = function
  | `Ok v -> v
  | `Conflict -> Alcotest.fail (what ^ ": unexpected conflict")
  | `Absent -> Alcotest.fail (what ^ ": unexpected absent")

let expect_cas_conflict what = function
  | `Conflict -> ()
  | `Ok _ -> Alcotest.fail (what ^ ": expected conflict, got ok")
  | `Absent -> Alcotest.fail (what ^ ": expected conflict, got absent")

let expect_cas_absent what = function
  | `Absent -> ()
  | `Ok _ -> Alcotest.fail (what ^ ": expected absent, got ok")
  | `Conflict -> Alcotest.fail (what ^ ": expected absent, got conflict")

(* compile a source program and persist it (with provenance ir json) *)
let seed_program p ~caller src =
  let art = C.compile_source src in
  let ternary = art.C.ternary in
  let hash = art.C.hash_hex in
  let ir_json = Yojson.Basic.to_string (Api.ir_json_of_artifact art) in
  S.upsert_program p ~hash ~ternary ~ir:(Some ir_json) ~created_by:caller ~source:None
  >>= fun _ -> return (hash, art)

(* -- index basics ------------------------------------------------------ *)

let test_path_roundtrip () =
  setup () >>= fun p ->
  let v1 = Tuna.Hash.hex_of_string "10" in
  S.value_put p ~hash:v1 ~ternary:"10" >>= fun () ->
  S.path_put p ~path:"t1/a/x" ~value_hash:v1 ~owner:"owner-1"
  >>= fun v ->
  Alcotest.(check int64) "first put is version 1" 1L v;
  S.path_get p ~path:"t1/a/x"
  >>= (function
        | None -> Alcotest.fail "path vanished"
        | Some e ->
            Alcotest.(check string) "value hash" v1 e.S.tp_value_hash;
            Alcotest.(check int64) "version" 1L e.S.tp_version;
            Alcotest.(check string) "owner" "owner-1" e.S.tp_owner;
            return ())
  >>= fun () ->
  let v2 = Tuna.Hash.hex_of_string "0" in
  S.value_put p ~hash:v2 ~ternary:"0" >>= fun () ->
  S.path_put p ~path:"t1/a/x" ~value_hash:v2 ~owner:"owner-1"
  >>= fun v ->
  Alcotest.(check int64) "second put bumps to 2" 2L v;
  S.value_fetch p v1
  >>= (function
        | Some t ->
            Alcotest.(check string) "value roundtrip" "10" t;
            return ()
        | None -> Alcotest.fail "value vanished")

let test_cas () =
  setup () >>= fun p ->
  let vh = Tuna.Hash.hex_of_string "10" in
  S.value_put p ~hash:vh ~ternary:"10" >>= fun () ->
  (* create: expected_version None inserts at version 1 *)
  S.path_put_cas p ~path:"t2/b/y" ~value_hash:vh ~owner:"o" ~expected_version:None
    ~expected_hash:None
  >>= fun v ->
  Alcotest.(check int64) "create at 1" 1L (expect_cas_ok "create" v);
  (* create again -> Conflict (path exists) *)
  S.path_put_cas p ~path:"t2/b/y" ~value_hash:vh ~owner:"o" ~expected_version:None
    ~expected_hash:None
  >>= fun v ->
  expect_cas_conflict "re-create" v;
  (* update at a wrong version -> Conflict *)
  S.path_put_cas p ~path:"t2/b/y" ~value_hash:vh ~owner:"o"
    ~expected_version:(Some 5L) ~expected_hash:None
  >>= fun v -> expect_cas_conflict "stale version" v;
  (* update at the right version -> Ok, version+1 *)
  S.path_put_cas p ~path:"t2/b/y" ~value_hash:vh ~owner:"o"
    ~expected_version:(Some 1L) ~expected_hash:None
  >>= fun v ->
  Alcotest.(check int64) "cas bumps to 2" 2L (expect_cas_ok "cas" v);
  (* cas against a path with no row -> Absent *)
  S.path_put_cas p ~path:"t2/b/none" ~value_hash:vh ~owner:"o"
    ~expected_version:(Some 1L) ~expected_hash:None
  >>= fun v -> expect_cas_absent "absent" v;
  (* cas with an expected value hash that does not match -> Conflict *)
  S.path_put_cas p ~path:"t2/b/y" ~value_hash:vh ~owner:"o"
    ~expected_version:(Some 2L) ~expected_hash:(Some (Tuna.Hash.hex_of_string "0"))
  >>= fun v -> expect_cas_conflict "stale value hash" v;
  return ()

let test_prefix_list () =
  setup () >>= fun p ->
  let vh = Tuna.Hash.hex_of_string "0" in
  S.value_put p ~hash:vh ~ternary:"0" >>= fun () ->
  let put path = S.path_put p ~path ~value_hash:vh ~owner:"o" >>= fun _ -> return () in
  put "t3/c/3" >>= fun () ->
  put "t3/c/1" >>= fun () ->
  put "t3/c/2" >>= fun () ->
  put "t3/d/9" >>= fun () ->
  S.path_list p ~prefix:"t3/c" ()
  >>= fun entries ->
  Alcotest.(check (list string)) "prefix range, byte-wise ordered"
    [ "t3/c/1"; "t3/c/2"; "t3/c/3" ]
    (List.map (fun e -> e.S.tp_path) entries);
  (* windowed reads (board.borg L6): [after] is an exclusive lower
     bound; the empty default keeps the historical result identical. *)
  S.path_list p ~prefix:"t3/c" ~limit:2 ()
  >>= fun w ->
  Alcotest.(check (list string)) "limit window from the range head"
    [ "t3/c/1"; "t3/c/2" ] (List.map (fun e -> e.S.tp_path) w);
  S.path_list p ~prefix:"t3/c" ~after:"t3/c/1" ~limit:10 ()
  >>= fun w2 ->
  Alcotest.(check (list string)) "cursor window is exclusive"
    [ "t3/c/2"; "t3/c/3" ] (List.map (fun e -> e.S.tp_path) w2);
  S.path_list p ~prefix:"t3/c" ~after:"t3/d/9" ~limit:10 ()
  >>= fun w3 ->
  Alcotest.(check (list string)) "cursor past the range is empty" []
    (List.map (fun e -> e.S.tp_path) w3);
  return ()

(* -- grant prefix at the prim boundary --------------------------------- *)

let expect_verified_ok run_id = function
  | Rp.Verified _ -> ()
  | Rp.Bad_chain msg -> Alcotest.failf "expected verified, got bad chain: %s" msg
  | Rp.Diverged d ->
      Alcotest.failf "expected verified, diverged: %s" d.Rp.reason
  | Rp.Unverifiable msg ->
      Alcotest.failf "run %s unverifiable: %s" run_id msg
  | Rp.Gone msg ->
      Alcotest.failf "run %s gced (retention GC): %s" run_id msg

let test_grant_prefix_boundary () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~name:"m7-root" ~token:"m7-token" () >>= fun me ->
  S.mint_grant p ~prim:"tree/put" ~args_attenuation:"{}"
    ~path_prefix:(Some "t4/a") ~caller:me.S.i_id ()
  >>= fun g ->
  (* (prim "name" a b): each operand is a SEPARATE arg -- the engine
     folds application, so two inputs bind the lambda's two params and
     the host sees args = [path; value] *)
  seed_program p ~caller:(Some me.S.i_id)
    "(lambda (p v) (prim \"tree/put\" p v))"
  >>= fun (hash, art) ->
  let put_inputs path value_ternary =
    [ Tuna.Cstr.encode path; Tuna.Canon.parse value_ternary ]
  in
  (* ALLOW: path inside the grant prefix *)
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree
    ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:(put_inputs "t4/a/one" "10") ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row_ok, js_ok) ->
  Alcotest.(check string) "allowed run normal" "normal"
    (S.Run_status.to_string row_ok.S.r_status);
   (match js_ok with
    | [ j ] ->
        Alcotest.(check bool) "allowed run journaled no error" false
          (Option.is_some j.S.j_error)
    | _ -> Alcotest.fail "bad journal shape");
  (match Tuna.Cstr.decode (Tuna.Canon.parse (Option.value row_ok.S.r_result_ternary ~default:"")) with
   | Some "version:1" -> ()
   | other -> Alcotest.failf "expected version:1 answer, got %s"
                (Option.value other ~default:"undecodable"));
  Rp.verify p ~run:row_ok () >>= fun v -> expect_verified_ok row_ok.S.r_id v;
  (* DENY: cross-prefix put is a journaled error answer, run continues *)
  Rn.execute p ~caller:me.S.i_id ~grant_ids:[ g.S.g_id ] ~program_hash:hash
    ~program:art.C.tree
    ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:(put_inputs "t4/b/two" "10") ~fuel:10000 ~size_cap:100000 ()
  >>= fun (row_denied, js_denied) ->
  Alcotest.(check string) "denied run still normal" "normal"
    (S.Run_status.to_string row_denied.S.r_status);
  (match js_denied with
   | [ j ] ->
       Alcotest.(check bool) "denial journaled as error" true
         (Option.is_some j.S.j_error);
         let e = Option.value j.S.j_error ~default:"" in
         Alcotest.(check string) "denial names the prefix"
           "grant denial (path outside grant prefix) for prim tree/put" e
   | _ -> Alcotest.fail "bad journal shape");
  (* the substrate op log carries both: an effect row and a denial row *)
  S.ops_fold p ~prefix:"t4" ~from_seq:0L ()
  >>= fun ops ->
  (* "effect" is a keyword in ocaml 5 — the binding is eff_row *)
  let eff_row =
    List.filter
      (fun (o : S.tree_op) -> o.S.o_op = "put" && o.S.o_path = "t4/a/one")
      ops
  in
  let denial =
    List.filter
      (fun (o : S.tree_op) -> o.S.o_op = "put" && o.S.o_path = "t4/b/two")
      ops
  in
  (match eff_row with
   | [ o ] ->
       Alcotest.(check bool) "effect row carries value hash" true
         (Option.is_some o.S.o_value_hash);
       Alcotest.(check (option int64)) "effect row version" (Some 1L) o.S.o_version
   | n -> Alcotest.failf "expected one effect row, got %d" (List.length n));
  (match denial with
   | [ o ] ->
       Alcotest.(check (option string)) "denial row has no value" None o.S.o_value_hash;
       Alcotest.(check (option int64)) "denial row has no version" None o.S.o_version
   | n -> Alcotest.failf "expected one denial row, got %d" (List.length n));
   return ()


(* -- mint surface: scope and prim are interface facts ------------------ *)

(* The mint is where a non-admin obtains a tree capability, so these are
   the tenancy asserts (2026-10-27): a path-addressed prim needs an
   explicit prefix, the prefix must sit inside authority the caller
   already covers, an operator seeds that authority on behalf of a
   member, and a NULL row covers NO path - the last one is why one
   store-wide row can no longer stand in for a garden. *)
let test_grant_mint_scope () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~is_admin:false ~name:"mint-root" ~token:"mint-token" ()
  >>= fun me ->
  let auth =
    { Api.auth_id = me.S.i_id
    ; Api.auth_name = me.S.i_name
    ; Api.auth_is_admin = me.S.i_is_admin }
  in
  let body s = Yojson.Basic.from_string s in
  let jfield k (j : Yojson.Basic.t) : Yojson.Basic.t =
    match j with
    | `Assoc kvs -> (
        match List.assoc_opt k kvs with Some v -> v | None -> `Null)
    | _ -> `Null
  in
  let str k j = match jfield k j with `String s -> s | _ -> "" in
  (* a pathless capability mints unscoped, and covers no path *)
  Api.grant_mint_core p auth (body {|{"prim":"echo"}|}) >>= fun (code, j) ->
  Alcotest.(check int) "pathless mint answers 201" 201 code;
  Alcotest.(check bool) "pathless mint stores a null prefix" true
    (jfield "path_prefix" j = `Null);
  S.has_covering_grant p me.S.i_id "t6/ok/x" >>= fun covered ->
  Alcotest.(check bool) "a NULL row covers no path" false covered;
  (* a path capability needs scope, and scope must already be authority *)
  Api.grant_mint_core p auth (body {|{"prim":"tree/put"}|}) >>= fun (code, _) ->
  Alcotest.(check int) "path-addressed mint without a prefix is refused" 400 code;
  Api.grant_mint_core p auth (body {|{"prim":"tree/put","path_prefix":"t6/ok/"}|})
  >>= fun (code, _) ->
  Alcotest.(check int) "a prefix outside the caller's authority is refused" 403 code;
  (* the operator seeds the garden; the member then narrows inside it *)
    S.mint_identity p ~is_admin:true ~name:"mint-seed-root" ~token:"mint-seed-token"
      ()
    >>= (function
          | Ok op -> return op
          | Error `Exists -> Alcotest.fail "mint-seed-root: exists"
          | Error (`Invalid_name m) | Error (`Name_conflict m) ->
              Alcotest.fail ("mint-seed-root: " ^ m))
    >>= fun op ->
  let op_auth =
    { Api.auth_id = op.S.i_id
    ; Api.auth_name = op.S.i_name
    ; Api.auth_is_admin = true }
  in
  Api.grant_mint_core p op_auth
    (body
       (Printf.sprintf
          {|{"prim":"tree/put","path_prefix":"t6/","identity":"%s"}|} me.S.i_id))
  >>= fun (code, j) ->
  Alcotest.(check int) "an admin seeds a garden on behalf" 201 code;
  Alcotest.(check string) "the seeded row belongs to the member" me.S.i_id
    (str "caller" j);
  Api.grant_mint_core p auth (body {|{"prim":"tree/put","path_prefix":"t6/ok/"}|})
  >>= fun (code, j) ->
  Alcotest.(check int) "a member narrows inside its own authority" 201 code;
  Alcotest.(check string) "the narrowed prefix is stored" "t6/ok/"
    (str "path_prefix" j);
  S.has_covering_grant p me.S.i_id "t6/ok/x" >>= fun inside ->
  Alcotest.(check bool) "in-garden path covered" true inside;
  S.has_covering_grant p me.S.i_id "t7/x" >>= fun beyond ->
  Alcotest.(check bool) "a path beyond the garden is not covered" false beyond;
  (* a member may not mint for anyone else; "" is never normalized *)
  Api.grant_mint_core p auth
    (body {|{"prim":"echo","identity":"00000000-0000-0000-0000-000000000000"}|})
  >>= fun (code, _) ->
  Alcotest.(check int) "a member may not mint for another identity" 403 code;
  Api.grant_mint_core p auth (body {|{"prim":"tree/put","path_prefix":""}|})
  >>= fun (code, _) ->
  Alcotest.(check int) "empty prefix refused" 400 code;
  Api.grant_mint_core p auth (body {|{"prim":"tree/putt","path_prefix":"t6/ok/"}|})
  >>= fun (code, _) ->
  Alcotest.(check int) "unknown prim refused" 400 code;
  return ()
(* -- op log: fork + chain ---------------------------------------------- *)

let test_ns_fork () =
  setup () >>= fun p ->
  let vh = Tuna.Hash.hex_of_string "22102000" in
  S.value_put p ~hash:vh ~ternary:"22102000" >>= fun () ->
  S.path_put p ~path:"t5/src/1" ~value_hash:vh ~owner:"src-owner"
  >>= fun _ ->
  S.path_put p ~path:"t5/src/2" ~value_hash:vh ~owner:"src-owner"
  >>= fun _ ->
  S.path_put p ~path:"t5/outside" ~value_hash:vh ~owner:"src-owner"
  >>= fun _ ->
  S.ns_fork p ~src_prefix:"t5/src" ~dst_prefix:"t5/dst" ~actor:"forker"
  >>= fun copied ->
  Alcotest.(check int) "copied 2" 2 copied;
  S.path_get p ~path:"t5/dst/1"
  >>= (function
        | None -> Alcotest.fail "forked path vanished"
        | Some e ->
            Alcotest.(check int64) "fork resets version to 1" 1L e.S.tp_version;
            Alcotest.(check string) "fork sets owner" "forker" e.S.tp_owner;
            Alcotest.(check string) "fork keeps value hash" vh e.S.tp_value_hash;
            return ())
  >>= fun () ->
  (* the source row is untouched *)
  S.path_get p ~path:"t5/src/1"
  >>= (function
        | None -> Alcotest.fail "source path vanished"
        | Some e ->
            Alcotest.(check int64) "source version unchanged" 1L e.S.tp_version;
            Alcotest.(check string) "source owner unchanged" "src-owner" e.S.tp_owner;
            return ())
  >>= fun () ->
  S.path_get p ~path:"t5/dst/outside" >>= fun outside ->
  Alcotest.(check bool) "outside the prefix not copied" true (Option.is_none outside);
  (* one fork marker row + one put row per copy *)
  S.ops_fold p ~prefix:"t5" ~from_seq:0L ()
  >>= fun ops ->
  let forks =
    List.filter (fun (o : S.tree_op) -> o.S.o_op = "fork") ops
  in
  let copies =
    List.filter
      (fun (o : S.tree_op) ->
        o.S.o_op = "put"
        && (o.S.o_path = "t5/dst/1" || o.S.o_path = "t5/dst/2"))
      ops
  in
  Alcotest.(check int) "one fork marker row" 1 (List.length forks);
  Alcotest.(check int) "one put row per copy" 2 (List.length copies);
  return ()

let test_ops_chain () =
  setup () >>= fun p ->
  (* writes through the prim path so the log actually has rows to
     verify: bare store puts carry no tree_ops rows *)
  let put path =
    Tp.dispatch ~pool:p ~actor:"o" ~name:"tree/put"
      ~args:(Tuna_server.Prims.tree_of_list
               [ Tuna.Cstr.encode path; Tuna.Canon.parse "0" ])
    >>= fun _ -> return ()
  in
  put "t6/ops/a" >>= fun () ->
  put "t6/ops/b" >>= fun () ->
  S.ops_fold p ~from_seq:0L ()
  >>= fun ops ->
  (match S.verify_ops_chain ops with
   | `Ok -> ()
   | `Bad msg -> Alcotest.failf "fresh chain must verify: %s" msg);
  (* tamper a payload field in place -> the chain walk must fail *)
  Db.q_unit ~params:[ S.p_str "t6/ops/a" ] p
    "UPDATE tree_ops SET path = 't6/tampered' WHERE path = $1"
  >>= fun () ->
  S.ops_fold p ~from_seq:0L ()
  >>= fun ops' ->
  (match S.verify_ops_chain ops' with
   | `Bad _ -> return ()
   | `Ok -> Alcotest.fail "tampered op log verified")

(* -- rewind ------------------------------------------------------------- *)

let test_rewind () =
  setup () >>= fun p ->
  let v1 = Tuna.Hash.hex_of_string "10" in
  let v2 = Tuna.Hash.hex_of_string "0" in
  (* write through the journaled prim path: bare store puts carry no op
     rows, and the fold can only reproduce what the log recorded *)
  let put path value_ternary =
    Tp.dispatch ~pool:p ~actor:"o" ~name:"tree/put"
      ~args:(Tuna_server.Prims.tree_of_list
               [ Tuna.Cstr.encode path; Tuna.Canon.parse value_ternary ])
    >>= fun _ -> return ()
  in
  put "t7/r/x" "10" >>= fun () ->
  put "t7/r/x" "0" >>= fun () ->
  put "t7/r/y" "10" >>= fun () ->
  put "t7/other/z" "10" >>= fun () ->
  (* the fold at head equals the live index *)
  S.path_list p ~prefix:"t7/r" ()
  >>= fun live ->
  S.ops_fold p ~prefix:"t7/r" ~from_seq:0L ()
  >>= fun ops ->
  let head = List.fold_left (fun a (o : S.tree_op) -> Int64.max a o.S.o_seq) 0L ops in
  Rw.state p ~prefix:"t7/r" ~at_seq:head
  >>= fun rew ->
  Alcotest.(check (list string)) "same paths"
    (List.map (fun e -> e.S.tp_path) live)
    (List.map (fun e -> e.Rw.path) rew);
  List.iter2
    (fun l r ->
      Alcotest.(check string) "same value hash" l.S.tp_value_hash r.Rw.value_hash;
      Alcotest.(check int64) "same version" l.S.tp_version r.Rw.version)
    live rew;
  (* point-in-time: before the last put, y did not exist and x was at v1 *)
  let seq_of path =
    match
      List.find_opt
        (fun (o : S.tree_op) -> o.S.o_op = "put" && o.S.o_path = path)
        ops
    with
    | Some o -> o.S.o_seq
    | None -> Alcotest.failf "no put row for %s" path
  in
  Rw.state p ~prefix:"t7/r" ~at_seq:(Int64.pred (seq_of "t7/r/x"))
  >>= fun early ->
  Alcotest.(check int) "x absent before its put" 0 (List.length early);
  Rw.state p ~prefix:"t7/r" ~at_seq:(seq_of "t7/r/x")
  >>= fun after_x ->
  (match after_x with
   | [ e ] ->
       Alcotest.(check string) "x at v1 hash" v1 e.Rw.value_hash;
       Alcotest.(check int64) "x at version 1" 1L e.Rw.version
   | n -> Alcotest.failf "expected exactly x, got %d entries" (List.length n));
  Rw.state p ~prefix:"t7/r" ~at_seq:(Int64.pred (seq_of "t7/r/y"))
  >>= fun mid ->
  (match mid with
   | [ e ] ->
       Alcotest.(check string) "x at v2 hash" v2 e.Rw.value_hash;
       Alcotest.(check int64) "x at version 2" 2L e.Rw.version
   | n -> Alcotest.failf "expected exactly x, got %d entries" (List.length n));
  return ()

let () =
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> print_endline "tree substrate tests skipped (TUNA_TEST_PG not set)"
  | Some _ ->
    let lwt _name f = Alcotest.test_case _name `Quick f in
    Tuna_test_eio.run "tree_substrate"
         [ ( "index"
           , [ lwt "put/get roundtrip + version bump" test_path_roundtrip
             ; lwt "cas create/conflict/absent" test_cas
             ; lwt "prefix list ordering" test_prefix_list ] )
         ; ("boundary", [ lwt "grant prefix allow + journaled denial" test_grant_prefix_boundary ])
         ; ("mint", [ lwt "scoped mint + prim vocabulary" test_grant_mint_scope ])
         ; ( "log"
           , [ lwt "ns_fork copies + fork op row" test_ns_fork
             ; lwt "chain verify + tamper detection" test_ops_chain ] )
         ; ("rewind", [ lwt "fold equals live index + point-in-time" test_rewind ]) ]
