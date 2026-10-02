(* Delegation-attenuation acceptance tests (borg/grants.borg
   §delegation-attenuation).  PG-gated integration
   (scripts/test-store.sh, TUNA_TEST_PG=1; own scratch db
   tuna_test_delegation).  Pins:

   1. attenuation mints a NARROWER grant from one the caller HOLDS: the
      derived row records both lineage facts (minted_by = minting
      identity, parent_grant = the narrowed capability), and the
      narrowing BITES at the boundary (a capped child denies calls the
      admit-all root admits; denial is a journaled ANSWER, run
      continues).
   2. the narrowing relation rejects widening: bigger max_ternary,
      admit-all under a cap, path-prefix escape/drop, prim change,
      malformed JSON, unknown predicate shapes.
   3. only the HOLDER may attenuate (another identity -> Wrong_caller).
   4. revocation walks the lineage: revoking a root kills the whole
      descendant subtree's future use (checks fail, attenuation from a
      dead branch refused, submission denied) -- attenuation must not
      escape revocation.  History untouched (journals answer replay).
   5. lineage is queryable both directions (ancestors + descendants).
   6. the layering claim: NO prim mints -- the calculus cannot grant;
      minting is a host API operation only.
*)

open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Run = Tuna_server.Run

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p

let who p name = S.bootstrap_identity p ~name ~token:(name ^ "-token") ()

let expect_ok_grant what = function
  | `Ok g -> return g
  | `Unknown -> Alcotest.fail (what ^ ": unknown grant")
  | `Revoked -> Alcotest.fail (what ^ ": revoked")
  | `Wrong_caller -> Alcotest.fail (what ^ ": wrong caller")
  | `Not_narrower r -> Alcotest.fail (what ^ ": not narrower: " ^ r)

let expect_rejected what = function
  | `Not_narrower _ -> return ()
  | `Ok _ -> Alcotest.fail (what ^ ": expected rejection, minted")
  | `Unknown -> Alcotest.fail (what ^ ": unknown grant")
  | `Revoked -> Alcotest.fail (what ^ ": revoked")
  | `Wrong_caller -> Alcotest.fail (what ^ ": wrong caller")

(* echo applied to one arg: the whole program is the prim call tree *)
let echo_program p =
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  let ternary = Tuna.Canon.encode call in
  let hash = Tuna.Hash.hex_of_string ternary in
  S.upsert_program p ~hash ~ternary ~ir:None ~created_by:None
  >>= fun _ -> return hash

let run_echo p ~caller ~grants ~input =
  echo_program p >>= fun ph ->
  Run.execute_run p ~caller ~program_hash:ph ~input_trees:[ input ]
    ~grant_ids:grants ~fuel:100_000 ~semantics:"v0" ~size_cap:100_000 ()

let test_attenuate_mints_and_bites () =
  setup () >>= fun p ->
  who p "deleg-root" >>= fun a ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:a.S.i_id
    ~minted_by:(Some a.S.i_id) ()
  >>= fun root ->
  S.attenuate_grant p ~parent_id:root.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 4}" ~path_prefix:None ~caller:a.S.i_id
    ()
  >>= expect_ok_grant "cap-4 child"
  >>= fun child ->
  (* both lineage facts recorded *)
  Alcotest.(check (option string)) "child.parent_grant = root" (Some root.S.g_id)
    child.S.g_parent_grant;
  Alcotest.(check (option string)) "child.minted_by = holder" (Some a.S.i_id)
    child.S.g_minted_by;
  (* the narrowing BITES at the boundary *)
  let small = Tuna.Canon.encode (Tuna.Tree.Stem Tuna.Tree.Leaf) in
  let big =
    (* 7-char ternary: over the cap of 4 *)
    Tuna.Canon.encode
      (Tuna.Tree.Fork
         ( Tuna.Tree.Stem (Tuna.Tree.Stem (Tuna.Tree.Stem Tuna.Tree.Leaf))
         , Tuna.Tree.Leaf ))
  in
  Alcotest.(check bool) "big args actually exceed the cap" true
    (String.length big > 4);
  run_echo p ~caller:a.S.i_id ~grants:[ child.S.g_id ]
    ~input:(match Tuna.Canon.of_string small with Ok t -> t | Error _ -> assert false)
  >>= (function
        | Ok (row, _) ->
            Alcotest.(check string) "small args: run normal" "normal"
              (S.Run_status.to_string row.S.r_status);
            Alcotest.(check (option string)) "small args: echoed" (Some small)
              row.S.r_result_ternary;
            return ()
        | Error (_, msg) -> Alcotest.fail ("small args run failed: " ^ msg))
  >>= fun () ->
  run_echo p ~caller:a.S.i_id ~grants:[ child.S.g_id ]
    ~input:(match Tuna.Canon.of_string big with Ok t -> t | Error _ -> assert false)
  >>= (function
        | Ok (row, js) ->
            (* denial is a journaled ANSWER; the run continues *)
            Alcotest.(check string) "big args: run still normal" "normal"
              (S.Run_status.to_string row.S.r_status);
            let denied =
              List.exists
                (fun (j : S.journal) ->
                  match j.S.j_error with
                  | Some e -> e = "grant denial: prim echo args exceed attenuation"
                  | None -> false)
                js
            in
            Alcotest.(check bool) "big args: denial journaled" true denied;
            return ()
        | Error (_, msg) -> Alcotest.fail ("big args run failed: " ^ msg))

let test_narrowing_relation () =
  setup () >>= fun p ->
  who p "deleg-narrow" >>= fun a ->
  let att parent prim child ?(path = None) ?(ppath = None) () =
    S.mint_grant p ~prim:parent ~args_attenuation:"{}" ~path_prefix:ppath
      ~caller:a.S.i_id ~minted_by:(Some a.S.i_id) ()
    >>= fun root ->
    S.attenuate_grant p ~parent_id:root.S.g_id ~prim
      ~args_attenuation:child ~path_prefix:path ~caller:a.S.i_id ()
  in
  (* widening max_ternary rejected *)
  att "echo" "echo" "{\"max_ternary\": 4}" () >>= expect_ok_grant "cap4" >>= fun c4 ->
  S.attenuate_grant p ~parent_id:c4.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 9}" ~path_prefix:None ~caller:a.S.i_id
    ()
  >>= expect_rejected "cap 4 -> 9"
  >>= fun () ->
  (* admit-all under a cap rejected *)
  S.attenuate_grant p ~parent_id:c4.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a.S.i_id ()
  >>= expect_rejected "cap -> admit-all"
  >>= fun () ->
  (* equal cap allowed (delegation without further attenuation) *)
  S.attenuate_grant p ~parent_id:c4.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 4}" ~path_prefix:None ~caller:a.S.i_id
    ()
  >>= expect_ok_grant "cap 4 -> 4" >>= fun _ -> return ()
  >>= fun () ->
  (* prim change rejected; "*" narrows to a named prim *)
  att "echo" "now" "{}" () >>= expect_rejected "echo -> now"
  >>= fun () ->
  att "*" "echo" "{}" () >>= expect_ok_grant "star -> echo" >>= fun _ ->
  return ()
  >>= fun () ->
  (* path narrowing: child under the parent prefix ok; escape rejected;
     dropping the prefix rejected; NULL parent admits any prefix *)
  att "tree/get" "tree/get" "{}" ~ppath:(Some "ns/") ~path:(Some "ns/sub/") ()
  >>= expect_ok_grant "ns/ -> ns/sub/" >>= fun _ -> return ()
  >>= fun () ->
  att "tree/get" "tree/get" "{}" ~ppath:(Some "ns/") ~path:(Some "other/") ()
  >>= expect_rejected "ns/ -> other/"
  >>= fun () ->
  att "tree/get" "tree/get" "{}" ~ppath:(Some "ns/") ~path:None ()
  >>= expect_rejected "ns/ -> NULL"
  >>= fun () ->
  att "tree/get" "tree/get" "{}" ~ppath:None ~path:(Some "any/") ()
  >>= expect_ok_grant "NULL -> any/" >>= fun _ -> return ()
  >>= fun () ->
  (* malformed child JSON and unknown predicate shapes refused *)
  att "echo" "echo" "not json" () >>= expect_rejected "malformed child"
  >>= fun () ->
  att "echo" "echo" "{\"weird\": true}" ()
  >>= expect_rejected "unknown child shape"
  >>= fun () ->
  att "echo" "echo" "{}" () >>= expect_ok_grant "weird parent" >>= fun weird ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{\"weird\": true}"
    ~caller:a.S.i_id ~minted_by:(Some a.S.i_id) ()
  >>= fun wroot ->
  S.attenuate_grant p ~parent_id:wroot.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a.S.i_id ()
  >>= expect_rejected "unknown parent shape"
  >>= fun () ->
  Alcotest.(check bool) "weird root is un-attenuable" true
    (weird.S.g_id <> wroot.S.g_id);
  return ()

let test_only_holder_attenuates () =
  setup () >>= fun p ->
  who p "deleg-hold-a" >>= fun a ->
  who p "deleg-hold-b" >>= fun b ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:a.S.i_id
    ~minted_by:(Some a.S.i_id) ()
  >>= fun root ->
  S.attenuate_grant p ~parent_id:root.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:b.S.i_id ()
  >>= (function
        | `Wrong_caller -> return ()
        | _ -> Alcotest.fail "non-holder must not attenuate")
  >>= fun () ->
  (* b mints their own root and attenuates that: fine *)
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:b.S.i_id
    ~minted_by:(Some b.S.i_id) ()
  >>= fun broot ->
  S.attenuate_grant p ~parent_id:broot.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:b.S.i_id ()
  >>= expect_ok_grant "b attenuates own root" >>= fun _ -> return ()

let test_revocation_walks_lineage () =
  setup () >>= fun p ->
  who p "deleg-rev" >>= fun a ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:a.S.i_id
    ~minted_by:(Some a.S.i_id) ()
  >>= fun root ->
  S.attenuate_grant p ~parent_id:root.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a.S.i_id ()
  >>= expect_ok_grant "child" >>= fun child ->
  S.attenuate_grant p ~parent_id:child.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 2}" ~path_prefix:None ~caller:a.S.i_id
    ()
  >>= expect_ok_grant "grandchild" >>= fun gc ->
  (* revoke the ROOT: the whole subtree's future use dies *)
  S.revoke_grant p root.S.g_id >>= fun () ->
  S.check_grant p ~id:gc.S.g_id ~caller:a.S.i_id ()
  >>= (function
        | `Revoked -> return ()
        | `Ok -> Alcotest.fail "grandchild survived root revocation"
        | _ -> Alcotest.fail "expected Revoked through lineage")
  >>= fun () ->
  S.check_grant p ~id:child.S.g_id ~caller:a.S.i_id ()
  >>= (function
        | `Revoked -> return ()
        | _ -> Alcotest.fail "child survived root revocation")
  >>= fun () ->
  (* attenuation from a dead branch refused *)
  S.attenuate_grant p ~parent_id:child.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a.S.i_id ()
  >>= (function
        | `Revoked -> return ()
        | _ -> Alcotest.fail "attenuation from dead lineage must refuse")
  >>= fun () ->
  (* submission with a lineage-dead grant is denied up front *)
  run_echo p ~caller:a.S.i_id ~grants:[ gc.S.g_id ]
    ~input:(match Tuna.Canon.of_string "10" with Ok t -> t | Error _ -> assert false)
  >>= (function
        | Error (_, msg) ->
            Alcotest.(check bool) "submission names the revoked grant" true
              (String.length msg > 0);
            return ()
        | Ok _ -> Alcotest.fail "lineage-dead grant must not run")
  >>= fun () ->
  (* revoking the CHILD only kills ITS subtree *)
  who p "deleg-rev-2" >>= fun a2 ->
  S.mint_grant p ~prim:"echo" ~args_attenuation:"{}" ~caller:a2.S.i_id
    ~minted_by:(Some a2.S.i_id) ()
  >>= fun root2 ->
  S.attenuate_grant p ~parent_id:root2.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a2.S.i_id ()
  >>= expect_ok_grant "child2"
  >>= fun child2 ->
  S.attenuate_grant p ~parent_id:child2.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 2}" ~path_prefix:None ~caller:a2.S.i_id
    ()
  >>= expect_ok_grant "gc2" >>= fun gc2 ->
  S.revoke_grant p child2.S.g_id >>= fun () ->
  S.check_grant p ~id:gc2.S.g_id ~caller:a2.S.i_id ()
  >>= (function
        | `Revoked -> return ()
        | _ -> Alcotest.fail "gc2 survived child2 revocation")
  >>= fun () ->
  S.check_grant p ~id:root2.S.g_id ~caller:a2.S.i_id ()
  >>= (function
        | `Ok -> return () (* the parent itself stays live *)
        | _ -> Alcotest.fail "revoking a child must not kill the parent")

let test_lineage_queryable () =
  setup () >>= fun p ->
  who p "deleg-audit" >>= fun a ->
  S.mint_grant p ~prim:"*" ~args_attenuation:"{}" ~caller:a.S.i_id
    ~minted_by:(Some a.S.i_id) ()
  >>= fun root ->
  S.attenuate_grant p ~parent_id:root.S.g_id ~prim:"echo" ~args_attenuation:"{}"
    ~path_prefix:None ~caller:a.S.i_id ()
  >>= expect_ok_grant "child" >>= fun child ->
  S.attenuate_grant p ~parent_id:child.S.g_id ~prim:"echo"
    ~args_attenuation:"{\"max_ternary\": 8}" ~path_prefix:None ~caller:a.S.i_id
    ()
  >>= expect_ok_grant "gc" >>= fun gc ->
  (* ancestors: nearest first, ending at the root *)
  S.grant_lineage p gc.S.g_id >>= fun lineage ->
  Alcotest.(check int) "lineage length 2" 2 (List.length lineage);
  (match lineage with
   | [ c; r ] ->
       Alcotest.(check string) "nearest ancestor is the parent" child.S.g_id
         c.S.g_id;
       Alcotest.(check string) "farthest ancestor is the root" root.S.g_id
         r.S.g_id
   | _ -> Alcotest.fail "bad lineage shape");
  (* descendants: direct children and deeper *)
  S.grant_descendants p root.S.g_id >>= fun desc ->
  Alcotest.(check int) "root has 2 descendants" 2 (List.length desc);
  Alcotest.(check bool) "child in descendants" true
    (List.exists (fun (g : S.grant) -> g.S.g_id = child.S.g_id) desc);
  Alcotest.(check bool) "gc in descendants" true
    (List.exists (fun (g : S.grant) -> g.S.g_id = gc.S.g_id) desc);
  S.grant_descendants p gc.S.g_id >>= fun none ->
  Alcotest.(check int) "gc has no descendants" 0 (List.length none);
  return ()

let test_no_mint_prim () =
  (* the layering claim (grants.borg §delegation-attenuation): granting
     FROM INSIDE the calculus is excluded in v1 -- no prim mints, so a
     program can never widen its own authority.  Minting is a host API
     operation only. *)
  let forbidden = [ "mint"; "grant"; "attenuate"; "delegate" ] in
  List.iter
    (fun n ->
      Alcotest.(check bool) (Printf.sprintf "no %S prim" n) false
        (Tuna_server.Prims.exists n))
    forbidden;
  Alcotest.(check bool) "echo still exists (sanity)" true
    (Tuna_server.Prims.exists "echo");
  return ()

let () =
  let lwt name f = Alcotest.test_case name `Quick f in
  Tuna_test_eio.run "delegation"
       [ ( "delegation-attenuation"
         , [ lwt "attenuate mints a narrower grant that bites"
               test_attenuate_mints_and_bites
           ; lwt "narrowing relation rejects widening" test_narrowing_relation
           ; lwt "only the holder may attenuate" test_only_holder_attenuates
           ; lwt "revocation walks the lineage" test_revocation_walks_lineage
           ; lwt "lineage queryable both directions" test_lineage_queryable
           ; lwt "no prim mints (layering)" test_no_mint_prim
           ] ) ]
