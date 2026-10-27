(* Signup + the name-registry law (garden aliasing).

   Part 1 (DB-free): Store.check_name_claim - the pure law.  Runs in the
   plain runtest rule in dune, no Postgres needed.

   Part 2 (PG-gated via scripts/test-store.sh, TUNA_TEST_PG=1, own
   scratch db tuna_test_signup): mint_identity and signup_identity
   enforce the law inside the creating transaction; signup seeds the
   garden grant (prim tree/put, prefix u.<name>.), and the boundary
   (Api.tree_allowed, the same has_covering_grant floor the /api/tree/*
   routes gate on) opens exactly the garden. *)

open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Api = Tuna_server.Api
module Web = Tuna_server.Web

let name_err = function
  | `Invalid_name e | `Name_conflict e -> e
  | `Exists -> "name already exists"

let expect_ok what ~normalized = function
  | Ok n -> Alcotest.(check string) what normalized n
  | Error e -> Alcotest.failf "%s: expected Ok, got %s" what (name_err e)

let expect_conflict what = function
  | Error (`Name_conflict _) -> ()
  | Error e -> Alcotest.failf "%s: expected Name_conflict, got %s" what (name_err e)
  | Ok n -> Alcotest.failf "%s: expected conflict, got Ok %S" what n

let expect_invalid what = function
  | Error (`Invalid_name _) -> ()
  | Error e -> Alcotest.failf "%s: expected Invalid_name, got %s" what (name_err e)
  | Ok n -> Alcotest.failf "%s: expected invalid, got Ok %S" what n

(* -- pure law (DB-free) ---------------------------------------------- *)

let test_alias_both_directions () =
  expect_conflict "candidate g vs existing gregor"
    (S.check_name_claim ~candidate:"g" ~existing:[ "gregor" ]);
  expect_conflict "candidate gregor vs existing g"
    (S.check_name_claim ~candidate:"gregor" ~existing:[ "g" ]);
  expect_conflict "candidate greg vs existing gregor"
    (S.check_name_claim ~candidate:"greg" ~existing:[ "gregor" ]);
  expect_conflict "equal names alias"
    (S.check_name_claim ~candidate:"gregor" ~existing:[ "gregor" ])

let test_alias_prefix_family () =
  expect_conflict "gregora refused while gregor exists (root prefix)"
    (S.check_name_claim ~candidate:"gregora" ~existing:[ "gregor" ]);
  expect_conflict "gregor refused while gregora exists (root prefix)"
    (S.check_name_claim ~candidate:"gregor" ~existing:[ "gregora" ]);
  expect_ok "unrelated names coexist" ~normalized:"tuna"
    (S.check_name_claim ~candidate:"tuna" ~existing:[ "gregor"; "root" ])

let test_normalization () =
  expect_conflict "trim + case fold before compare"
    (S.check_name_claim ~candidate:"  Gregor " ~existing:[ "gregor" ]);
  expect_ok "uppercase folds to lowercase" ~normalized:"wren"
    (S.check_name_claim ~candidate:"WREN" ~existing:[])

let test_charset () =
  expect_ok "a-z0-9 and hyphen are legal" ~normalized:"a1-b2"
    (S.check_name_claim ~candidate:"a1-b2" ~existing:[]);
  expect_invalid "empty" (S.check_name_claim ~candidate:"" ~existing:[]);
  expect_invalid "space" (S.check_name_claim ~candidate:"bad name" ~existing:[]);
  expect_invalid "punct" (S.check_name_claim ~candidate:"gregor!" ~existing:[]);
  expect_invalid "dot" (S.check_name_claim ~candidate:"u.g" ~existing:[]);
  expect_invalid "leading hyphen"
    (S.check_name_claim ~candidate:"-greg" ~existing:[]);
  expect_invalid "trailing hyphen"
    (S.check_name_claim ~candidate:"greg-" ~existing:[]);
  expect_invalid "too long (33)"
    (S.check_name_claim ~candidate:(String.make 33 'a') ~existing:[]);
  expect_ok "exactly 32 chars ok" ~normalized:(String.make 32 'a')
    (S.check_name_claim ~candidate:(String.make 32 'a') ~existing:[])

(* -- the env gate (DB-free) ------------------------------------------ *)

let test_signup_env_gate () =
  Unix.putenv "TUNA_SIGNUP" "";
  Alcotest.(check bool) "empty value disables" false (Api.signup_enabled ());
  Unix.putenv "TUNA_SIGNUP" "0";
  Alcotest.(check bool) "0 disables" false (Api.signup_enabled ());
  Unix.putenv "TUNA_SIGNUP" "1";
  Alcotest.(check bool) "1 enables" true (Api.signup_enabled ());
  Unix.putenv "TUNA_SIGNUP" ""

(* -- the anti-spam gate (DB-free) -------------------------------------- *)

let expect_allowed what = function
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: expected Ok, got Error %S" what e

let expect_refused what = function
  | Error _ -> ()
  | Ok () -> Alcotest.failf "%s: expected refusal, got Ok" what

let test_signup_rate_window () =
  let t : (string, float * int) Hashtbl.t = Hashtbl.create 8 in
  let try_at now =
    Api.signup_rate_check ~table:t ~limit:3 ~window:3600. ~now "1.2.3.4"
  in
  expect_allowed "1st in window" (try_at 100.);
  expect_allowed "2nd in window" (try_at 101.);
  expect_allowed "3rd in window" (try_at 102.);
  expect_refused "4th in window refused" (try_at 103.);
  expect_refused "refusal consumes no slot: still capped late in window"
    (try_at 3400.);
  expect_allowed "fresh window re-arms" (try_at 3700.)

let test_signup_rate_per_address () =
  let t : (string, float * int) Hashtbl.t = Hashtbl.create 8 in
  let try_at addr =
    Api.signup_rate_check ~table:t ~limit:1 ~window:3600. ~now:5. addr
  in
  expect_allowed "first address" (try_at "1.1.1.1");
  expect_refused "same address capped" (try_at "1.1.1.1");
  expect_allowed "another address unaffected" (try_at "2.2.2.2")

let test_signup_client_addr () =
  let mk headers =
    { Web.meth = "POST"; target = "/api/signup"; headers; body = ""
    ; captures = [] }
  in
  let addr h = Api.signup_client_addr (mk h) in
  Alcotest.(check string) "no header is direct" "direct" (addr []);
  Alcotest.(check string) "single entry passes through" "9.9.9.9"
    (addr [ ("X-Forwarded-For", "9.9.9.9") ]);
  Alcotest.(check string) "relayd-appended last entry wins" "8.8.8.8"
    (addr [ ("X-Forwarded-For", "spoofed, 8.8.8.8") ]);
  Alcotest.(check string) "empty header is direct" "direct"
    (addr [ ("X-Forwarded-For", " ") ])

let test_signup_gate_rate () =
  Unix.putenv "TUNA_SIGNUP_RATE_PER_HOUR" "1";
  let mk ip =
    { Web.meth = "POST"
    ; target = "/api/signup"
    ; headers = [ ("X-Forwarded-For", ip) ]
    ; body = ""
    ; captures = [] }
  in
  expect_allowed "gate 1st" (Api.signup_gate (mk "gate-test-1"));
  expect_refused "gate 2nd refused" (Api.signup_gate (mk "gate-test-1"));
  Unix.putenv "TUNA_SIGNUP_RATE_PER_HOUR" ""

(* -- pg: the law + the garden (scripts/test-store.sh gate) ----------- *)

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p

let test_signup_seeds_garden () =
  setup () >>= fun p ->
  S.signup_identity p ~name:" Sg-Alice " ~token:"sg-alice-token" ()
  >>= (function
        | Error e -> Alcotest.failf "signup failed: %s" (name_err e)
        | Ok (i, g) ->
            Alcotest.(check string) "name normalized" "sg-alice" i.S.i_name;
            Alcotest.(check bool) "signup is non-admin" false i.S.i_is_admin;
            Alcotest.(check string) "garden prim" "tree/put" g.S.g_prim;
            Alcotest.(check (option string)) "garden prefix" (Some "u.sg-alice.")
              g.S.g_path_prefix;
            Alcotest.(check string) "the row belongs to the new identity"
              i.S.i_id g.S.g_caller;
            let auth =
              { Api.auth_id = i.S.i_id
              ; Api.auth_name = i.S.i_name
              ; Api.auth_is_admin = false }
            in
            Api.tree_allowed p auth "u.sg-alice.hello" >>= fun inside ->
            Alcotest.(check bool) "in-garden path allowed" true inside;
            Api.tree_allowed p auth "u.sg-other/x" >>= fun beyond ->
            Alcotest.(check bool) "outside path denied" false beyond;
            (* a real write inside the garden lands *)
            let vh = Tuna.Hash.hex_of_string "1010" in
            S.path_put p ~path:"u.sg-alice.hello" ~value_hash:vh ~owner:i.S.i_id
            >>= fun v ->
            Alcotest.(check int64) "in-garden put at version 1" 1L v;
            return ())

let test_mint_enforces_law_in_tx () =
  setup () >>= fun p ->
  S.mint_identity p ~name:"sg-gregor" ~token:"t1" ()
  >>= (function
        | Ok _ -> return ()
        | Error e -> Alcotest.failf "seed mint failed: %s" (name_err e))
  >>= fun () ->
  S.mint_identity p ~name:"sg-g" ~token:"t2" ()
  >>= (function
        | Error (`Name_conflict _) -> return ()
        | Error e ->
            Alcotest.failf "sg-g must alias sg-gregor, got %s" (name_err e)
        | Ok _ -> Alcotest.fail "sg-g must alias sg-gregor")
  >>= fun () ->
  S.mint_identity p ~name:"sg-greg" ~token:"t3" ()
  >>= (function
        | Error (`Name_conflict _) -> return ()
        | Error e ->
            Alcotest.failf "sg-greg must alias sg-gregor, got %s" (name_err e)
        | Ok _ -> Alcotest.fail "sg-greg must alias sg-gregor")
  >>= fun () ->
  S.mint_identity p ~name:"sg-gregor" ~token:"t4" ()
  >>= (function
        | Error (`Name_conflict _) | Error `Exists -> return ()
        | Error (`Invalid_name e) ->
            Alcotest.failf "exact duplicate: wrong error: %s" e
        | Ok _ -> Alcotest.fail "exact duplicate mint must fail")
  >>= fun () ->
  S.mint_identity p ~name:"sg-gregora" ~token:"t5" ()
  >>= (function
        | Error (`Name_conflict _) -> return ()
        | Error e ->
            Alcotest.failf "sg-gregora must alias sg-gregor (root), got %s"
              (name_err e)
        | Ok _ -> Alcotest.fail "sg-gregora must alias sg-gregor (root)")
  >>= fun () ->
  S.mint_identity p ~name:"Bad Name!" ~token:"t6" ()
  >>= (function
        | Error (`Invalid_name _) -> return ()
        | Error e -> Alcotest.failf "expected Invalid_name, got %s" (name_err e)
        | Ok _ -> Alcotest.fail "charset violation must be rejected")
  >>= fun () ->
  (* signup hits the same law, in the same transaction *)
  S.signup_identity p ~name:"sg-g" ~token:"t7" ()
  >>= (function
        | Error (`Name_conflict _) -> return ()
        | Error e -> Alcotest.failf "expected Name_conflict, got %s" (name_err e)
        | Ok _ -> Alcotest.fail "signup must respect the aliasing law")
  >>= fun () ->
  S.verify_token p "t1"
  >>= (function
        | Some i ->
            Alcotest.(check string) "seed identity intact" "sg-gregor" i.S.i_name
        | None -> Alcotest.fail "seed identity vanished")

let test_signup_core_codes () =
  setup () >>= fun p ->
  Api.signup_core p ~name:"bad name!" >>= fun (code, _) ->
  Alcotest.(check int) "invalid name answers 400" 400 code;
  Api.signup_core p ~name:"sg-core" >>= fun (code, j) ->
  Alcotest.(check int) "fresh signup answers 201" 201 code;
  let field k = function
    | `Assoc kvs -> List.assoc_opt k kvs
    | _ -> None
  in
  let token =
    match field "token" j with
    | Some (`String t) -> t
    | _ -> Alcotest.fail "signup response carries the token once"
  in
  Alcotest.(check bool) "token non-empty" true (String.length token > 0);
  S.verify_token p token
  >>= (function
        | Some i -> Alcotest.(check string) "token verifies" "sg-core" i.S.i_name
        | None -> Alcotest.fail "returned token does not verify");
  Api.signup_core p ~name:"sg-co" >>= fun (code, _) ->
  Alcotest.(check int) "aliasing signup answers 409" 409 code;
  return ()

let () =
  let lwt n f = Alcotest.test_case n `Quick f in
  let pure_tests =
      [ ( "law"
        , [ lwt "aliasing both directions" test_alias_both_directions
          ; lwt "prefix-family names refused" test_alias_prefix_family
          ; lwt "normalization" test_normalization
          ; lwt "charset" test_charset
          ; lwt "TUNA_SIGNUP gate" test_signup_env_gate ] )
      ; ( "anti-spam"
        , [ lwt "fixed window refills" test_signup_rate_window
          ; lwt "buckets are per address" test_signup_rate_per_address
          ; lwt "client addr trusts the last hop" test_signup_client_addr
          ; lwt "gate rate-limits" test_signup_gate_rate ] ) ]
  in
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> Alcotest.run "signup_garden" pure_tests
  | Some _ ->
      let pg_tests =
        [ ( "pg"
          , [ lwt "signup seeds the garden" test_signup_seeds_garden
            ; lwt "mint + signup enforce the law in-tx"
                test_mint_enforces_law_in_tx
            ; lwt "signup_core answer codes" test_signup_core_codes ] ) ]
      in
      Tuna_test_eio.run "signup_garden" (pure_tests @ pg_tests)