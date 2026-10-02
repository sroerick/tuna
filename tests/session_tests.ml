(* pp-slice accounts tests (0013): the browser auth tier — password
   credentials, opaque sessions, expiry/revoke.  PG-gated via
   scripts/test-store.sh like the other store suites (own scratch db
   tuna_test_sessions). *)
open Lwt.Infix

module Db = Tuna_store.Db
module S = Tuna_store.Store
module C = Tuna_store.Credentials

(* store-lib unit coverage that needs no PG: format + round-trip + the
   constant-time compare (kept here so the DB suite owns the accounts
   story end to end) *)
let test_hash_format () =
  let h = C.hash_password "correct horse battery staple" in
  Alcotest.(check string) "format prefix" "sha256$" (String.sub h 0 7);
  Alcotest.(check bool) "verify ok" true
    (C.verify_password "correct horse battery staple" ~stored:h);
  Alcotest.(check bool) "verify wrong" false
    (C.verify_password "Tr0ub4dor&3" ~stored:h);
  (* each call salts fresh: two hashes of one password differ, both verify *)
  let h2 = C.hash_password "correct horse battery staple" in
  Alcotest.(check bool) "fresh salt" true (not (String.equal h h2));
  Alcotest.(check bool) "new salt verifies" true
    (C.verify_password "correct horse battery staple" ~stored:h2);
  (* malformed stored strings fail closed *)
  Alcotest.(check bool) "garbage fails" false
    (C.verify_password "x" ~stored:"$argon2id$future");
  Alcotest.(check bool) "empty fails" false
    (C.verify_password "x" ~stored:"sha256$$");
  (* the dummy hash is spendable and never compares true *)
  Alcotest.(check bool) "dummy never true" false
    (C.verify_password "throttle-dummy" ~stored:C.dummy_hash);
  (* constant-time compare basics: equal true, prefix/extension false *)
  Alcotest.(check bool) "secure_equal eq" true (C.secure_equal "abcdef" "abcdef");
  Alcotest.(check bool) "secure_equal ne" false (C.secure_equal "abcdef" "abcdeg");
  Alcotest.(check bool) "secure_equal len" false (C.secure_equal "abcdef" "abcdef0");
  Lwt.return ()

let with_pg f =
  Db.init (Db.config_from_env ())
  >>= fun p ->
  Db.apply_migrations p ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> f p

let test_passwords () =
  with_pg
    (fun p ->
      S.bootstrap_identity p ~name:"accounts-test-alice" ~token:"alice-token" ()
      >>= fun alice ->
      Alcotest.(check bool) "not admin by default" false
        (if alice.S.i_is_admin then false else true);
      S.verify_password p ~username:"accounts-test-alice" ~password:"no-password-yet"
      >>= fun none ->
      Alcotest.(check bool) "no credential -> deny" true (Option.is_none none);
      S.set_password p ~identity_id:alice.S.i_id ~password:"hunter2"
      >>= fun () ->
      S.verify_password p ~username:"accounts-test-alice" ~password:"hunter2"
      >>= fun ok ->
      let i = Option.get ok in
      Alcotest.(check string) "password -> identity" alice.S.i_id i.S.i_id;
      S.verify_password p ~username:"accounts-test-alice" ~password:"hunter3"
      >>= fun bad ->
      Alcotest.(check bool) "wrong password -> deny" true (Option.is_none bad);
      S.verify_password p ~username:"accounts-test-unknown" ~password:"hunter2"
      >>= fun unknown ->
      Alcotest.(check bool) "unknown user -> deny" true (Option.is_none unknown);
      Lwt.return ())

let test_sessions () =
  with_pg
    (fun p ->
      S.bootstrap_identity p ~name:"accounts-test-bob" ~token:"bob-token" ()
      >>= fun bob ->
      S.mint_session p ~identity_id:bob.S.i_id ~ttl_seconds:3600
      >>= fun tok ->
      Alcotest.(check int) "token length 64" 64 (String.length tok);
      S.verify_session p tok
      >>= fun who ->
      (match who with
       | None -> Alcotest.fail "minted session must verify"
       | Some i -> Alcotest.(check string) "session -> identity" bob.S.i_id i.S.i_id);
      S.verify_session p (tok ^ "0") >>= fun bad ->
      Alcotest.(check bool) "wrong token -> anonymous" true (Option.is_none bad);
      S.revoke_session p tok
      >>= fun () ->
      S.verify_session p tok >>= fun revoked ->
      Alcotest.(check bool) "revoked -> anonymous" true (Option.is_none revoked);
      (* revoke is idempotent *)
      S.revoke_session p tok
      >>= fun () ->
      (* ttl 0: expires now, i.e. already past *)
      S.mint_session p ~identity_id:bob.S.i_id ~ttl_seconds:0
      >>= fun tok0 ->
      S.verify_session p tok0 >>= fun gone ->
      Alcotest.(check bool) "expired -> anonymous" true (Option.is_none gone);
      (* sessions for one identity are independent rows *)
      S.mint_session p ~identity_id:bob.S.i_id ~ttl_seconds:3600
      >>= fun tok2 ->
      S.revoke_session p tok2
      >>= fun () ->
      S.verify_session p tok >>= fun r1 ->
      Alcotest.(check bool) " revoke one leaves nothing (earlier revoke held)" true
        (Option.is_none r1);
      Lwt.return ())

let test_auth_log () =
  with_pg
    (fun p ->
      S.bootstrap_identity p ~name:"accounts-test-carol" ~token:"carol-token" ()
      >>= fun carol ->
      S.set_password p ~identity_id:carol.S.i_id ~password:"pw-carol"
      >>= fun () ->
      S.verify_password p ~username:"accounts-test-carol" ~password:"pw-carol"
      >>= fun _ ->
      S.verify_password p ~username:"accounts-test-missing" ~password:"x"
      >>= fun _ ->
      S.mint_session p ~identity_id:carol.S.i_id ~ttl_seconds:60
      >>= fun _ ->
      let cell (r : Pgx.row) i = Pgx.Value.to_string_exn (List.nth r i) in
      let bool_cell (r : Pgx.row) i =
        if Pgx.Value.to_bool_exn (List.nth r i) then "true" else "false"
      in
      Db.q p "SELECT kind, success FROM auth_log ORDER BY happened_at, id"
      >>= fun rows ->
      let kind_success r = (cell r 0, bool_cell r 1) in
      let all = List.map kind_success rows in
      (match rows with
       | [] -> Alcotest.fail "auth_log stayed empty after logins"
       | _ -> ());
      Alcotest.(check bool) "password success logged"
        true (List.mem ("password", "true") all);
      Alcotest.(check bool) "password failure logged"
        true (List.exists (fun r -> r = ("password", "false")) all);
      Alcotest.(check bool) "session mint logged"
        true (List.exists (fun r -> fst r = "session") all);
      Lwt.return ())

let () =
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> print_endline "session tests skipped (TUNA_TEST_PG not set)"
  | Some _ ->
    let lwt _name f = Alcotest_lwt.test_case _name `Quick (fun _sw () -> f ()) in
    Lwt_main.run
      (Alcotest_lwt.run "sessions"
         [ ( "hash-format"
           , [ lwt "interim format round-trip, fail-closed, dummy" test_hash_format ] )
         ; ("passwords", [ lwt "set+verify / wrong / unknown" test_passwords ])
         ; ( "sessions"
           , [ lwt "mint+verify+revoke+expiry+lifetimes" test_sessions ] )
         ; ( "auth-log"
           , [ lwt "password+session attempts land in auth_log" test_auth_log ] )
         ])
