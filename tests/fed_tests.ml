(* M12 federation F1 acceptance tests (borg/federation.borg, value
   exchange).  Integration tests require Postgres (scripts/test-store.sh
   gate, TUNA_TEST_PG=1); skipped silently otherwise.  Runs on its OWN
   scratch db (tuna_test_fed) so the PG suites never share one.

   Exercise the Dream-free core (Api.fed_value_core) directly, like
   m11_tests does for value_get_core: the acceptance chapter is about
   store behavior and the rehash contract, not HTTP plumbing.

   The rehash criterion is independent: sha256 recomputed here with
   digestif must equal the hash the peer asked for, whatever the kind. *)

open Lwt.Infix

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Api = Tuna_server.Api
module J = Yojson.Basic

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> Lwt.return p

let admin p = S.bootstrap_identity p ~name:"fed-root" ~token:"fed-root-token" ()

let peer p name =
  S.bootstrap_identity p ~is_admin:false ~name ~token:(name ^ "-token") ()

let auth_of i =
  { Api.auth_id = i.S.i_id
  ; Api.auth_name = i.S.i_name
  ; Api.auth_is_admin = i.S.i_is_admin }

let independent_sha256 bytes =
  String.lowercase_ascii
    (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string bytes))

let json_string j k = match J.Util.member k j with `String s -> Some s | _ -> None

let ops_with p ~op ~path =
  S.ops_fold p ~prefix:"" ~from_seq:0L ()
  >>= fun ops ->
  Lwt.return
    (List.filter
       (fun (o : S.tree_op) -> o.S.o_op = op && o.S.o_path = path)
       ops)

(* -- the core contract: one hash, three homes, JSON out ---------------- *)

let test_fed_tree_roundtrip () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let ternary = "2202102010" in
  let hash = independent_sha256 ternary in
  S.value_put p ~hash ~ternary >>= fun () ->
  Api.fed_value_core p ~auth:(auth_of me) ~hash
  >>= fun (code, body) ->
  let j = J.from_string body in
  Alcotest.(check int) "tree: fed get 200" 200 code;
  Alcotest.(check (option string)) "tree: kind" (Some "tree")
    (json_string j "kind");
  Alcotest.(check (option string)) "tree: payload is the ternary"
    (Some ternary) (json_string j "payload");
  Alcotest.(check (option string)) "tree: hash echoed" (Some hash)
    (json_string j "hash");
  (* the rehash contract: recompute the hash from the payload alone *)
  let payload = Option.value (json_string j "payload") ~default:"?" in
  Alcotest.(check string) "tree: rehash matches" hash
    (independent_sha256 payload);
  Lwt.return ()

let test_fed_bytes_roundtrip () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let a = auth_of me in
  let bytes = "\000\001\255byte-value" in
  Api.value_put_core p ~auth:a ~bytes >>= fun (put_code, put_j) ->
  Alcotest.(check int) "bytes: put 201" 201 put_code;
  let hash = Option.value (json_string put_j "hash") ~default:"?" in
  Api.fed_value_core p ~auth:a ~hash >>= fun (code, body) ->
  let j = J.from_string body in
  Alcotest.(check int) "bytes: fed get 200" 200 code;
  Alcotest.(check (option string)) "bytes: kind" (Some "bytes")
    (json_string j "kind");
  let payload = Option.value (json_string j "payload") ~default:"?" in
  Alcotest.(check string) "bytes: base64 decodes byte-identical" bytes
    (Base64.decode_exn payload);
  Alcotest.(check string) "bytes: rehash matches" hash
    (independent_sha256 (Base64.decode_exn payload));
  Lwt.return ()

(* a program hash resolves as kind tree with the program's own ternary:
   one address law, so a peer can pull a program and rehash it like any
   tree value *)
let test_fed_program_hash () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let ternary = "221010210" in
  let hash = Tuna.Hash.hex_of_string ternary in
  S.upsert_program p ~hash ~ternary ~ir:None ~created_by:None >>= fun _ ->
  Api.fed_value_core p ~auth:(auth_of me) ~hash >>= fun (code, body) ->
  let j = J.from_string body in
  Alcotest.(check int) "program: fed get 200" 200 code;
  Alcotest.(check (option string)) "program: kind tree" (Some "tree")
    (json_string j "kind");
  Alcotest.(check (option string)) "program: payload is the ternary"
    (Some ternary) (json_string j "payload");
  Alcotest.(check string) "program: rehash matches" hash
    (independent_sha256 ternary);
  Lwt.return ()

(* absent hashes answer 404 with an error field, and even denials land
   in the ops chain (op fed-value), attributed to the calling identity *)
let test_fed_absent_and_journal () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let ghost = independent_sha256 "no-value-was-ever-stored" in
  Api.fed_value_core p ~auth:(auth_of me) ~hash:ghost >>= fun (code, body) ->
  Alcotest.(check int) "absent: 404" 404 code;
  let j = J.from_string body in
  Alcotest.(check bool) "absent: error field present"
    (match json_string j "error" with Some _ -> true | None -> false)
    true;
  ops_with p ~op:"fed-value" ~path:ghost >>= fun ops ->
  Alcotest.(check int) "absent: denial journaled" 1 (List.length ops);
  (match ops with
    | [ o ] ->
        Alcotest.(check string) "absent: attributed to the caller" me.S.i_id
          o.S.o_actor
    | _ -> Alcotest.fail "impossible: ops_with length checked above");
  Lwt.return ()

(* per-peer identities: a non-admin peer's fetch is attributed to the
   peer, never to root; the peer needs no grant (hash-gated reads) *)
let test_fed_peer_attribution () =
  setup () >>= fun p ->
  admin p >>= fun _ ->
  peer p "fed-peer-testpeer" >>= fun pe ->
  let ternary = "2201022100" in
  let hash = independent_sha256 ternary in
  S.value_put p ~hash ~ternary >>= fun () ->
  Api.fed_value_core p ~auth:(auth_of pe) ~hash >>= fun (code, _) ->
  Alcotest.(check int) "peer: fed get 200 (no grant needed)" 200 code;
  ops_with p ~op:"fed-value" ~path:hash >>= fun ops ->
  (match ops with
    | [ o ] ->
        Alcotest.(check string) "peer: attributed to the peer identity"
          pe.S.i_id o.S.o_actor
    | _ -> Alcotest.fail "peer: expected exactly one fed-value op");
  Lwt.return ()

(* boot-time peer name validation: pure unit checks over the env parser
   helpers (lowercase alnum + dashes; dash maps to underscore in the
   token env name) *)
let test_fed_peer_names () =
  Alcotest.(check bool) "valid name" true (Api.fed_peer_name_ok "mainframe");
  Alcotest.(check bool) "valid dashed" true (Api.fed_peer_name_ok "town-2");
  Alcotest.(check bool) "empty rejected" false (Api.fed_peer_name_ok "");
  Alcotest.(check bool) "uppercase rejected" false
    (Api.fed_peer_name_ok "Mainframe");
  Alcotest.(check bool) "underscore rejected" false
    (Api.fed_peer_name_ok "bad_name");
  Alcotest.(check bool) "over 64 chars rejected" false
    (Api.fed_peer_name_ok (String.make 65 'a'));
  Alcotest.(check string) "token env maps dash to underscore"
    "TUNA_FED_PEER_TOKEN_TOWN_2" (Api.fed_peer_token_env "town-2");
  Lwt.return ()

let () =
  let lwt name f = Alcotest_lwt.test_case name `Quick (fun _sw () -> f ()) in
  Lwt_main.run
    (Alcotest_lwt.run "fed"
       [ ( "value-exchange"
         , [ lwt "tree roundtrip rehash" test_fed_tree_roundtrip
           ; lwt "bytes roundtrip rehash" test_fed_bytes_roundtrip
           ; lwt "program hash as tree" test_fed_program_hash
           ; lwt "absent 404 + journaled denial" test_fed_absent_and_journal
           ; lwt "peer attribution" test_fed_peer_attribution
           ; lwt "peer name validation" test_fed_peer_names ] ) ])
