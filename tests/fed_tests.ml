(* M12 federation F1 acceptance tests (borg/federation.borg, value
   exchange).  Integration tests require Postgres (scripts/test-store.sh
   gate, TUNA_TEST_PG=1); skipped silently otherwise.  Runs on its OWN
   scratch db (tuna_test_fed) so the PG suites never share one.

   Exercise the Dream-free core (Api.fed_value_core) directly, like
   m11_tests does for value_get_core: the acceptance chapter is about
   store behavior and the rehash contract, not HTTP plumbing.

   The rehash criterion is independent: sha256 recomputed here with
   digestif must equal the hash the peer asked for, whatever the kind. *)

open Tuna_store.Direct

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Api = Tuna_server.Api
module J = Yojson.Basic

let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> return p

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
  return
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
  return ()

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
  return ()

(* a program hash resolves as kind tree with the program's own ternary:
   one address law, so a peer can pull a program and rehash it like any
   tree value *)
let test_fed_program_hash () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let ternary = "221010210" in
  let hash = Tuna.Hash.hex_of_string ternary in
  S.upsert_program p ~hash ~ternary ~ir:None ~created_by:None ~source:None >>= fun _ ->
  Api.fed_value_core p ~auth:(auth_of me) ~hash >>= fun (code, body) ->
  let j = J.from_string body in
  Alcotest.(check int) "program: fed get 200" 200 code;
  Alcotest.(check (option string)) "program: kind tree" (Some "tree")
    (json_string j "kind");
  Alcotest.(check (option string)) "program: payload is the ternary"
    (Some ternary) (json_string j "payload");
  Alcotest.(check string) "program: rehash matches" hash
    (independent_sha256 ternary);
  return ()

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
  return ()

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
  return ()

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
  return ()

(* -- F2: ops-chain sync (borg/federation.borg) --------------------------

   Pull a verifiable op window, apply it into ns/<peer>/..., re-derive
   the index by fold.  Rejects are values (journaled), never absorbed:
   a broken chain, an out-of-prefix path, an un-held value, a value
   that does not rehash, and a peer writing outside its ns/<peer>/ each
   answer an error code. *)

(* seed a source-namespace effect exactly as /api/tree/put does:
   content-addressed value + path row + one linked op row. *)
let seed_path p ~actor ~path ~ternary =
  let hash = independent_sha256 ternary in
  S.value_put p ~hash ~ternary >>= fun () ->
  S.path_put p ~path ~value_hash:hash ~owner:actor >>= fun v ->
  S.op_append p ~op:"put" ~path ~value_hash:(Some hash) ~prev_version:None
    ~version:(Some v) ~actor
  >>= fun _ -> return hash

(* pull the window for [prefix] as [auth], returning the ops JSON list.
   Each op row carries its prev_hash, so apply re-verifies per row. *)
let pull_ops p ~auth ~prefix ~after_seq =
  Api.fed_ops_core p ~auth ~prefix ~after_seq ~limit:1000
  >>= fun (code, j) ->
  Alcotest.(check int) "pull 200" 200 code;
  (match J.Util.member "verified" j with
   | `Bool true -> ()
   | _ -> Alcotest.fail "pull window did not verify");
  match J.Util.member "ops" j with
  | `List ops -> return ops
  | _ -> Alcotest.fail "pull response has no ops array"

let apply_body ~src ~dst ops =
  `Assoc [ ("src_prefix", `String src); ("dst_prefix", `String dst)
         ; ("ops", `List ops) ]

let test_f2_pull_window () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let a = auth_of me in
  let src = "f2src/pull/" in
  seed_path p ~actor:me.S.i_id ~path:(src ^ "one") ~ternary:"2202102010"
  >>= fun h1 ->
  seed_path p ~actor:me.S.i_id ~path:(src ^ "two") ~ternary:"221010210"
  >>= fun _ ->
  pull_ops p ~auth:a ~prefix:src ~after_seq:0 >>= fun ops ->
  Alcotest.(check bool) "window non-empty" true (List.length ops >= 2);
  (* every returned op is under the prefix *)
  List.iter
    (fun oj ->
      match J.Util.member "path" oj with
      | `String path ->
          Alcotest.(check bool)
            ("op under prefix: " ^ path) true
            (S.prefix_match src path)
      | _ -> Alcotest.fail "op has no path")
    ops;
  (* the value the effect cites is present and the pull is journaled *)
  S.value_present p h1 >>= fun present ->
  Alcotest.(check bool) "cited value held" true present;
  ops_with p ~op:"fed-ops" ~path:src >>= fun fs ->
  Alcotest.(check bool) "pull journaled as fed-ops" true (fs <> []);
  return ()

let test_f2_apply_roundtrip () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "fed-peer-town" >>= fun pe ->
  let a = auth_of me in
  let src = "f2src/rt/" in
  let dst = "ns/town/f2rt/" in
  seed_path p ~actor:me.S.i_id ~path:(src ^ "alpha") ~ternary:"2202102010"
  >>= fun h1 ->
  seed_path p ~actor:me.S.i_id ~path:(src ^ "beta") ~ternary:"221010210"
  >>= fun h2 ->
  pull_ops p ~auth:a ~prefix:src ~after_seq:0 >>= fun ops ->
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst ops)
  >>= fun (code, j) ->
  Alcotest.(check int) ("apply 200 (" ^ J.to_string j ^ ")") 200 code;
  (match J.Util.member "applied_count" j with
   | `Int n -> Alcotest.(check int) "two effects applied" 2 n
   | _ -> Alcotest.fail "apply response has no applied_count");
  (* the destination index carries both values at fresh version 1 *)
  S.path_get p ~path:(dst ^ "alpha") >>= fun ea ->
  S.path_get p ~path:(dst ^ "beta") >>= fun eb ->
  (match (ea, eb) with
   | Some ea, Some eb ->
       Alcotest.(check string) "alpha value hash moved" h1 ea.S.tp_value_hash;
       Alcotest.(check string) "beta value hash moved" h2 eb.S.tp_value_hash;
       Alcotest.(check int64) "alpha fresh version" 1L ea.S.tp_version;
       Alcotest.(check int64) "beta fresh version" 1L eb.S.tp_version
   | _ -> Alcotest.fail "destination paths missing after apply");
  (* every applied source op appended exactly one destination log row *)
  S.ops_head_seq p >>= fun head ->
  Alcotest.(check bool) "destination log grew" true (head >= 5L);
  (* the apply itself is journaled, attributed to the peer *)
  ops_with p ~op:"fed-apply" ~path:dst >>= fun fa ->
  (match fa with
   | [ o ] ->
       Alcotest.(check string) "apply attributed to the peer" pe.S.i_id
         o.S.o_actor
   | _ -> Alcotest.fail "expected exactly one fed-apply op");
  return ()

let test_f2_apply_shadow () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "fed-peer-shadow" >>= fun pe ->
  let a = auth_of me in
  let src = "f2src/shadow/" in
  let dst = "ns/shadow/f2sh/" in
  seed_path p ~actor:me.S.i_id ~path:(src ^ "x") ~ternary:"2202102010"
  >>= fun _ ->
  pull_ops p ~auth:a ~prefix:src ~after_seq:0 >>= fun ops ->
  (* first apply: version 1 *)
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst ops)
  >>= fun (c1, _) ->
  Alcotest.(check int) "first apply 200" 200 c1;
  (* second apply of the same window: same path, fresh version 2,
     reported under "shadowed" - append-only, never a silent merge *)
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst ops)
  >>= fun (c2, j) ->
  Alcotest.(check int) "second apply 200" 200 c2;
  (match J.Util.member "shadowed" j with
   | `List (_ :: _) -> ()
   | _ -> Alcotest.fail "re-apply did not report a shadow");
  S.path_get p ~path:(dst ^ "x") >>= fun e ->
  (match e with
   | Some e -> Alcotest.(check int64) "shadowed version 2" 2L e.S.tp_version
   | None -> Alcotest.fail "shadow destination path missing");
  return ()

let test_f2_apply_rejects () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "fed-peer-reject" >>= fun pe ->
  let a = auth_of me in
  let src = "f2src/rej/" in
  let dst = "ns/reject/f2rej/" in
  seed_path p ~actor:me.S.i_id ~path:(src ^ "keep") ~ternary:"2202102010"
  >>= fun _ ->
  pull_ops p ~auth:a ~prefix:src ~after_seq:0 >>= fun ops ->
  (* 1. broken chain: tamper an op_hash in the window *)
  let tampered =
    List.map
      (fun oj ->
        match oj with
        | `Assoc kvs ->
            `Assoc
              (List.map
                 (fun (k, v) ->
                   if k = "op_hash" then (k, `String (String.make 64 '0'))
                   else (k, v))
                 kvs)
        | other -> other)
      ops
  in
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst tampered)
  >>= fun (code, _) ->
  Alcotest.(check int) "broken chain refused (400)" 400 code;
  (* 2. out-of-prefix path: claim a path the src prefix does not cover *)
  let stray =
    List.map
      (fun oj ->
        match oj with
        | `Assoc kvs ->
            `Assoc
              (List.map
                 (fun (k, v) ->
                   if k = "path" then (k, `String "elsewhere/not/src")
                   else (k, v))
                 kvs)
        | other -> other)
      ops
  in
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst stray)
  >>= fun (code, _) ->
  Alcotest.(check int) "out-of-prefix refused (400)" 400 code;
  (* 3. un-held value: a CORRECTLY-LINKED op citing a hash in no value
     store.  Give it an arbitrary prev_hash and compute its op_hash from
     that (so the chain check passes); the value check must then be what
     refuses. *)
  let ghost = independent_sha256 "never-stored-value" in
  let ghost_json =
    let prev = String.make 64 'b' in
    let o =
      { S.o_seq = 999999L
      ; o_path = src ^ "ghost"
      ; o_op = "put"
      ; o_value_hash = Some ghost
      ; o_prev_version = None
      ; o_version = Some 1L
      ; o_actor = me.S.i_id
      ; o_ts_unix = 0L
      ; o_op_hash = "" }
    in
    let h =
      Tuna.Hash.hex_of_string (prev ^ S.tree_op_concat o)
    in
    [ `Assoc
        [ ("seq", `Int 999999)
        ; ("path", `String o.S.o_path)
        ; ("op", `String "put")
        ; ("value_hash", `String ghost)
        ; ("prev_version", `Null)
        ; ("version", `Int 1)
        ; ("actor", `String me.S.i_id)
        ; ("ts", `Int 0)
        ; ("prev_hash", `String prev)
        ; ("op_hash", `String h) ] ]
  in
  Api.fed_apply_core p ~auth:(auth_of pe) (apply_body ~src ~dst ghost_json)
  >>= fun (code, j) ->
  Alcotest.(check int) "un-held value refused (400)" 400 code;
  (match J.Util.member "error" j with
   | `String m ->
       Alcotest.(check bool) "error names the un-held value" true
         (let needle = "not held" in
          let n = String.length needle and len = String.length m in
          let rec scan i =
            if i + n > len then false
            else if String.sub m i n = needle then true
            else scan (i + 1)
          in
          scan 0)
   | _ -> Alcotest.fail "refusal had no error string");
  (* 4. a value cited inline that does NOT rehash to its claimed hash is
     refused (the F1 trust-nothing-but-the-hash contract) *)
  let wrong_value =
    `Assoc
      [ ("src_prefix", `String src)
      ; ("dst_prefix", `String dst)
      ; ("ops", `List ops)
      ; ( "values"
        , `List
            [ `Assoc
                [ ("hash", `String (String.make 64 'c'))
                ; ("kind", `String "tree")
                ; ("payload", `String "22102000") ] ] ) ]
  in
  Api.fed_apply_core p ~auth:(auth_of pe) wrong_value >>= fun (code, _) ->
  Alcotest.(check int) "non-rehashing value refused (400)" 400 code;
  (* 5. authz: a peer writing outside its ns/<peer>/ is denied and the
     denial is journaled. *)
  Api.fed_apply_core p ~auth:(auth_of pe)
    (apply_body ~src ~dst:"ns/someone-else/x" ops)
  >>= fun (code, _) ->
  Alcotest.(check int) "foreign ns refused (403)" 403 code;
  ops_with p ~op:"fed-apply" ~path:"ns/someone-else/x" >>= fun d ->
  Alcotest.(check int) "denial journaled" 1 (List.length d);
  return ()

let test_f2_apply_admin_any_ns () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let a = auth_of me in
  let src = "f2src/admin/" in
  let dst = "ns/anything/admin/" in
  seed_path p ~actor:me.S.i_id ~path:(src ^ "v") ~ternary:"2202102010"
  >>= fun _ ->
  pull_ops p ~auth:a ~prefix:src ~after_seq:0 >>= fun ops ->
  Api.fed_apply_core p ~auth:a (apply_body ~src ~dst ops)
  >>= fun (code, _) ->
  Alcotest.(check int) "admin may write any ns (200)" 200 code;
  return ()

let () =
  let lwt name f = Alcotest.test_case name `Quick f in
  Tuna_test_eio.run "fed"
       [ ( "value-exchange"
         , [ lwt "tree roundtrip rehash" test_fed_tree_roundtrip
           ; lwt "bytes roundtrip rehash" test_fed_bytes_roundtrip
           ; lwt "program hash as tree" test_fed_program_hash
           ; lwt "absent 404 + journaled denial" test_fed_absent_and_journal
           ; lwt "peer attribution" test_fed_peer_attribution
           ; lwt "peer name validation" test_fed_peer_names ] )
       ; ( "ops-chain-sync"
         , [ lwt "pull verifiable window" test_f2_pull_window
           ; lwt "apply roundtrip re-derives the index" test_f2_apply_roundtrip
           ; lwt "re-apply shadows, never merges" test_f2_apply_shadow
           ; lwt "rejects: chain/prefix/value/rehash/authz" test_f2_apply_rejects
           ; lwt "admin may write any namespace" test_f2_apply_admin_any_ns ] )
       ]
