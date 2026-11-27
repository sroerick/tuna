(* M12 federation FED1 acceptance tests (borg/federation.borg, value
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

(* -- FED2: ops-chain sync (borg/federation.borg) --------------------------

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
     refused (the FED1 trust-nothing-but-the-hash contract) *)
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

(* -- dictionary visibility (borg/federation.borg dictionary-visibility) --

   A dictionary is private to its identity by default: the pull omits
   dict/<id>/... and ns/<peer>/dict/<id>/... rows for any caller that is
   not that identity or admin, unless the identity's dictionary is
   public (the fold of its dict-visibility rows).  last_seq and more stay
   computed over the UNFILTERED window, and a put/cas at vis/... never
   sets the flag. *)

let identity_of p ~is_admin ~name =
  S.bootstrap_identity p ~is_admin ~name ~token:(name ^ "-token") ()

let pull_paths p ~auth ~prefix ~after_seq ~limit =
  Api.fed_ops_core p ~auth ~prefix ~after_seq ~limit >>= fun (code, j) ->
  Alcotest.(check int) "pull 200" 200 code;
  (match J.Util.member "verified" j with
   | `Bool true -> ()
   | _ -> Alcotest.fail "pull window did not verify");
  let paths =
    match J.Util.member "ops" j with
    | `List ops ->
        List.map
          (fun oj ->
            match J.Util.member "path" oj with
            | `String s -> s
            | _ -> Alcotest.fail "op row has no path")
          ops
    | _ -> Alcotest.fail "pull response has no ops array"
  in
  let last_seq =
    match J.Util.member "last_seq" j with
    | `Int i -> i
    | _ -> Alcotest.fail "pull response has no last_seq"
  in
  let more =
    match J.Util.member "more" j with `Bool b -> b | _ -> Alcotest.fail "no more"
  in
  return (paths, last_seq, more)

let contains needle hay = List.exists (( = ) needle) hay

let test_vis_private_by_default () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  identity_of p ~is_admin:false ~name:"visOwnA" >>= fun a ->
  identity_of p ~is_admin:false ~name:"visOwnB" >>= fun b ->
  seed_path p ~actor:a.S.i_id ~path:"dict/visOwnA/foo" ~ternary:"2202102010"
  >>= fun _ ->
  seed_path p ~actor:a.S.i_id ~path:"plain/kept" ~ternary:"221010210"
  >>= fun _ ->
  S.ops_head_seq p >>= fun head ->
  (* B sees the ordinary row and no dict row; last_seq crosses them all *)
  pull_paths p ~auth:(auth_of b) ~prefix:"" ~after_seq:0 ~limit:1000
  >>= fun (paths, last_seq, _) ->
  Alcotest.(check bool) "B omits dict/visOwnA/foo" false
    (contains "dict/visOwnA/foo" paths);
  Alcotest.(check bool) "B still sees the ordinary row" true
    (contains "plain/kept" paths);
  Alcotest.(check int) "last_seq advances past hidden rows" (Int64.to_int head)
    last_seq;
  (* A sees its own rows *)
  pull_paths p ~auth:(auth_of a) ~prefix:"dict/visOwnA/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "A sees its own dict row" true
    (contains "dict/visOwnA/foo" paths);
  (* admin sees them *)
  pull_paths p ~auth:(auth_of me) ~prefix:"dict/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "admin sees the dict row" true
    (contains "dict/visOwnA/foo" paths);
  (* public makes B's next pull carry them, still verified *)
  S.dict_visibility_set p ~actor:a.S.i_id ~target:"visOwnA" ~public:true
  >>= fun _ ->
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/visOwnA/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "B sees A's dict row once public" true
    (contains "dict/visOwnA/foo" paths);
  (* and private again hides it: the fold is last row by seq *)
  S.dict_visibility_set p ~actor:a.S.i_id ~target:"visOwnA" ~public:false
  >>= fun _ ->
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/visOwnA/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "B omits A's dict row once private again" false
    (contains "dict/visOwnA/foo" paths);
  return ()

let test_vis_peer_namespace_rows () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  identity_of p ~is_admin:false ~name:"visNsA" >>= fun a ->
  identity_of p ~is_admin:false ~name:"visNsB" >>= fun b ->
  peer p "fed-peer-vispeer" >>= fun pe ->
  seed_path p ~actor:me.S.i_id ~path:"ns/vispeer/dict/visNsA/x"
    ~ternary:"2202102010"
  >>= fun _ ->
  pull_paths p ~auth:(auth_of b) ~prefix:"ns/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "B omits ns/P/dict/A row" false
    (contains "ns/vispeer/dict/visNsA/x" paths);
  pull_paths p ~auth:(auth_of pe) ~prefix:"ns/vispeer/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "P's own pull omits dict/A rows" false
    (contains "ns/vispeer/dict/visNsA/x" paths);
  pull_paths p ~auth:(auth_of a) ~prefix:"ns/vispeer/" ~after_seq:0 ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "A's pull carries them" true
    (contains "ns/vispeer/dict/visNsA/x" paths);
  return ()

let test_vis_paging_all_hidden () =
  setup () >>= fun p ->
  admin p >>= fun _ ->
  identity_of p ~is_admin:false ~name:"visPageA" >>= fun a ->
  identity_of p ~is_admin:false ~name:"visPageB" >>= fun b ->
  S.ops_head_seq p >>= fun head0 ->
  seed_path p ~actor:a.S.i_id ~path:"dict/visPageA/h1" ~ternary:"2202102010"
  >>= fun _ ->
  seed_path p ~actor:a.S.i_id ~path:"dict/visPageA/h2" ~ternary:"221010210"
  >>= fun _ ->
  seed_path p ~actor:a.S.i_id ~path:"dict/visPageA/h3" ~ternary:"2201022100"
  >>= fun _ ->
  (* a FULL (limit-row) window whose rows are all hidden: more is true,
     the op list is empty *)
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/visPageA/" ~after_seq:(Int64.to_int head0)
    ~limit:3
  >>= fun (paths, _, more) ->
  Alcotest.(check int) "full all-hidden window is empty" 0 (List.length paths);
  Alcotest.(check bool) "full all-hidden window answers more:true" true more;
  (* the next read is past the three hidden rows: a short window of the
     remaining rows answers more:false *)
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/visPageA/" ~after_seq:(Int64.to_int head0)
    ~limit:100
  >>= fun (paths, _, more) ->
  Alcotest.(check int) "short all-hidden final window is empty" 0
    (List.length paths);
  Alcotest.(check bool) "short all-hidden final window answers more:false" false
    more;
  return ()

let test_vis_setting_only_by_command () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  identity_of p ~is_admin:false ~name:"visSetA" >>= fun a ->
  identity_of p ~is_admin:false ~name:"visSetB" >>= fun b ->
  (* a non-admin may not set another identity's flag: refused, no row *)
  Tuna_server.Repl_cmd.execute p ~caller:b.S.i_id
    ~command:"dict-visibility visSetA public" ~inputs:[] ~grant_ids:[] ~fuel:1000
    ~size_cap:1000 ()
  >>= (function
        | Ok o ->
            Alcotest.failf "non-admin cross-set was accepted (%s)" o.Tuna_server.Repl_cmd.o_kind
        | Error _ -> return ());
  S.dict_visibility_public p >>= fun public ->
  Alcotest.(check bool) "refused set appended no row" false
    (List.mem "visSetA" public);
  (* an ordinary put/cas at vis/visSetA/public sets nothing *)
  seed_path p ~actor:me.S.i_id ~path:"vis/visSetA/public" ~ternary:"2202102010"
  >>= fun _ ->
  S.dict_visibility_public p >>= fun public ->
  Alcotest.(check bool) "ordinary write at vis/ sets nothing" false
    (List.mem "visSetA" public);
  (* an admin command does set it *)
  Tuna_server.Repl_cmd.execute p ~caller:me.S.i_id
    ~command:"dict-visibility visSetA public" ~inputs:[] ~grant_ids:[] ~fuel:1000
    ~size_cap:1000 ()
  >>= (function
        | Ok _ -> return ()
        | Error (_, msg) -> Alcotest.failf "admin set refused: %s" msg);
  S.dict_visibility_public p >>= fun public ->
  Alcotest.(check bool) "admin command sets the flag" true (List.mem "visSetA" public);
  (* the actor is the caller and the path names the target *)
  ops_with p ~op:"dict-visibility" ~path:"vis/visSetA/public" >>= fun rows ->
  (match rows with
   | [ o ] ->
       Alcotest.(check string) "setting actor is the caller" me.S.i_id o.S.o_actor
   | _ -> Alcotest.fail "expected exactly one dict-visibility row");
  ignore a;
  return ()

let test_vis_peer_push_changes_nothing () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let auth = auth_of me in
  peer p "fed-peer-vispush" >>= fun pe ->
  (* the window root keeps the vis/ segment: apply lands the suffix under
     ns/<peer>/, so the row arrives at ns/vispush/vis/visPushA/public *)
  let src = "vispushsrc/" in
  seed_path p ~actor:me.S.i_id ~path:"vispushsrc/vis/visPushA/public"
    ~ternary:"2202102010"
  >>= fun _ ->
  pull_ops p ~auth ~prefix:src ~after_seq:0 >>= fun ops ->
  Api.fed_apply_core p ~auth:(auth_of pe)
    (apply_body ~src ~dst:"ns/vispush/" ops)
  >>= fun (code, j) ->
  Alcotest.(check int) ("apply 200 (" ^ J.to_string j ^ ")") 200 code;
  (* it landed under the peer namespace and set nothing *)
  S.path_get p ~path:"ns/vispush/vis/visPushA/public" >>= fun row ->
  Alcotest.(check bool) "pushed vis row landed under ns/<peer>/" true
    (row <> None);
  S.dict_visibility_public p >>= fun public ->
  Alcotest.(check bool) "pushed vis row changed nothing" false
    (List.mem "visPushA" public);
  return ()

(* -- defs as ops (borg/local.borg l5-defs-as-ops) ---------------------

   A def appends exactly ONE put row at dict/<identity>/<name>; the
   derived repl_dict index is the fold of that log; the dictionary at a
   past op is the fold up to that seq; undef is a put of the zero-length
   bytes marker, never a delete.  Another identity's ops pull omits a
   private dictionary's rows (and still verifies), and includes them once
   that identity sets its dictionary public.  dict/ is reserved for the
   REPL host: a tree grant whose prefix reaches it is refused. *)

let test_l5_defs_as_ops () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  identity_of p ~is_admin:false ~name:"l5DefA" >>= fun a ->
  identity_of p ~is_admin:false ~name:"l5DefB" >>= fun b ->
  let run caller command =
    Tuna_server.Repl_cmd.execute p ~caller ~command ~inputs:[] ~grant_ids:[]
      ~fuel:10_000 ~size_cap:10_000 ()
  in
  let ok what = function
    | Ok _ -> return ()
    | Error (_, msg) -> Alcotest.failf "%s refused: %s" what msg
  in
  run a.S.i_id "def l5x %0" >>= ok "def";
  let path = "dict/l5DefA/l5x" in
  ops_with p ~op:"put" ~path >>= fun rows ->
  Alcotest.(check int) "a def appends exactly one put row" 1 (List.length rows);
  let def_seq =
    match rows with
    | [ o ] ->
        Alcotest.(check bool) "the row carries a value hash" true
          (o.S.o_value_hash <> None);
        o.S.o_seq
    | _ -> Alcotest.fail "expected exactly one def row"
  in
  (* the derived index is the fold of the log *)
  S.dict_fold p ~identity:"l5DefA" () >>= fun foldA ->
  S.dict_list p ~identity_id:a.S.i_id >>= fun listed ->
  Alcotest.(check (list (pair string string)))
    "repl_dict is the fold of the log" foldA
    (List.map
       (fun (d : S.dict_entry) ->
         (d.S.d_name, Tuna.Hash.hex_of_string d.S.d_ternary))
       listed);
  Alcotest.(check bool) "the name is in the effective dictionary" true
    (List.mem_assoc "l5x" foldA);
  (* the dictionary at a past op: the name is absent before its def *)
  S.dict_fold p ~to_seq:(Int64.pred def_seq) ~identity:"l5DefA" ()
  >>= fun past ->
  Alcotest.(check int) "a past fold does not hold the name" 0
    (List.length past);
  (* undef is a put of the marker, never a delete, and removes the name *)
  run a.S.i_id "undef l5x" >>= ok "undef";
  ops_with p ~op:"put" ~path >>= fun rows2 ->
  Alcotest.(check int) "undef appends one more put, not a delete"
    2 (List.length rows2);
  Alcotest.(check bool) "the undef row cites the marker" true
    (List.exists
       (fun (o : S.tree_op) -> o.S.o_value_hash = Some S.undef_marker)
       rows2);
  S.dict_fold p ~identity:"l5DefA" () >>= fun after_undef ->
  Alcotest.(check int) "undef removes the name from the fold" 0
    (List.length after_undef);
  (* another identity's pull omits a private dictionary, still verifying *)
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/l5DefA/" ~after_seq:0
    ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "a private dictionary's rows are omitted" false
    (contains path paths);
  (* ... and carries them once the dictionary is public *)
  run a.S.i_id "dict-visibility public" >>= ok "dict-visibility";
  S.dict_visibility_set p ~actor:a.S.i_id ~target:"l5DefA" ~public:true
  >>= fun _ ->
  pull_paths p ~auth:(auth_of b) ~prefix:"dict/l5DefA/" ~after_seq:0
    ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "a public dictionary's rows are served" true
    (contains path paths);
  (* WRITE AUTHORITY: a tree grant whose prefix reaches dict/ is refused *)
  let mint prim prefix =
    let j =
      `Assoc
        (("prim", `String prim)
        :: (match prefix with
            | None -> []
            | Some s -> [ ("path_prefix", `String s) ]))
    in
    Api.grant_mint_core p (auth_of a) j
  in
  mint "tree/put" None >>= fun (code, _) ->
  Alcotest.(check int) "an unscoped tree grant is refused" 400 code;
  mint "tree/put" (Some "dict/l5DefA/") >>= fun (code, _) ->
  Alcotest.(check int) "a dict/ prefix is refused" 400 code;
  mint "tree/put" (Some "d") >>= fun (code, _) ->
  Alcotest.(check int) "a partial prefix reaching dict/ is refused" 400 code;
  ignore me;
  return ()

(* -- definitions as ops (borg/local.borg l5-defs-as-ops) ----------------

   A def is ONE put row at dict/<identity>/<name> in tree_ops; repl_dict
   is the derived index of that log, so the effective dictionary is
   unchanged after the migration, the dictionary at any past op is a fold
   up to that seq, and undef is a put of the zero-length-bytes marker,
   never a delete (apply replicates only rows carrying a value hash). *)

let test_l5_backfill_unchanged () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~is_admin:false ~name:"l5bk" ~token:"l5bk-token" ()
  >>= fun me ->
  (* a pre-L5 dictionary row the op log does not carry yet *)
  S.dict_set p ~identity_id:me.S.i_id ~name:"bkConst" ~ternary:"2202102010"
  >>= fun () ->
  S.dict_list p ~identity_id:me.S.i_id >>= fun before ->
  Alcotest.(check (list (pair string string))) "pre-backfill dictionary"
    [ ("bkConst", "2202102010") ]
    (List.map (fun d -> (d.S.d_name, d.S.d_ternary)) before);
  S.dict_backfill p >>= fun () ->
  (* after migration every identity's effective dictionary is unchanged *)
  S.dict_list p ~identity_id:me.S.i_id >>= fun after ->
  Alcotest.(check (list (pair string string)))
    "post-backfill dictionary is unchanged"
    (List.map (fun d -> (d.S.d_name, d.S.d_ternary)) before)
    (List.map (fun d -> (d.S.d_name, d.S.d_ternary)) after);
  (* history starts at the switch-over: one put op at the dict path *)
  ops_with p ~op:"put" ~path:"dict/l5bk/bkConst" >>= fun rows ->
  (match rows with
   | [ o ] ->
       Alcotest.(check (option string)) "backfill row cites the value hash"
         (Some (Tuna.Hash.hex_of_string "2202102010")) o.S.o_value_hash
   | _ -> Alcotest.fail "expected exactly one backfill put at the dict path");
  (* the dictionary at any past op is a fold of the log *)
  S.dict_fold p ~identity:"l5bk" () >>= fun fold ->
  Alcotest.(check (list (pair string string))) "fold holds the entry"
    [ ("bkConst", Tuna.Hash.hex_of_string "2202102010") ] fold;
  (* idempotent: a second backfill appends nothing *)
  S.dict_backfill p >>= fun () ->
  ops_with p ~op:"put" ~path:"dict/l5bk/bkConst" >>= fun rows2 ->
  Alcotest.(check int) "backfill is idempotent" 1 (List.length rows2);
  return ()

let test_l5_def_round_ops () =
  setup () >>= fun p ->
  S.bootstrap_identity p ~is_admin:false ~name:"l5def" ~token:"l5def-token" ()
  >>= fun me ->
  let run command =
    Tuna_server.Repl_cmd.execute p ~caller:me.S.i_id ~command ~inputs:[]
      ~grant_ids:[] ~fuel:1000 ~size_cap:1000 ()
    >>= function
    | Ok _ -> return ()
    | Error (_, msg) -> Alcotest.failf "round refused: %s" msg
  in
  run "def bk %22102000" >>= fun () ->
  ops_with p ~op:"put" ~path:"dict/l5def/bk" >>= fun rows ->
  Alcotest.(check int) "a def round appends exactly one put row" 1
    (List.length rows);
  let first = List.hd rows in
  Alcotest.(check (option string)) "the row cites the defined tree"
    (Some (Tuna.Hash.hex_of_string "22102000")) first.S.o_value_hash;
  (* the dictionary at that past op is the fold up to its seq *)
  S.dict_fold p ~to_seq:first.S.o_seq ~identity:"l5def" () >>= fun past ->
  Alcotest.(check (list (pair string string))) "past dictionary is a fold"
    [ ("bk", Tuna.Hash.hex_of_string "22102000") ] past;
  (* a redef appends one more row; the fold follows the log *)
  run "def bk %0" >>= fun () ->
  ops_with p ~op:"put" ~path:"dict/l5def/bk" >>= fun rows2 ->
  Alcotest.(check int) "a redef appends one more row" 2 (List.length rows2);
  S.dict_fold p ~identity:"l5def" () >>= fun now ->
  S.dict_get p ~identity_id:me.S.i_id ~name:"bk" >>= fun derived ->
  (match derived with
   | Some d ->
       Alcotest.(check (pair string string))
         "the dictionary is the log's fold"
         ("bk", Tuna.Hash.hex_of_string d.S.d_ternary) (List.hd now)
   | None -> Alcotest.fail "derived row missing after a def");
  (* undef is a put of the marker: no delete row ever lands on dict/ *)
  run "undef bk" >>= fun () ->
  ops_with p ~op:"put" ~path:"dict/l5def/bk" >>= fun rows3 ->
  Alcotest.(check int) "undef appends a put, not a delete" 3
    (List.length rows3);
  (match List.rev rows3 with
   | last :: _ ->
       Alcotest.(check (option string)) "undef is a put of the marker"
         (Some S.undef_marker) last.S.o_value_hash
   | [] -> Alcotest.fail "expected dict rows");
  S.ops_fold p ~prefix:"dict/l5def/" () >>= fun dict_ops ->
  Alcotest.(check bool) "dict/ carries no delete op" false
    (List.exists (fun (o : S.tree_op) -> o.S.o_op = "delete") dict_ops);
  S.dict_fold p ~identity:"l5def" () >>= fun after_undef ->
  Alcotest.(check (list (pair string string)))
    "undef removes the name from the fold" [] after_undef;
  S.dict_list p ~identity_id:me.S.i_id >>= fun rows4 ->
  Alcotest.(check int) "the derived index drops the name" 0 (List.length rows4);
  return ()

(* -- L5: definitions as ops (borg/local.borg l5-defs-as-ops) ----------

   A def is a put of the defined tree's value hash at the path
   dict/<identity>/<name> in the tree_ops chain; repl_dict is the derived
   index of that log.  So a def round appends exactly ONE row at its dict
   path, the dictionary at any past op is a fold, and the migration
   backfill carries a pre-existing repl_dict row into the log without
   changing the effective dictionary.  The dictionary is still private by
   default on the pull surface. *)

(* a per-run suffix: the scratch db is never recreated between runs *)
let fresh_seq = ref 0

let fresh_name prefix =
  incr fresh_seq;
  Printf.sprintf "%s%d" prefix !fresh_seq

let test_l5_def_is_one_op () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let who = fresh_name "l5Def" in
  identity_of p ~is_admin:false ~name:who >>= fun a ->
  let path = "dict/" ^ who ^ "/l5id" in
  let count () =
    ops_with p ~op:"put" ~path >>= fun ops -> return (List.length ops)
  in
  let def src =
    Tuna_server.Repl_cmd.execute p ~caller:a.S.i_id ~command:("def " ^ src)
      ~inputs:[] ~grant_ids:[] ~fuel:1000 ~size_cap:1000 ()
    >>= (function
          | Ok o ->
              Alcotest.(check string) "a def round answers kind def" "def"
                o.Tuna_server.Repl_cmd.o_kind;
              return ()
          | Error (_, msg) -> Alcotest.failf "def round refused: %s" msg)
  in
  count () >>= fun before ->
  def "l5id (lambda (x) x)" >>= fun () ->
  count () >>= fun after ->
  Alcotest.(check int) "a def round appends exactly one tree_ops row"
    (before + 1) after;
  ops_with p ~op:"put" ~path >>= fun rows ->
  let last = List.nth rows (List.length rows - 1) in
  (* the row names the def value hash; the version is local and climbs *)
  Alcotest.(check string) "the row's actor is the def round's caller" a.S.i_id
    last.S.o_actor;
  Alcotest.(check bool) "the row carries a value hash" true
    (Option.is_some last.S.o_value_hash);
  Alcotest.(check (option int64)) "the version climbs by one per op"
    (Some (Int64.of_int (before + 1))) last.S.o_version;
  Alcotest.(check (option int64)) "the row cites its predecessor version"
    (if before = 0 then None else Some (Int64.of_int before))
    last.S.o_prev_version;
  (* the derived index is the fold of the log *)
  S.dict_list p ~identity_id:a.S.i_id >>= fun rows_d ->
  S.dict_fold p ~identity:who () >>= fun fold ->
  (match (rows_d, List.assoc_opt "l5id" fold) with
   | [ d ], Some h ->
       Alcotest.(check string) "the derived index keeps the name" "l5id"
         d.S.d_name;
       Alcotest.(check (option string)) "index and fold hold the same hash"
         last.S.o_value_hash (Some h)
   | _ -> Alcotest.fail "expected one derived row agreeing with the fold");
  (* the dictionary at any past op is a fold: after a re-def, the fold up
     to the first row still holds the FIRST binding *)
  let first_seq = last.S.o_seq in
  def "l5id (lambda (y) (lambda (x) x))" >>= fun () ->
  S.dict_fold p ~to_seq:first_seq ~identity:who () >>= fun past ->
  S.dict_fold p ~identity:who () >>= fun now ->
  (match (List.assoc_opt "l5id" past, List.assoc_opt "l5id" now) with
   | Some old_h, Some new_h ->
       Alcotest.(check bool) "the past fold is the earlier binding" true
         (old_h <> new_h);
       Alcotest.(check (option string)) "the past fold is the first row's hash"
         last.S.o_value_hash (Some old_h)
   | _ -> Alcotest.fail "both folds must bind the name");
  (* another identity's pull omits the private dictionary's rows and the
     window still verifies; once public, the rows arrive *)
  identity_of p ~is_admin:false ~name:(fresh_name "l5Peer") >>= fun b ->
  pull_paths p ~auth:(auth_of b) ~prefix:("dict/" ^ who ^ "/") ~after_seq:0
    ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "a private dictionary's rows are omitted" false
    (contains path paths);
  S.dict_visibility_set p ~actor:me.S.i_id ~target:who ~public:true
  >>= fun _ ->
  pull_paths p ~auth:(auth_of b) ~prefix:("dict/" ^ who ^ "/") ~after_seq:0
    ~limit:1000
  >>= fun (paths, _, _) ->
  Alcotest.(check bool) "a public dictionary's rows are served" true
    (contains path paths);
  return ()

let test_l5_backfill_keeps_dictionary () =
  setup () >>= fun p ->
  let who = fresh_name "l5Back" in
  identity_of p ~is_admin:false ~name:who >>= fun bf ->
  let path = "dict/" ^ who ^ "/l5old" in
  let count () =
    ops_with p ~op:"put" ~path >>= fun ops -> return (List.length ops)
  in
  (* a derived row the log does not carry yet (the pre-migration shape) *)
  S.dict_set p ~identity_id:bf.S.i_id ~name:"l5old" ~ternary:"22102000"
  >>= fun () ->
  count () >>= fun before ->
  Alcotest.(check int) "no op row before the migration" 0 before;
  S.dict_backfill p >>= fun () ->
  count () >>= fun after ->
  Alcotest.(check int) "the backfill appends one row" 1 after;
  (* the effective dictionary is unchanged and is now a fold of the log *)
  S.dict_list p ~identity_id:bf.S.i_id >>= fun rows ->
  (match rows with
   | [ d ] ->
       Alcotest.(check string) "the effective dictionary is unchanged"
         "22102000" d.S.d_ternary
   | _ -> Alcotest.fail "the backfill must not touch the derived row");
  S.dict_fold p ~identity:who () >>= fun fold ->
  (match List.assoc_opt "l5old" fold with
   | Some h ->
       Alcotest.(check string) "the fold binds the backfilled value hash"
         (Tuna.Hash.hex_of_string "22102000") h
   | None -> Alcotest.fail "the fold must hold the backfilled name");
  (* idempotent: a second run appends nothing *)
  S.dict_backfill p >>= fun () ->
  count () >>= fun again ->
  Alcotest.(check int) "the backfill is idempotent" 1 again;
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
       ; ( "dictionary-visibility"
         , [ lwt "private by default, owner/admin/public" test_vis_private_by_default
           ; lwt "ns/<peer>/dict/<id> rows" test_vis_peer_namespace_rows
           ; lwt "paging over an all-hidden window" test_vis_paging_all_hidden
           ; lwt "set only through the command" test_vis_setting_only_by_command
           ; lwt "a peer push of vis/ changes nothing"
               test_vis_peer_push_changes_nothing ] )
       ]
