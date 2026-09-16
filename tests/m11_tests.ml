(* M11 acceptance tests: byte values (borg/byte-values.borg), routes as
   paths (borg/routes.borg) and the tree/del boundary prim.  Integration
   tests require Postgres (scripts/dev.sh start-pg); skipped silently
   otherwise via scripts/test-store.sh gating TUNA_TEST_PG=1.  Runs on
   its OWN scratch db (tuna_test_m11) so the three PG suites can run in
   parallel.

   Exercise order: the Dream-free cores (Api.value_put_core /
   value_get_core, Routes.publish/delete/get/dispatch) and the prim
   boundary (Value_prims / Tree_prims / Run.execute) directly -- the
   acceptance chapters are about store behavior, not HTTP plumbing;
   Api.dispatch_route adapts the same records to Dream. *)

open Lwt.Infix

module Db = Tuna_store.Db
module S = Tuna_store.Store
module Tp = Tuna_server.Tree_prims
module Vp = Tuna_server.Value_prims
module Rt = Tuna_server.Routes
module Rw = Tuna_server.Rewind
module Rn = Tuna_server.Run
module Api = Tuna_server.Api
module C = Tuna_compiler.Bracket
module J = Yojson.Basic
module P = Tuna_server.Prims

(* every test opens with this: connect to the scratch db the gate
   script selected and bring the migrations up before touching tables *)
let setup () =
  Db.init (Db.config_from_env ()) >>= fun p ->
  Db.apply_migrations p
    ~dir:(try Sys.getenv "TUNA_TEST_MIGRATIONS" with Not_found -> "../migrations")
  >>= fun _ -> Lwt.return p

(* identities: the bootstrap admin (default is_admin=true) and non-admin
   peers for grant-holder / capability-denial paths *)
let admin p = S.bootstrap_identity p ~name:"m11-root" ~token:"m11-root-token" ()

let peer p name =
  S.bootstrap_identity p ~is_admin:false ~name ~token:(name ^ "-token") ()

let auth_of i =
  { Api.auth_id = i.S.i_id
  ; Api.auth_name = i.S.i_name
  ; Api.auth_is_admin = i.S.i_is_admin }

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else go (i + 1)
  in
  m = 0 || go 0

(* compile a source program and persist it (with provenance ir json) *)
let seed_program p ~caller src =
  let art = C.compile_source src in
  let ir_json = Yojson.Basic.to_string (Api.ir_json_of_artifact art) in
  S.upsert_program p ~hash:art.C.hash_hex ~ternary:art.C.ternary
    ~ir:(Some ir_json) ~created_by:caller
  >>= fun _ -> Lwt.return art

(* independent sha256: the ADDRESS LAW criterion compares the store's
   hash against digestif called directly in the test *)
let independent_sha256 bytes =
  String.lowercase_ascii
    (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string bytes))

let json_string j k = match J.Util.member k j with `String s -> Some s | _ -> None

let json_int j k = match J.Util.member k j with `Int n -> Some n | _ -> None

let header hdrs name = List.assoc_opt name hdrs

(* the tree_ops rows of one op kind at one path, whole log *)
let ops_with p ~op ~path =
  S.ops_fold p ~prefix:"" ~from_seq:0L ()
  >>= fun ops ->
  Lwt.return
    (List.filter
       (fun (o : S.tree_op) -> o.S.o_op = op && o.S.o_path = path)
       ops)

(* -- byte values: round-trip, address law, kind disjoint --------------- *)

(* exactly 100 KiB of deterministic HTML *)
let html_page () =
  let b = Buffer.create (1024 * 1024) in
  Buffer.add_string b "<html><body>\n";
  while Buffer.length b < 102_400 do
    Buffer.add_string b
      "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>\n"
  done;
  Buffer.add_string b "</body></html>\n";
  let s = Buffer.contents b in
  String.sub s 0 102_400

let corpus =
  [ ("empty", "")
  ; ("1-byte", "\001")
  ; ("high-bit", "\255\254\128\127")
    (* valid ternary bytes: the byte kind and the tree kind must stay
       disjoint even when the bytes would parse as ternary *)
  ; ("ternary-shaped", "2210200010")
  ; ("100k-html", html_page ()) ]

let test_byte_roundtrip () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let a = auth_of me in
  let rec iter_corpus = function
    | [] -> Lwt.return ()
    | (label, bytes) :: rest ->
      let b64 = Base64.encode_string bytes in
      (* the API surface *)
      Api.value_put_core p ~auth:a ~bytes
      >>= fun (code, j) ->
      Alcotest.(check int) (label ^ ": api put 201") 201 code;
      let api_hash = Option.value (json_string j "hash") ~default:"missing" in
      Alcotest.(check string) (label ^ ": hash = independent sha256")
        (independent_sha256 bytes) api_hash;
      Alcotest.(check int) (label ^ ": api len") (String.length bytes)
        (Option.value (json_int j "len") ~default:(-1));
      (* the prim path must hash identically (no canonicalization drift
         between the prim path and the API path, acceptance 2) *)
      Vp.dispatch ~pool:p ~actor:me.S.i_id ~name:"value/put"
        ~args:(P.tree_of_list [ P.str b64 ])
      >>= (function
            | `Ok t -> (
                match P.list_of_tree t with
                | [ h; kind; len ] ->
                    Alcotest.(check string) (label ^ ": prim hash = api hash")
                      api_hash (Option.value (P.unstr h) ~default:"?");
                    Alcotest.(check string) (label ^ ": prim kind") "bytes"
                      (Option.value (P.unstr kind) ~default:"?");
                    Alcotest.(check string) (label ^ ": prim len")
                      (Int64.to_string (Int64.of_int (String.length bytes)))
                      (Option.value (P.unstr len) ~default:"?");
                    Lwt.return ()
                | _ ->
                    Alcotest.fail
                      (label ^ ": prim put result must be [hash kind len]"))
            | `Error e -> Alcotest.failf "%s: prim put denied: %s" label e)
      >>= fun () ->
      (* round-trip through the API *)
      Api.value_get_core p ~auth:a ~hash:api_hash ~kind:(Some "bytes")
      >>= fun (gcode, gct, gbody, ghdrs) ->
      Alcotest.(check int) (label ^ ": api get 200") 200 gcode;
      Alcotest.(check string) (label ^ ": api get content type")
        "application/octet-stream" gct;
      Alcotest.(check string) (label ^ ": byte-identical round-trip") bytes gbody;
      Alcotest.(check (option string)) (label ^ ": X-Tuna-Kind header")
        (Some "bytes") (header ghdrs "X-Tuna-Kind");
      (* round-trip through the prim; results above the journal payload
         cap are error ANSWERS by convention (like tree/list) - the API
         surface serves big payloads out-of-band - so the prim get only
         round-trips payloads that fit, and the over-cap answer is
         asserted as the documented error *)
      (if String.length b64 > P.payload_cap then
         Vp.dispatch ~pool:p ~actor:me.S.i_id ~name:"value/get"
           ~args:(P.tree_of_list [ P.str api_hash; P.str "bytes" ])
         >>= (function
               | `Error e ->
                   Alcotest.(check bool)
                     (label ^ ": over-cap prim get answers the cap error") true
                     (contains e "payload cap");
                   Lwt.return ()
               | `Ok _ ->
                   Alcotest.fail
                     (label ^ ": over-cap prim get must be an error answer"))
       else
         Vp.dispatch ~pool:p ~actor:me.S.i_id ~name:"value/get"
           ~args:(P.tree_of_list [ P.str api_hash; P.str "bytes" ])
         >>= (function
               | `Ok t -> (
                   match P.list_of_tree t with
                   | [ _h; _kind; _len; payload ] ->
                       let payload = Option.value (P.unstr payload) ~default:"?" in
                       Alcotest.(check string)
                         (label ^ ": prim round-trip byte-identical") bytes
                         (Base64.decode_exn payload);
                       Lwt.return ()
                   | _ ->
                       Alcotest.fail
                         (label
                         ^ ": prim get result must be [hash kind len payload]"))
               | `Error e -> Alcotest.failf "%s: prim get denied: %s" label e))
      >>= fun () -> iter_corpus rest
  in
  iter_corpus corpus
  >>= fun () ->
  (* kind disjoint (law 1): the ternary-shaped byte value is NOT in the
     tree store until a tree twin is stored; same hash, both tables, no
     coalescing *)
  let tbytes = List.assoc "ternary-shaped" corpus in
  let thash = independent_sha256 tbytes in
  Api.value_get_core p ~auth:a ~hash:thash ~kind:(Some "tree")
  >>= fun (kcode, _, _, _) ->
  Alcotest.(check int) "tree kind absent while only bytes stored" 404 kcode;
  S.value_put p ~hash:thash ~ternary:tbytes >>= fun () ->
  Api.value_get_core p ~auth:a ~hash:thash ~kind:(Some "tree")
  >>= fun (tcode, _, tbody, thdrs) ->
  Alcotest.(check int) "tree kind present after tree twin" 200 tcode;
  Alcotest.(check string) "tree twin returns the ternary" tbytes tbody;
  Alcotest.(check (option string)) "tree kind header" (Some "tree")
    (header thdrs "X-Tuna-Kind");
  Api.value_get_core p ~auth:a ~hash:thash ~kind:(Some "bytes")
  >>= fun (bcode, _, bbody, _) ->
  Alcotest.(check int) "byte kind still present" 200 bcode;
  Alcotest.(check string) "byte kind unchanged" tbytes bbody;
  Lwt.return ()

let test_byte_dedup_and_bloat () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let bytes = List.assoc "100k-html" corpus in
  Api.value_put_core p ~auth:(auth_of me) ~bytes >>= fun (code1, j1) ->
  Alcotest.(check int) "first put 201" 201 code1;
  Api.value_put_core p ~auth:(auth_of me) ~bytes >>= fun (code2, j2) ->
  Alcotest.(check int) "double put 201 (dedup no-op)" 201 code2;
  let h1 = Option.value (json_string j1 "hash") ~default:"?" in
  let h2 = Option.value (json_string j2 "hash") ~default:"?" in
  Alcotest.(check string) "dedup returns the same hash" h1 h2;
  Db.q ~params:[ S.p_str h1 ] p
    "SELECT count(*)::text FROM byte_values WHERE hash = $1"
  >>= (function
        | [ r ] ->
            Alcotest.(check string) "storage does not grow (one row)" "1"
              (S.text r 0 "count");
            Lwt.return ()
        | _ -> Alcotest.fail "count query shape")
  >>= fun () ->
  (* BLOAT INVERSION (byte-values.borg acceptance 3): the byte form is
     strictly smaller than the unary/ternary tree encoding of the same
     page.  Cstr (common/lib/cstr.ml) encodes byte b as Fork (Stem
     (unary b), rest), so the ternary length of the page-as-string-tree
     is 1 + sum(byte + 3) (a leaf for nil; fork + stem marker + b stems
     + leaf per byte).  The closed form is checked against a real
     Canon.encode on a small prefix, then applied to the 100 KiB page. *)
  let ternary_len s =
    let sum = ref 1 (* the final Leaf *) in
    String.iter (fun c -> sum := !sum + Char.code c + 3) s;
    !sum
  in
  let small = "<p>hi</p>" in
  Alcotest.(check int) "closed form = real Cstr ternary length"
    (ternary_len small)
    (String.length (Tuna.Canon.encode (Tuna.Cstr.encode small)));
  let tree_len = ternary_len bytes in
  Alcotest.(check bool) "byte form strictly smaller than tree encoding" true
    (String.length bytes < tree_len);
  Printf.printf "bloat ratio (tree-encoding bytes / raw bytes), 100 KiB page: %.1fx\n"
    (float_of_int tree_len /. float_of_int (String.length bytes));
  Lwt.return ()

(* -- journal completeness: cap denial + absent read are journaled ------ *)

let test_value_denials_journaled () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  (* cap denial (law 3: journaled answer, never an exception): the 1 MiB
     default cap + 1 byte *)
  let big = String.make (S.value_max_bytes () + 1) 'x' in
  Api.value_put_core p ~auth:(auth_of me) ~bytes:big
  >>= fun (code, j) ->
  Alcotest.(check int) "cap answers 413" 413 code;
  Alcotest.(check bool) "cap denial carries an error message" true
    (Option.is_some (json_string j "error"));
  ops_with p ~op:"value-put" ~path:""
  >>= (function
  | [ o ] ->
      Alcotest.(check (option string)) "cap denial row has no value" None
        o.S.o_value_hash;
      Alcotest.(check (option int64)) "cap denial row has no version" None
        o.S.o_version;
      Lwt.return ()
  | n -> Alcotest.failf "expected one value-put denial row, got %d" (List.length n))
  >>= fun () ->
  (* absent read journaled (acceptance 4: every value op journals) *)
  let ghost = String.make 64 'a' in
  Api.value_get_core p ~auth:(auth_of me) ~hash:ghost ~kind:None
  >>= fun (gcode, _, _, _) ->
  Alcotest.(check int) "absent read answers 404" 404 gcode;
  ops_with p ~op:"value-get" ~path:ghost
  >>= function
  | [ o ] ->
      Alcotest.(check (option string)) "absent read row has no value" None
        o.S.o_value_hash;
      Lwt.return ()
  | n -> Alcotest.failf "expected one value-get denial row, got %d" (List.length n)

(* -- routes: template dispatch, reserved shadow, program runs ---------- *)

let record ~meth ?(template = None) ?(program = None) ?(grant_prefix = None) ct =
  `Assoc
    [ ("method", `String meth)
    ; ("template", match template with Some h -> `String h | None -> `Null)
    ; ("program", match program with Some h -> `String h | None -> `Null)
    ; ("content_type", `String ct)
    ; ("grant_prefix", match grant_prefix with Some g -> `String g | None -> `Null) ]

let test_template_route () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "m11-pub" >>= fun pub ->
  (* the publisher holds a covering grant on route/ (the non-admin path) *)
  S.mint_grant p ~prim:"tree/put" ~args_attenuation:"{}"
    ~path_prefix:(Some "route/") ~caller:pub.S.i_id ()
  >>= fun _g ->
  let bytes = "<h1>m11 says hi</h1>\n" in
  Api.value_put_core p ~auth:(auth_of me) ~bytes >>= fun (_, tj) ->
  let thash = Option.value (json_string tj "hash") ~default:"?" in
  Rt.publish p ~caller_id:pub.S.i_id ~caller_admin:false ~site_path:"hello"
    ~record:(record ~meth:"GET" ~template:(Some thash) "text/html; charset=utf-8")
    ~expected_version:None
  >>= fun (pcode, pj) ->
  Alcotest.(check int) "publish with covering grant 201" 201 pcode;
  Alcotest.(check bool) "publish ok" true
    (match J.Util.member "ok" pj with `Bool b -> b | _ -> false);
  (* anonymous dispatch: byte-identical, the record's content_type, the
     X-Tuna-Route header (law 3 + acceptance 1) *)
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"hello" ~query:None
    ~body:"" ~actor:None
  >>= fun r ->
  Alcotest.(check int) "anonymous dispatch 200" 200 r.Rt.code;
  Alcotest.(check string) "content_type from the record"
    "text/html; charset=utf-8" r.Rt.content_type;
  Alcotest.(check string) "byte-identical to the stored template" bytes r.Rt.body;
  Alcotest.(check (option string)) "X-Tuna-Route header" (Some "hello")
    (header r.Rt.headers "X-Tuna-Route");
  (* law 5: method mismatch answers plain 404 *)
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"POST" ~site_path:"hello" ~query:None
    ~body:"" ~actor:None
  >>= fun r404 ->
  Alcotest.(check int) "method mismatch 404" 404 r404.Rt.code;
  Lwt.return ()

let test_reserved_shadow () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let tmpl = record ~meth:"GET" ~template:(Some (String.make 64 'b')) "text/html" in
  Rt.publish p ~caller_id:me.S.i_id ~caller_admin:true ~site_path:"health"
    ~record:tmpl ~expected_version:None
  >>= fun (code, j) ->
  Alcotest.(check int) "reserved shadow denied" 400 code;
  Alcotest.(check bool) "denial names the reserved collision" true
    (match json_string j "error" with
     | Some msg -> contains msg "reserved"
     | None -> false);
  (* the denial is journaled (NULL effect) and nothing was published *)
  ops_with p ~op:"put" ~path:"route/health"
  >>= (function
  | [ o ] ->
      Alcotest.(check (option string)) "denial row has no value" None
        o.S.o_value_hash;
      Alcotest.(check (option int64)) "denial row has no version" None
        o.S.o_version;
      Lwt.return ()
  | n ->
      Alcotest.failf "expected one reserved-shadow denial row, got %d"
        (List.length n))
  >>= fun () ->
  (* the reserved surface still answers: dispatch never serves a
     reserved site path (law 2 - static wins, always; the reserved
     matchers mount before the routes catch-all in Api.router) *)
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"health" ~query:None
    ~body:"" ~actor:None
  >>= fun r ->
  Alcotest.(check int) "dispatch over a reserved path 404s" 404 r.Rt.code;
  Alcotest.(check string) "dispatch reserved answers plain 404" "not found"
    r.Rt.body;
  Lwt.return ()

let test_program_route () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  (* law 4: the run must end in a value hash.  The program is a constant
     returning the stored hash as a tree literal (a Cstr of the hash), so
     the run ends Normal in a value hash and the stored bytes are served
     with the record's content_type. *)
  let body = "{\"served\":true}" in
  Api.value_put_core p ~auth:(auth_of me) ~bytes:body >>= fun (_, bj) ->
  let bhash = Option.value (json_string bj "hash") ~default:"?" in
  let lit = Tuna.Canon.encode (Tuna.Cstr.encode bhash) in
  seed_program p ~caller:(Some me.S.i_id) ("(lambda (ctx) %" ^ lit ^ ")")
  >>= fun art ->
  Rt.publish p ~caller_id:me.S.i_id ~caller_admin:true ~site_path:"compute"
    ~record:(record ~meth:"ANY" ~program:(Some art.C.hash_hex) "application/json")
    ~expected_version:None
  >>= fun (pcode, _) ->
  Alcotest.(check int) "program route published" 201 pcode;
  let invoker = me.S.i_id in
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"compute" ~query:None
    ~body:"" ~actor:(Some invoker)
  >>= fun r ->
  Alcotest.(check int) "program route 200" 200 r.Rt.code;
  Alcotest.(check string) "law-4 finish: value bytes served" body r.Rt.body;
  Alcotest.(check string) "content_type from the record" "application/json"
    r.Rt.content_type;
  let run_id = Option.value (header r.Rt.headers "X-Tuna-Run") ~default:"?" in
  Alcotest.(check bool) "X-Tuna-Run present" true (run_id <> "?");
  (* the run row is journaled, attributable, and the request context
     reached the program as the input (acceptance 2) *)
  S.fetch_run p run_id
  >>= (function
        | None -> Alcotest.fail "run row vanished"
        | Some row ->
            Alcotest.(check string) "run normal" "normal"
              (S.Run_status.to_string row.S.r_status);
            Alcotest.(check string) "run attributed to the invoker" invoker
              (Option.value row.S.r_caller ~default:"");
            let ctx =
              Rt.context_json ~meth:"GET" ~site_path:"compute" ~query:None
                ~body:"" ~actor:(Some invoker)
            in
            Alcotest.(check (list string))
              "the request context is the run's input"
              [ Tuna.Hash.hex_of_tree (Tuna.Cstr.encode (J.to_string ctx)) ]
              row.S.r_input_hashes;
            Lwt.return ())
  >>= fun () ->
  S.fetch_journals p run_id >>= fun _js -> Lwt.return ()
  >>= fun () ->
  (* and an anonymous visitor gets a daemon-attributed run (law 3) *)
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"compute" ~query:None
    ~body:"" ~actor:None
  >>= fun r2 ->
  Alcotest.(check int) "anonymous program dispatch 200" 200 r2.Rt.code;
  let run2 = Option.value (header r2.Rt.headers "X-Tuna-Run") ~default:"?" in
  S.fetch_run p run2
  >>= (function
        | None -> Alcotest.fail "anonymous run row vanished"
        | Some row ->
            Alcotest.(check string) "anonymous run attributed to the daemon"
              me.S.i_id (Option.value row.S.r_caller ~default:"");
            Lwt.return ())

let test_route_capability_and_lifecycle () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "m11-nog" >>= fun nog ->
  let tmpl = record ~meth:"GET" ~template:(Some (String.make 64 'c')) "text/plain" in
  (* capability check (acceptance 4): publish/delete without a covering
     grant are journaled denials *)
  Rt.publish p ~caller_id:nog.S.i_id ~caller_admin:false ~site_path:"locked"
    ~record:tmpl ~expected_version:None
  >>= fun (pcode, _) ->
  Alcotest.(check int) "publish without covering grant 403" 403 pcode;
  Rt.delete p ~caller_id:nog.S.i_id ~caller_admin:false ~site_path:"locked"
    ~expected_version:None
  >>= fun (dcode, _) ->
  Alcotest.(check int) "delete without covering grant 403" 403 dcode;
  ops_with p ~op:"put" ~path:"route/locked"
  >>= (function
  | [ o ] ->
      Alcotest.(check (option string)) "publish denial journaled NULL" None
        o.S.o_value_hash;
      Lwt.return ()
  | n -> Alcotest.failf "expected publish denial row, got %d" (List.length n))
  >>= fun () ->
  ops_with p ~op:"delete" ~path:"route/locked"
  >>= (function
  | [ _ ] -> Lwt.return ()
  | n -> Alcotest.failf "expected delete denial row, got %d" (List.length n))
  >>= fun () ->
  (* lifecycle (acceptance 5): publish, dispatch, delete, 404 *)
  S.mint_grant p ~prim:"tree/put" ~args_attenuation:"{}"
    ~path_prefix:(Some "route/tmp") ~caller:nog.S.i_id ()
  >>= fun _ ->
  let tmpl_bytes = "scoped template\n" in
  Api.value_put_core p ~auth:(auth_of me) ~bytes:tmpl_bytes
  >>= fun (_, tmj) ->
  let thash = Option.value (json_string tmj "hash") ~default:"?" in
  let tmpl2 = record ~meth:"GET" ~template:(Some thash) "text/plain" in
  Rt.publish p ~caller_id:nog.S.i_id ~caller_admin:false ~site_path:"tmp/x"
    ~record:tmpl2 ~expected_version:None
  >>= fun (ok_code, _) ->
  Alcotest.(check int) "scoped covering grant publishes" 201 ok_code;
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"tmp/x" ~query:None
    ~body:"" ~actor:None
  >>= fun r1 ->
  Alcotest.(check int) "serves before delete" 200 r1.Rt.code;
  Alcotest.(check string) "template bytes served" tmpl_bytes r1.Rt.body;
  Rt.delete p ~caller_id:nog.S.i_id ~caller_admin:false ~site_path:"tmp/x"
    ~expected_version:None
  >>= fun (del_code, dj) ->
  Alcotest.(check int) "delete with covering grant 200" 200 del_code;
  Alcotest.(check bool) "delete carries the deleted version" true
    (Option.is_some (json_int dj "deleted_version"));
  Rt.dispatch p ~daemon:me.S.i_id ~meth:"GET" ~site_path:"tmp/x" ~query:None
    ~body:"" ~actor:None
  >>= fun r2 ->
  Alcotest.(check int) "deleted route answers 404" 404 r2.Rt.code;
  Rt.get p ~actor:me.S.i_id ~site_path:"tmp/x" >>= fun (gcode, _) ->
  Alcotest.(check int) "route get after delete 404" 404 gcode;
  Lwt.return ()

(* rewind over route/ restores the prior route table exactly (routes are
   paths, so rewind-as-fold covers them; acceptance 5) *)
let test_route_rewind () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  (* fold and assert over the rw/ slice so other tests' routes (they run
     in the same scratch db) stay out of the picture *)
  let rec_ h = record ~meth:"GET" ~template:h "text/plain" in
  Rt.publish p ~caller_id:me.S.i_id ~caller_admin:true ~site_path:"rw/a"
    ~record:(rec_ (Some (String.make 64 '1'))) ~expected_version:None
  >>= fun _ ->
  S.ops_fold p ~prefix:"route/rw" ~from_seq:0L () >>= fun ops ->
  let head ops =
    List.fold_left (fun a (o : S.tree_op) -> Int64.max a o.S.o_seq) 0L ops
  in
  let after_a = head ops in
  Rt.publish p ~caller_id:me.S.i_id ~caller_admin:true ~site_path:"rw/b"
    ~record:(rec_ (Some (String.make 64 '2'))) ~expected_version:None
  >>= fun _ ->
  Rt.delete p ~caller_id:me.S.i_id ~caller_admin:true ~site_path:"rw/a"
    ~expected_version:None
  >>= fun _ ->
  (* live now: only rw/b *)
  S.path_list p ~prefix:"route/rw" () >>= fun live ->
  Alcotest.(check (list string)) "live table after publish+delete"
    [ "route/rw/b" ] (List.map (fun e -> e.S.tp_path) live);
  (* fold at head reproduces the live table *)
  S.ops_fold p ~prefix:"route/rw" ~from_seq:0L () >>= fun ops2 ->
  Rw.state p ~prefix:"route/rw" ~at_seq:(head ops2) >>= fun rew_head ->
  Alcotest.(check (list string)) "rewind at head = live table"
    (List.map (fun e -> e.S.tp_path) live)
    (List.map (fun e -> e.Rw.path) rew_head);
  (* rewind to just after rw/a's publish: the prior table, rw/a back *)
  Rw.state p ~prefix:"route/rw" ~at_seq:after_a >>= fun rew_prior ->
  Alcotest.(check (list string)) "rewind restores the prior route table"
    [ "route/rw/a" ] (List.map (fun e -> e.Rw.path) rew_prior);
  Lwt.return ()

(* -- tree/del at the boundary ------------------------------------------ *)

let delete_program p ~caller =
  seed_program p ~caller "(lambda (p) (prim \"tree/del\" p))"

let run_del p ~caller ~grant_ids ~art path =
  Rn.execute p ~caller ~grant_ids ~program_hash:art.C.hash_hex
    ~program:art.C.tree
    ~ir_json:(Some (Yojson.Basic.to_string (Api.ir_json_of_artifact art)))
    ~inputs:[ Tuna.Cstr.encode path ] ~fuel:10000 ~size_cap:100000 ()

let journal_error js =
  match js with
  | [ j ] -> j.S.j_error
  | _ -> Alcotest.fail "bad journal shape"

let test_tree_del_boundary () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  peer p "m11-del" >>= fun del ->
  delete_program p ~caller:(Some del.S.i_id) >>= fun art ->
  (* seed a put so the delete has something to remove *)
  let vh = Tuna.Hash.hex_of_string "10" in
  S.value_put p ~hash:vh ~ternary:"10" >>= fun () ->
  Tp.dispatch ~pool:p ~actor:me.S.i_id ~name:"tree/put"
    ~args:(P.tree_of_list [ Tuna.Cstr.encode "td/a/one"; Tuna.Canon.parse "10" ])
  >>= fun _ ->
  (* ALLOWED with a covering grant in the run's map *)
  S.mint_grant p ~prim:"tree/del" ~args_attenuation:"{}"
    ~path_prefix:(Some "td/a") ~caller:del.S.i_id ()
  >>= fun g ->
  run_del p ~caller:del.S.i_id ~grant_ids:[ g.S.g_id ] ~art "td/a/one"
  >>= fun (row_ok, js_ok) ->
  Alcotest.(check string) "delete run normal" "normal"
    (S.Run_status.to_string row_ok.S.r_status);
  Alcotest.(check (option string)) "allowed delete journaled no error" None
    (journal_error js_ok);
  (match
     Tuna.Cstr.decode
       (Tuna.Canon.parse (Option.value row_ok.S.r_result_ternary ~default:""))
   with
   | Some "deleted:2" -> ()
   | other ->
       Alcotest.failf "expected deleted:2, got %s"
         (Option.value other ~default:"undecodable"));
  S.path_get p ~path:"td/a/one"
  >>= fun gone ->
  Alcotest.(check bool) "path removed" true (Option.is_none gone);
  (* the effect row: op delete, value_hash NULL, prev = 1, version = 2 *)
  ops_with p ~op:"delete" ~path:"td/a/one"
  >>= (function
  | [ o ] ->
      Alcotest.(check (option string)) "delete row value_hash NULL" None
        o.S.o_value_hash;
      Alcotest.(check (option int64)) "delete row prev_version" (Some 1L)
        o.S.o_prev_version;
      Alcotest.(check (option int64)) "delete row version = the row's next version"
        (Some 2L) o.S.o_version;
      Lwt.return ()
  | n -> Alcotest.failf "expected one delete effect row, got %d" (List.length n))
  >>= fun () ->
  (* ADMINS EXEMPT: no grant in the run's map, the live is_admin check
     dispatches (operator pin) *)
  Tp.dispatch ~pool:p ~actor:me.S.i_id ~name:"tree/put"
    ~args:(P.tree_of_list [ Tuna.Cstr.encode "td/admin/x"; Tuna.Canon.parse "0" ])
  >>= fun _ ->
  run_del p ~caller:me.S.i_id ~grant_ids:[] ~art "td/admin/x"
  >>= fun (row_adm, js_adm) ->
  Alcotest.(check string) "admin-exempt delete run normal" "normal"
    (S.Run_status.to_string row_adm.S.r_status);
  Alcotest.(check (option string)) "admin delete journaled no error" None
    (journal_error js_adm);
  (match js_adm with
   | [ j ] ->
       Alcotest.(check (option string)) "admin delete carries no grant id" None
         j.S.j_grant_id
   | _ -> Alcotest.fail "bad journal shape");
  S.path_get p ~path:"td/admin/x"
  >>= fun gone ->
  Alcotest.(check bool) "admin-deleted path removed" true (Option.is_none gone);
  (* DENIED without a covering grant: journaled error answer + NULL-effect
     delete row; the run keeps going *)
  peer p "m11-bystander" >>= fun bystander ->
  run_del p ~caller:bystander.S.i_id ~grant_ids:[] ~art "td/b/two"
  >>= fun (row_denied, js_denied) ->
  Alcotest.(check string) "denied run still normal" "normal"
    (S.Run_status.to_string row_denied.S.r_status);
  (match journal_error js_denied with
   | Some e ->
       Alcotest.(check string) "denial names the missing grant"
         "grant denial: no live grant for prim tree/del" e
   | None -> Alcotest.fail "expected a journaled denial");
  ops_with p ~op:"delete" ~path:"td/b/two"
  >>= (function
  | [ o ] ->
      Alcotest.(check (option string)) "denial row has no value" None
        o.S.o_value_hash;
      Alcotest.(check (option int64)) "denial row has no version" None
        o.S.o_version;
      Lwt.return ()
  | n -> Alcotest.failf "expected one delete denial row, got %d" (List.length n))
  >>= fun () ->
  (* ABSENT denial journaled: covering grant, path does not exist *)
  run_del p ~caller:del.S.i_id ~grant_ids:[ g.S.g_id ] ~art "td/a/never-was"
  >>= fun (row_absent, js_absent) ->
  Alcotest.(check string) "absent-delete run still normal" "normal"
    (S.Run_status.to_string row_absent.S.r_status);
  (match journal_error js_absent with
   | Some e ->
       Alcotest.(check string)
         "absent denial is an error answer, not an exception"
         "tree/del: no value at path td/a/never-was" e
   | None -> Alcotest.fail "expected a journaled absent denial");
  ops_with p ~op:"delete" ~path:"td/a/never-was"
  >>= function
  | [ o ] ->
      Alcotest.(check (option int64)) "absent denial row has no version" None
        o.S.o_version;
      Lwt.return ()
  | n -> Alcotest.failf "expected one absent denial row, got %d" (List.length n)

(* rewind fold honors deletes: delete then rewind-to-before = path back *)
let test_tree_del_rewind () =
  setup () >>= fun p ->
  admin p >>= fun me ->
  let vh = Tuna.Hash.hex_of_string "0" in
  S.value_put p ~hash:vh ~ternary:"0" >>= fun () ->
  let put path =
    Tp.dispatch ~pool:p ~actor:me.S.i_id ~name:"tree/put"
      ~args:(P.tree_of_list [ Tuna.Cstr.encode path; Tuna.Canon.parse "0" ])
    >>= fun _ -> Lwt.return ()
  in
  let del path =
    Tp.dispatch ~pool:p ~actor:me.S.i_id ~name:"tree/del"
      ~args:(P.tree_of_list [ Tuna.Cstr.encode path ])
    >>= fun _ -> Lwt.return ()
  in
  put "td/r/x" >>= fun () ->
  S.ops_fold p ~prefix:"td/r" ~from_seq:0L () >>= fun ops ->
  let seq_of op_name path =
    match
      List.find_opt
        (fun (o : S.tree_op) -> o.S.o_op = op_name && o.S.o_path = path)
        ops
    with
    | Some o -> o.S.o_seq
    | None -> Alcotest.failf "no %s row for %s" op_name path
  in
  let after_put = seq_of "put" "td/r/x" in
  del "td/r/x" >>= fun () ->
  S.path_list p ~prefix:"td/r" () >>= fun live ->
  Alcotest.(check int) "live index has no paths after delete" 0
    (List.length live);
  S.ops_fold p ~prefix:"td/r" ~from_seq:0L () >>= fun ops2 ->
  let head =
    List.fold_left (fun a (o : S.tree_op) -> Int64.max a o.S.o_seq) 0L ops2
  in
  Rw.state p ~prefix:"td/r" ~at_seq:head >>= fun rew_head ->
  Alcotest.(check int) "fold at head reproduces the delete" 0
    (List.length rew_head);
  Rw.state p ~prefix:"td/r" ~at_seq:after_put >>= fun rew_prior ->
  (match rew_prior with
   | [ e ] ->
       Alcotest.(check string) "rewind-to-before restores the path" "td/r/x"
         e.Rw.path;
       Alcotest.(check string) "restored value hash" vh e.Rw.value_hash;
       Alcotest.(check int64) "restored version" 1L e.Rw.version;
       Lwt.return ()
   | n ->
       Alcotest.failf "expected exactly the restored path, got %d"
         (List.length n))

let () =
  match Sys.getenv_opt "TUNA_TEST_PG" with
  | None -> print_endline "m11 tests skipped (TUNA_TEST_PG not set)"
  | Some _ ->
    let lwt _name f = Alcotest_lwt.test_case _name `Quick (fun _sw () -> f ()) in
    Lwt_main.run
      (Alcotest_lwt.run "m11"
         [ ( "byte-values"
           , [ lwt "round-trip + address law + kind disjoint" test_byte_roundtrip
             ; lwt "dedup + bloat inversion" test_byte_dedup_and_bloat
             ; lwt "cap denial + absent read journaled"
                 test_value_denials_journaled ] )
         ; ( "routes"
           , [ lwt "template route serves anonymous" test_template_route
             ; lwt "reserved shadow denied + static wins" test_reserved_shadow
             ; lwt "program route journaled + law-4 finish" test_program_route
             ; lwt "capability checks + delete lifecycle"
                 test_route_capability_and_lifecycle
             ; lwt "rewind restores the prior route table" test_route_rewind ] )
         ; ( "tree-del"
           , [ lwt "boundary allow/deny/absent + admin exempt"
                 test_tree_del_boundary
             ; lwt "rewind fold honors deletes" test_tree_del_rewind ] ) ])
