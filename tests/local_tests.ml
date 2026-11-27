(* local chapter L1 (borg/local.borg): the frozen stdlib reaches the
   offline front ends.  Tuna_stdlib.Std_dict must be the manifest
   (names, hashes, sizes), must equal what the server's seeding round
   builds (Tuna_server.Stdlib_embed.defs compiled in order, each def
   against the defs before it - the sabralib identity's own dictionary),
   and must resolve names as the server does for an identity with no
   defs: lambda param > std > reader builtins.  L2: the local store's
   op chain is the server's formula, and local REPL rounds resolve and
   shadow as the server's.  L3: a local run with prims seals a record
   deriv-check verifies (every semantics), the server's journal-fed
   replay reproduces it, and a flipped answer fails.  No PG, no HTTP. *)

open Tuna
module Bracket = Tuna_compiler.Bracket
module Std = Tuna_stdlib.Std_dict

let read_lines path =
  let ic = open_in path in
  let rec go acc =
    match input_line ic with
    | l -> go (if String.trim l = "" then acc else l :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  go []

let dict () = Std.dictionary ()

let compile src = Bracket.compile_source ~dictionary:(dict ()) src

(* manifest rows: "<name> <size> <sha256>" *)
let test_manifest () =
  let manifest =
    read_lines "../stdlib/v1/manifest"
    |> List.map (fun l ->
           match String.split_on_char ' ' l with
           | [ n; size; h ] -> (n, int_of_string size, h)
           | _ -> Alcotest.failf "bad manifest row %S" l)
  in
  Alcotest.(check int) "row count" (List.length manifest) (List.length Std.rows);
  List.iter2
    (fun (mn, msize, mh) (n, h, ternary) ->
      Alcotest.(check string) "name in manifest order" mn n;
      Alcotest.(check string) (n ^ " hash = manifest") mh h;
      let t = List.assoc n (dict ()) in
      Alcotest.(check string) (n ^ " ternary hashes to its row") h (Hash.hex_of_tree t);
      Alcotest.(check string) (n ^ " ternary round-trips") ternary (Canon.encode t);
      Alcotest.(check int) (n ^ " size = manifest") msize (Tree.size t))
    manifest Std.rows

(* what the server seeds: every def round compiled against the
   sabralib dictionary built so far (REPL def; later defs shadow) *)
let test_equals_seeded () =
  let seeded =
    List.fold_left
      (fun acc (name, src) ->
        let art = Bracket.compile_source ~dictionary:acc src in
        (name, art.Bracket.tree) :: List.remove_assoc name acc)
      [] Tuna_server.Stdlib_embed.defs
  in
  Alcotest.(check int) "same names" (List.length seeded) (List.length (dict ()));
  List.iter
    (fun (n, t) ->
      match List.assoc_opt n seeded with
      | None -> Alcotest.failf "%s frozen but not seeded" n
      | Some s ->
          Alcotest.(check string) (n ^ " frozen = seeded") (Hash.hex_of_tree s)
            (Hash.hex_of_tree t))
    (dict ())

let test_resolution () =
  (* known-good: not true -> leaf in 2 steps, at compile time *)
  let a = compile "(not true)" in
  Alcotest.(check string) "(not true) = leaf" "0" a.Bracket.ternary;
  Alcotest.(check int) "(not true) 2 steps" 2 a.Bracket.steps;
  let b = compile "(list-length [1 2 3])" in
  Alcotest.(check string) "(list-length [1 2 3]) = 3" "1110" b.Bracket.ternary;
  (* a lambda parameter shadows std *)
  let shadow = compile "(lambda (not) (not 0))" in
  let hand = Bracket.compile_source "(lambda (f) (f 0))" in
  Alcotest.(check string) "param shadows std" hand.Bracket.hash_hex shadow.Bracket.hash_hex;
  (* a source with no free names compiles identically with or without std *)
  let plain = "(lambda (x) (pair x [1 2]))" in
  Alcotest.(check string) "free-name-free source unchanged"
    (Bracket.compile_source plain).Bracket.hash_hex (compile plain).Bracket.hash_hex;
  (* reader builtins stay reachable: std defines neither pair nor cons *)
  List.iter
    (fun n ->
      Alcotest.(check bool) (n ^ " not shadowed by std") false
        (List.mem_assoc n (dict ())))
    [ "pair"; "cons" ];
  (* pins (tool-language T2) hold against the frozen hash *)
  let h = List.assoc "list-fold" (List.map (fun (n, h, _) -> (n, h)) Std.rows) in
  let pinned = compile ("list-fold@" ^ String.sub h 0 8) in
  Alcotest.(check string) "pin holds" h pinned.Bracket.hash_hex;
  (match compile "list-fold@00000000" with
   | _ -> Alcotest.fail "moved pin must not compile"
   | exception Tuna_compiler.Ir.Error _ -> ());
  match compile "nosuchname" with
  | _ -> Alcotest.fail "unbound name must not compile"
  | exception Tuna_compiler.Ir.Error _ -> ()

(* -- L2: the local store and REPL -------------------------------------- *)

module LS = Tuna_local.Local_store
module LR = Tuna_local.Local_repl

let fresh_dir () =
  let d = Filename.temp_file "tuna-local" "" in
  Sys.remove d;
  d

let opts =
  { LR.fuel = 1_000_000; size_cap = 1_000_000; semantics = "v0";
    grants = []; allow_hosts = []; inputs = [] }

let run st line =
  match LR.execute st opts line with
  | Ok o -> o
  | Error e -> Alcotest.failf "%s: %s" line e

(* the local chain formula is the store's, field for field *)
let test_op_concat () =
  List.iter
    (fun (seq, path, op, vh, pv, v, actor, ts) ->
      Alcotest.(check string) ("op_concat " ^ path)
        (Tuna_store.Store.op_concat ~seq ~path ~op ~value_hash:vh
           ~prev_version:pv ~version:v ~actor ~ts_unix:ts)
        (LS.op_concat ~seq ~path ~op ~value_hash:vh ~prev_version:pv
           ~version:v ~actor ~ts_unix:ts))
    [ (1L, "dict/local/inc", "put", Some LS.undef_marker, None, Some 1L, "local", 0L);
      (42L, "runs/x", "cas", None, Some 3L, None, "a|b", 1795671740L);
      (7L, "", "put", Some "ab", Some 1L, Some 2L, "", -1L) ]

let test_store_chain () =
  let st = LS.open_ (fresh_dir ()) in
  Alcotest.(check string) "default identity" "local" st.LS.identity;
  ignore (run st "def inc (lambda (x) (int-add x 1))");
  ignore (run st "def not (lambda (b) b)");
  ignore (run st "undef not");
  Alcotest.(check (result unit string)) "chain verifies" (Ok ()) (LS.verify st);
  let o = LS.ops st in
  Alcotest.(check int) "three effect ops" 3 (List.length o);
  Alcotest.(check string) "genesis predecessor" LS.genesis (List.hd o).LS.prev_hash;
  let last = List.nth o 2 in
  Alcotest.(check (option string)) "undef is the marker put"
    (Some LS.undef_marker) last.LS.value_hash;
  Alcotest.(check bool) "undef carries the prev binding" true
    (last.LS.prev_value_hash <> None);
  ignore (run st "def not (lambda (b) (not b))");
  let redef = List.nth (LS.ops st) 3 in
  Alcotest.(check (option string)) "a def after undef names the marker"
    (Some LS.undef_marker) redef.LS.prev_value_hash;
  Alcotest.(check (option int64)) "per-path version" (Some 2L) last.LS.version;
  Alcotest.(check (list string)) "dict is the fold" [ "inc"; "not" ]
    (List.map fst (LS.dict_hashes st));
  (* reopen: same identity, same state; a second identity is refused *)
  let st2 = LS.open_ st.LS.root in
  Alcotest.(check (list string)) "persists" [ "inc"; "not" ] (List.map fst (LS.dict_hashes st2));
  (match LS.open_ ~identity:"other" st.LS.root with
   | _ -> Alcotest.fail "a directory holds one identity"
   | exception Failure _ -> ());
  (* tamper: rewrite one value hash in the log *)
  let p = Filename.concat st.LS.root "ops.jsonl" in
  let s = In_channel.with_open_bin p In_channel.input_all in
  let s' = Str.global_replace (Str.regexp_string LS.undef_marker) (String.make 64 'f') s in
  Out_channel.with_open_bin p (fun oc -> Out_channel.output_string oc s');
  match LS.verify st with
  | Ok () -> Alcotest.fail "tampered log must not verify"
  | Error _ -> ()

(* L6 pushes ops.jsonl as a federation window: every row must parse with
   the server's own window parser and verify with the store's chain walk *)
let test_window_rows () =
  let st = LS.open_ (fresh_dir ()) in
  ignore (run st "def inc (lambda (x) (int-add x 1))");
  ignore (run st "undef inc");
  let lines =
    In_channel.with_open_bin (Filename.concat st.LS.root "ops.jsonl")
      In_channel.input_all
    |> String.split_on_char '\n' |> List.filter (( <> ) "")
  in
  let rows =
    List.map
      (fun l ->
        match Tuna_server.Api.parse_tree_op_json (Yojson.Basic.from_string l) with
        | Ok (o, prev) -> { Tuna_store.Store.w_op = o; w_prev_hash = prev }
        | Error e -> Alcotest.failf "window parser refused %s: %s" l e)
      lines
  in
  Alcotest.(check int) "two rows" 2 (List.length rows);
  match Tuna_store.Store.verify_window_rows rows with
  | `Ok -> ()
  | `Bad e -> Alcotest.failf "store chain walk refused the local log: %s" e

(* a crash mid-append leaves a fragment with no newline: ignored on
   read, dropped by the next append, chain intact *)
let test_torn_line () =
  let st = LS.open_ (fresh_dir ()) in
  ignore (run st "def a 1");
  let p = Filename.concat st.LS.root "ops.jsonl" in
  Out_channel.with_open_gen [ Open_append; Open_binary ] 0o600 p (fun oc ->
      Out_channel.output_string oc "{\"seq\":");
  Alcotest.(check (list string)) "fragment ignored" [ "a" ] (List.map fst (LS.dict_hashes st));
  ignore (run st "def b 2");
  Alcotest.(check (result unit string)) "chain intact" (Ok ()) (LS.verify st);
  Alcotest.(check int) "two committed rows" 2 (List.length (LS.ops st));
  (* a corrupt COMMITTED line is an error value, not a crash *)
  Out_channel.with_open_gen [ Open_append; Open_binary ] 0o600 p (fun oc ->
      Out_channel.output_string oc "not json\n");
  match LR.execute st opts "dict" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "a corrupt log must answer an error"

let test_repl_rounds () =
  let st = LS.open_ (fresh_dir ()) in
  let std_not = (run st "(not true)").LR.hash in
  Alcotest.(check string) "std not: (not true) = false" (Hash.hex_of_tree Tree.Leaf) std_not;
  ignore (run st "def not (lambda (b) b)");
  Alcotest.(check string) "own def shadows std"
    (Hash.hex_of_tree (Tree.Stem Tree.Leaf)) (run st "(not true)").LR.hash;
  ignore (run st "undef not");
  Alcotest.(check string) "undef restores std" std_not (run st "(not true)").LR.hash;
  let o = run st "(list-length [1 2 3])" in
  Alcotest.(check (option string)) "status" (Some "normal") o.LR.status;
  Alcotest.(check string) "list-length 3" "1110" o.LR.ternary;
  Alcotest.(check string) "get addresses the last result" "110"
    (run st "get 0").LR.ternary;
  (* a round with no result becomes the last round, as on the server *)
  let starved = { opts with LR.fuel = 5; inputs = [ (compile "[1 2 3]").Bracket.tree ] } in
  (match LR.execute st starved "(lambda (xs) (list-length xs))" with
   | Ok o -> Alcotest.(check (option string)) "starved" (Some "fuel_exhausted") o.LR.status
   | Error e -> Alcotest.failf "starved round: %s" e);
  (match LR.execute st opts "get 0" with
   | Error e -> Alcotest.(check string) "get after no result" "last round produced no result tree" e
   | Ok _ -> Alcotest.fail "get must not reach past a resultless round");
  (* a hash argument is never a file path *)
  (match LR.execute st opts "first-diff ../identity ../identity" with
   | Error e -> Alcotest.(check string) "path-like hash" "unknown program hash ../identity" e
   | Ok _ -> Alcotest.fail "a non-hash must not resolve");
  (match LR.execute st opts "eval nosuchname" with
   | Error e -> Alcotest.(check bool) "compile error text" true
                  (String.length e > 14 && String.sub e 0 14 = "compile error:")
   | Ok _ -> Alcotest.fail "unbound must fail");
  match LR.execute st opts "bogus command" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown command must fail"

(* the dictionary-visibility command in the offline CLI: self-only, and
   the setting is a journaled dict-visibility row at vis/<identity> *)
let test_repl_dict_visibility () =
  let st = LS.open_ (fresh_dir ()) in
  let o = run st "dict-visibility public" in
  Alcotest.(check string) "kind" "dict-visibility" o.LR.kind;
  let rows =
    List.filter (fun (r : LS.op) -> r.LS.op = "dict-visibility") (LS.ops st)
  in
  (match rows with
   | [ r ] -> Alcotest.(check string) "path" "vis/local/public" r.LS.path
   | _ -> Alcotest.fail "expected exactly one dict-visibility row");
  match LR.execute st opts "dict-visibility other public" with
  | Error e ->
      Alcotest.(check bool) "a non-self target is refused" true
        (String.length e > 0)
  | Ok _ -> Alcotest.fail "the offline CLI must refuse a non-self target"

(* -- L3: the local run host ---------------------------------------------- *)

module LRun = Tuna_local.Local_run
module D = Tuna_deriv.Deriv

let cfg ?(grants = [ "*" ]) ?(semantics = "v0") () =
  { LRun.grants; allow_hosts = []; semantics; fuel = 10_000_000; size_cap = 10_000_000 }

let prog_src =
  {|(lambda (x) [(runtime (prim "echo" x)) (runtime (prim "now"))
     (runtime (prim "store/put" x 42)) (runtime (prim "store/get" x))
     (runtime (prim "tree/get" x))])|}

let run_src st c src args =
  let art = compile src in
  LRun.execute st c ~tags:art.Bracket.tags ~program:art.Bracket.tree
    (List.map (fun a -> (compile a).Bracket.tree) args)

let check_verified what d =
  match D.verify d with
  | D.Verified -> ()
  | D.Failed m -> Alcotest.failf "%s: deriv-check refused: %s" what m

(* the server's journal-fed replay (Tuna_server.Replay.execute_fed) over
   the local journal: same status, result hash and steps, every row
   consumed *)
let server_replay (d : D.t) =
  let rows =
    List.map
      (fun (j : D.Journal.t) ->
        { Tuna_store.Store.j_run_id = d.D.d_run_id; j_seq = j.D.Journal.j_seq;
          j_callsite_path = j.D.Journal.j_callsite_path; j_prim = j.D.Journal.j_prim;
          j_prim_contract = j.D.Journal.j_prim_contract; j_grant_id = j.D.Journal.j_grant_id;
          j_args_ternary = j.D.Journal.j_args_ternary; j_args_hash = j.D.Journal.j_args_hash;
          j_result_ternary = j.D.Journal.j_result_ternary;
          j_result_hash = j.D.Journal.j_result_hash; j_error = j.D.Journal.j_error;
          j_wall_ms = j.D.Journal.j_wall_ms; j_host_build = j.D.Journal.j_host_build;
          j_prev_hash = j.D.Journal.j_prev_hash; j_row_hash = j.D.Journal.j_row_hash })
      d.D.d_journal
  in
  let parse t = match Canon.of_string t with Ok t -> t | Error _ -> Alcotest.fail "ternary" in
  let module R = Tuna_server.Replay in
  let accel = Tuna_interp.Accel.registry_of_semantics d.D.d_semantics in
  let mode = if d.D.d_semantics = "v1" then R.Eng.Sharing else R.Eng.Canonical in
  let outcome, consumed =
    R.execute_fed ~mode ?accel ~program:(parse d.D.d_program_ternary)
      ~inputs:(List.map parse d.D.d_input_ternaries) ~fuel:d.D.d_fuel
      ~size_cap:d.D.d_size_cap rows
  in
  Alcotest.(check int) "server replay consumed every row" (List.length rows) consumed;
  Alcotest.(check string) "server replay status" (D.status_to_string d.D.d_status)
    (R.outcome_status outcome);
  Alcotest.(check (option string)) "server replay result" d.D.d_result_hash (R.outcome_hash outcome);
  Alcotest.(check int) "server replay steps" d.D.d_step_count (R.outcome_steps outcome)

let test_run_record () =
  let st = LS.open_ (fresh_dir ()) in
  let r = run_src st (cfg ()) prog_src [ {|"k"|} ] in
  let d = r.LRun.record in
  Alcotest.(check (list string)) "journal prims"
    [ "echo"; "now"; "store/put"; "store/get"; "tree/get" ]
    (List.map (fun j -> j.D.Journal.j_prim) d.D.d_journal);
  let j n = List.nth d.D.d_journal n in
  Alcotest.(check (option string)) "store/get answers the put"
    (Some (Hash.hex_of_tree (compile "42").Bracket.tree)) (j 3).D.Journal.j_result_hash;
  Alcotest.(check (option string)) "substrate prim is not local"
    (Some "prim tree/get: not available in the local host") (j 4).D.Journal.j_error;
  Alcotest.(check bool) "callsite paths from provenance" true
    (List.for_all (fun j -> j.D.Journal.j_callsite_path <> "") d.D.d_journal);
  check_verified "local v0 run" d;
  server_replay d;
  (* sealed: bytes by hash, cited from the op log, the log still a window *)
  Alcotest.(check (option string)) "record bytes stored by hash"
    (Some (D.to_string d)) (LS.get_bytes st r.LRun.record_hash);
  Alcotest.(check (option string)) "runs/<id> cites the record"
    (Some r.LRun.record_hash) (LS.last_value st ("runs/" ^ d.D.d_run_id));
  Alcotest.(check (result unit string)) "op chain" (Ok ()) (LS.verify st);
  (* tamper: flip one journal answer (rehashing nothing) *)
  let flipped =
    { d with D.d_journal =
        List.mapi (fun i (j : D.Journal.t) ->
            if i = 1 then { j with D.Journal.j_result_ternary = Some "0";
                                   j_result_hash = Some (Hash.hex_of_string "0") }
            else j) d.D.d_journal }
  in
  match D.verify flipped with
  | D.Failed _ -> ()
  | D.Verified -> Alcotest.fail "a flipped journal answer must not verify"

let test_run_denial () =
  let st = LS.open_ (fresh_dir ()) in
  let d = (run_src st (cfg ~grants:[ "echo" ] ()) prog_src [ {|"k"|} ]).LRun.record in
  let errs = List.map (fun j -> j.D.Journal.j_error) d.D.d_journal in
  Alcotest.(check (list (option string))) "grants are arguments, denial is data"
    [ None; Some "grant denial: no live grant for prim now";
      Some "grant denial: no live grant for prim store/put";
      Some "grant denial: no live grant for prim store/get";
      Some "grant denial: no live grant for prim tree/get" ] errs;
  Alcotest.(check (option string)) "a named grant is cited" (Some "local:echo")
    (List.hd d.D.d_journal).D.Journal.j_grant_id;
  check_verified "denied run" d;
  server_replay d

(* L3 in the repl: eval and def rounds answer prims under the session's
   grants; a denial is the server's error ANSWER, never an exception, and
   a round seals no record (L2's op-log shape is unchanged) *)
let test_repl_prims () =
  let st = LS.open_ (fresh_dir ()) in
  let inputs = [ (compile "42").Bracket.tree ] in
  let key s = (compile (Printf.sprintf "%S" s)).Bracket.tree in
  let run_with grants line =
    match LR.execute st { opts with LR.grants = grants; inputs } line with
    | Ok o -> o
    | Error e -> Alcotest.failf "%s: %s" line e
  in
  let first = run_with [ "store/put" ] {|(lambda (x) (runtime (prim "store/put" "k" x)))|} in
  Alcotest.(check (option string)) "granted prim answers" (Some "normal")
    first.LR.status;
  (match LS.kv_get st (key "k") with
   | Some t ->
       Alcotest.(check string) "the prim really ran"
         (Hash.hex_of_tree (compile "42").Bracket.tree) (Hash.hex_of_tree t)
   | None -> Alcotest.fail "granted store/put wrote nothing");
  let denied =
    run_with [] {|(lambda (x) (runtime (prim "store/put" "k2" x)))|}
  in
  Alcotest.(check (option string)) "denial is a finished round"
    (Some "normal") denied.LR.status;
  Alcotest.(check bool) "a denied prim has no effect" true
    (LS.kv_get st (key "k2") = None);
  let msg = "grant denial: no live grant for prim store/put" in
  let encoded = Tuna.Canon.encode (Tuna.Cstr.encode msg) in
  let has_text =
    try
      ignore (Str.search_forward (Str.regexp_string encoded) denied.LR.ternary 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "the denial text is the answer" true has_text;
  Alcotest.(check int) "rounds seal no record" 0 (List.length (LS.ops st));
  (* a def round stores a closure; the prim fires when a later eval round
     calls it, and the def adds exactly one dictionary row *)
  ignore
    (run_with [ "store/put" ]
       {|def putk (lambda (x) (runtime (prim "store/put" "kd" x)))|});
  let called = run_with [ "store/put" ] "eval putk" in
  Alcotest.(check (option string)) "a defined prim answers when called"
    (Some "normal") called.LR.status;
  (match LS.kv_get st (key "kd") with
   | Some t ->
       Alcotest.(check string) "the defined prim ran"
         (Hash.hex_of_tree (compile "42").Bracket.tree) (Hash.hex_of_tree t)
   | None -> Alcotest.fail "the defined prim wrote nothing");
  Alcotest.(check int) "one dictionary row, no run row" 1
    (List.length (LS.ops st))

(* the registry step: fast1-fast3 records verify offline, and the
   accelerator really fired (fewer steps than v0) *)
let test_run_semantics () =
  let st = LS.open_ (fresh_dir ()) in
  let src = "(lambda (xs) (list-length xs))" in
  let v0 = (run_src st (cfg ()) src [ "[1 2 3 4 5 6 7 8]" ]).LRun.record in
  check_verified "v0" v0;
  List.iter
    (fun sem ->
      let d = (run_src st (cfg ~semantics:sem ()) src [ "[1 2 3 4 5 6 7 8]" ]).LRun.record in
      Alcotest.(check string) (sem ^ " semantics recorded") sem d.D.d_semantics;
      Alcotest.(check (option string)) (sem ^ " same result as v0") v0.D.d_result_hash
        d.D.d_result_hash;
      Alcotest.(check bool) (sem ^ " accelerated") true (d.D.d_step_count < v0.D.d_step_count);
      check_verified sem d;
      server_replay d)
    [ "v1"; "fast1"; "fast2"; "fast3" ]
  |> ignore;
  (* a fast* run that answers a prim: the journal and the accelerator
     must both survive the registry path *)
  List.iter
    (fun sem ->
      let d =
        (run_src st (cfg ~semantics:sem ())
           {|(lambda (x) (runtime (prim "echo" x)))|} [ {|"k"|} ])
          .LRun.record
      in
      Alcotest.(check (list string)) (sem ^ " journals the prim") [ "echo" ]
        (List.map (fun j -> j.D.Journal.j_prim) d.D.d_journal);
      check_verified (sem ^ " with a prim") d;
      server_replay d)
    [ "fast1"; "fast2"; "fast3" ];
  match LRun.execute st (cfg ~semantics:"fast9" ()) ~program:Tree.Leaf [] with
  | _ -> Alcotest.fail "unknown semantics must be refused"
  | exception Failure _ -> ()

let () =
  Alcotest.run "local"
    [ ( "l1-stdlib-offline",
        [ Alcotest.test_case "frozen rows = manifest" `Quick test_manifest;
          Alcotest.test_case "frozen = server seeding round" `Quick test_equals_seeded;
          Alcotest.test_case "resolution order and pins" `Quick test_resolution ] );
      ( "l2-local-repl",
        [ Alcotest.test_case "op chain formula = Store.op_concat" `Quick test_op_concat;
          Alcotest.test_case "store: chain, fold, identity, tamper" `Quick test_store_chain;
          Alcotest.test_case "log rows = federation window rows" `Quick test_window_rows;
          Alcotest.test_case "torn final line, corrupt line" `Quick test_torn_line;
          Alcotest.test_case "rounds: std, shadow, undef, get, errors" `Quick test_repl_rounds;
          Alcotest.test_case "dict-visibility is self-only and journaled" `Quick test_repl_dict_visibility ] );
      ( "l3-local-prims",
        [ Alcotest.test_case "record verifies, server replay agrees, tamper fails" `Quick test_run_record;
          Alcotest.test_case "grants are arguments" `Quick test_run_denial;
          Alcotest.test_case "repl rounds answer prims and seal no record" `Quick test_repl_prims;
          Alcotest.test_case "v1 and fast1-fast3 records verify offline" `Quick test_run_semantics ] ) ]
