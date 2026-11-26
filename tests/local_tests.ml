(* local chapter L1 (borg/local.borg): the frozen stdlib reaches the
   offline front ends.  Tuna_stdlib.Std_dict must be the manifest
   (names, hashes, sizes), must equal what the server's seeding round
   builds (Tuna_server.Stdlib_embed.defs compiled in order, each def
   against the defs before it - the sabralib identity's own dictionary),
   and must resolve names as the server does for an identity with no
   defs: lambda param > std > reader builtins.  L2: the local store's
   op chain is the server's formula, and local REPL rounds resolve and
   shadow as the server's.  No PG, no HTTP. *)

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
  { LR.fuel = 1_000_000; size_cap = 1_000_000;
    mode = Tuna_interp.Eval.Canonical; accel = None; inputs = [] }

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
          Alcotest.test_case "rounds: std, shadow, undef, get, errors" `Quick test_repl_rounds ] ) ]
