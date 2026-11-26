(* local chapter L1 (borg/local.borg): the frozen stdlib reaches the
   offline front ends.  Tuna_stdlib.Std_dict must be the manifest
   (names, hashes, sizes), must equal what the server's seeding round
   builds (Tuna_server.Stdlib_embed.defs compiled in order, each def
   against the defs before it - the sabralib identity's own dictionary),
   and must resolve names as the server does for an identity with no
   defs: lambda param > std > reader builtins.  No PG, no HTTP. *)

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

let () =
  Alcotest.run "local"
    [ ( "l1-stdlib-offline",
        [ Alcotest.test_case "frozen rows = manifest" `Quick test_manifest;
          Alcotest.test_case "frozen = server seeding round" `Quick test_equals_seeded;
          Alcotest.test_case "resolution order and pins" `Quick test_resolution ] ) ]
