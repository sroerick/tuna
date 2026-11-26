(* tool-language T2 (borg/tool-language.borg): the gate-receipt verdict
   written against the shared stdlib with pinned names
   (scripts/dialect/verdict-std.sabra) answers exactly as the original
   with its helpers inline (scripts/gate-receipts/verdict.sabra) on every
   policy case of the gate-receipts chapter.

   The two compile to DIFFERENT trees: let-bound helpers inside the
   program and the same helpers read as closed dictionary literals are
   different programs for the same function (tree calculus is
   intensional; a program hash names the tree, not the function).  So
   the acceptance here is extensional - same answer on every case, no
   prim call - and the hash difference is pinned too, so nobody later
   reads the std form as a drop-in for a receipt's program hash.

   The dictionary is the seeded stdlib itself: stdlib/v1/core.defs
   compiled in file order, cross-references inlined, exactly as
   tools/stdprobe/freeze.ml and the seeding round do.  No PG, no HTTP. *)

open Tuna
module T = Tree
module Bracket = Tuna_compiler.Bracket

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* "def <name> <source>" records, blank-line separated, # comments *)
let stdlib_dictionary =
  lazy
    (read_all "../stdlib/v1/core.defs"
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           match String.index_opt line ' ' with
           | Some 3 when String.sub line 0 4 = "def " ->
               let rest = String.sub line 4 (String.length line - 4) in
               let sp = String.index rest ' ' in
               Some
                 ( String.sub rest 0 sp,
                   String.sub rest (sp + 1) (String.length rest - sp - 1) )
           | _ -> None)
    |> List.fold_left
         (fun dict (name, src) ->
           let art = Bracket.compile_source ~dictionary:dict src in
           (name, art.Bracket.tree) :: dict)
         [])

let original =
  lazy (Bracket.compile_source (read_all "../scripts/gate-receipts/verdict.sabra"))

let std =
  lazy
    (Bracket.compile_source ~dictionary:(Lazy.force stdlib_dictionary)
       (read_all "../scripts/dialect/verdict-std.sabra"))

(* -- observation records (the gate-receipts chapter's encoding) -- *)

let tag n =
  let rec go = function 0 -> T.Leaf | k -> T.Stem (go (k - 1)) in
  go n

let int_lit n = Int_enc.positive_of_decimal (string_of_int n)
let str s = Cstr.encode s
let pair a b = T.Fork (a, b)
let rec lst = function [] -> T.Leaf | x :: xs -> pair x (lst xs)
let entry k v = pair (tag k) (pair v T.Leaf)

let observation ?(gate = "tuna/runtest") ?(tree_clean = 1) ?(diff_hash = "")
    ?(exit = 0) counts =
  let counts =
    match counts with
    | None -> tag 1
    | Some (r, p, f, s) -> lst [ int_lit r; int_lit p; int_lit f; int_lit s ]
  in
  lst
    [ entry 1 (str gate); entry 2 (str "tuna"); entry 3 (str "make -C tuna test");
      entry 4 (str (String.make 40 'a')); entry 5 (int_lit tree_clean);
      entry 6 (str diff_hash); entry 7 (int_lit exit);
      entry 8 (str (String.make 64 'b')); entry 9 counts;
      entry 10 (str "alcotest-v1"); entry 11 (str "runner");
      entry 12 (str "2026-11-24T00:00:00Z") ]

let run program obs skip =
  let host ~site:_ ~name ~args:_ = `Error ("verdict called prim " ^ name) in
  match
    Tuna_interp.Eval.eval ~host ~fuel:50_000_000 ~size_cap:1_000_000 ~program
      [ obs; lst (List.map str skip) ]
  with
  | Tuna_interp.Eval.Normal (t, _) -> Canon.encode t
  | _ -> Alcotest.fail "verdict did not reach a normal form"

let pass = "10" and fail = "110" and unverified = "1110"

(* name, observation, skip-allowed, the chapter's answer *)
let cases =
  [ ("exit 0, counts [0 0 0 0]", observation (Some (0, 0, 0, 0)), [], unverified);
    ( "pp SKIP shape",
      observation ~gate:"pp/data-spaces" (Some (15, 15, 0, 15)), [], unverified );
    ( "pp SKIP shape, skip-allowed",
      observation ~gate:"pp/data-spaces" (Some (15, 15, 0, 15)),
      [ "pp/data-spaces" ], pass );
    ("exit 0, counts [93 93 0 0]", observation (Some (93, 93, 0, 0)), [], pass);
    ("exit 1", observation ~exit:1 (Some (93, 93, 0, 0)), [], fail);
    ("unparsed counts", observation None, [], unverified);
    ("exit outranks unparsed", observation ~exit:1 None, [], fail);
    ("failed nonzero", observation (Some (5, 4, 1, 0)), [], fail);
    ( "skip-allowed cannot excuse a failure",
      observation (Some (5, 4, 1, 1)), [ "tuna/runtest" ], fail );
    ( "dirty tree, no diff",
      observation ~tree_clean:0 (Some (93, 93, 0, 0)), [], unverified );
    ( "dirty tree, diff present",
      observation ~tree_clean:0 ~diff_hash:"deadbeef" (Some (93, 93, 0, 0)), [],
      pass ) ]

let agreement =
  List.map
    (fun (name, obs, skip, want) ->
      Alcotest.test_case name `Quick (fun () ->
          let a = run (Lazy.force original).Bracket.tree obs skip in
          let b = run (Lazy.force std).Bracket.tree obs skip in
          Alcotest.(check string) "original answers the chapter" want a;
          Alcotest.(check string) "std form answers as the original" a b))
    cases

let identity =
  [ Alcotest.test_case "different trees: std is not a drop-in program hash" `Quick
      (fun () ->
        Alcotest.(check bool) "hashes differ" true
          ((Lazy.force original).Bracket.hash_hex
          <> (Lazy.force std).Bracket.hash_hex)) ]

(* -- T4: the JSON codec ------------------------------------------------ *)

module C = Tuna_server.Json_codec
module J = Yojson.Basic

let ty s = match C.ty_of_json (J.from_string s) with
  | Ok t -> t | Error m -> Alcotest.failf "type %s: %s" s m

let enc t v = match C.encode (ty t) (J.from_string v) with
  | Ok x -> x | Error m -> Alcotest.failf "encode %s: %s" v m

let roundtrip name t v =
  Alcotest.test_case ("round trip: " ^ name) `Quick (fun () ->
      match C.decode (ty t) (enc t v) with
      | Ok back ->
          Alcotest.(check string) "same JSON" (J.to_string (J.from_string v))
            (J.to_string back)
      | Error m -> Alcotest.failf "decode: %s" m)

(* the codec's tree must be the tree the reader's own literal builds *)
let same_as_reader name t v src =
  Alcotest.test_case ("matches the reader: " ^ name) `Quick (fun () ->
      Alcotest.(check string) "ternary"
        (Canon.encode (Bracket.compile_source src).Bracket.tree)
        (Canon.encode (enc t v)))

let refused name t v sub =
  Alcotest.test_case ("refused: " ^ name) `Quick (fun () ->
      let r =
        match C.ty_of_json (J.from_string t) with
        | Error m -> Error m
        | Ok ty -> C.encode ty (J.from_string v)
      in
      match r with
      | Ok _ -> Alcotest.fail "expected an error"
      | Error m ->
          Alcotest.(check bool) (Printf.sprintf "%S names %S" m sub) true
            (let n = String.length sub in
             let rec go i = i + n <= String.length m && (String.sub m i n = sub || go (i + 1)) in
             go 0))

let obs_type =
  {|{"record": {"gate": [1, "string"], "project": [2, "string"],
     "command": [3, "string"], "rev": [4, "string"], "tree_clean": [5, "int"],
     "diff_hash": [6, "string"], "exit": [7, "int"], "log_hash": [8, "string"],
     "counts": [9, {"list": "int"}], "adapter": [10, "string"],
     "runner": [11, "string"], "started_at": [12, "string"]}}|}

let verdict_type = {|{"enum": {"pass": 1, "fail": 2, "unverified": 3}}|}

let obs_json ?(exit = 0) counts =
  Printf.sprintf
    {|{"started_at": "2026-11-24T00:00:00Z", "gate": "tuna/runtest",
       "project": "tuna", "command": "make -C tuna test", "rev": "%s",
       "tree_clean": 1, "diff_hash": "", "exit": %d, "log_hash": "%s",
       "counts": %s, "adapter": "alcotest-v1", "runner": "runner"}|}
    (String.make 40 'a') exit (String.make 64 'b') counts

let codec =
  [ roundtrip "bool" {|"bool"|} "true";
    roundtrip "int zero" {|"int"|} "0";
    roundtrip "int 42" {|"int"|} "42";
    roundtrip "int -7" {|"int"|} "-7";
    roundtrip "int large" {|"int"|} "1152921504606846975";
    roundtrip "string with unicode" {|"string"|} {|"día ☃ / \\ x"|};
    roundtrip "tag" {|"tag"|} "9";
    roundtrip "tree" {|"tree"|} {|"21100"|};
    roundtrip "list" {|{"list": "int"}|} "[1, 2, 3]";
    roundtrip "option none" {|{"option": "int"}|} "null";
    roundtrip "option some" {|{"option": "int"}|} "5";
    roundtrip "record (decoded in tag order)"
      {|{"record": {"a": [1, "int"], "b": [2, {"list": "bool"}]}}|}
      {|{"a": 1, "b": [true, false]}|};
    roundtrip "enum" verdict_type {|"unverified"|};
    same_as_reader "int literal" {|"int"|} "42" "42";
    same_as_reader "negative int literal" {|"int"|} "-7" "-7";
    same_as_reader "tag literal" {|"tag"|} "5" "#5";
    same_as_reader "string literal" {|"string"|} {|"hi"|} {|"hi"|};
    same_as_reader "record = bracket pairs, sorted by tag"
      {|{"record": {"a": [1, "int"], "b": [2, "string"]}}|}
      {|{"b": "x", "a": 3}|} {|[[#1 3] [#2 "x"]]|};
    refused "unknown field" {|{"record": {"a": [1, "int"]}}|} {|{"a": 1, "z": 2}|}
      "$.z: unknown field";
    refused "float" {|"int"|} "1.5" "floats have no tuna encoding";
    refused "nested path" {|{"list": {"record": {"a": [1, "int"]}}}|}
      {|[{"a": 1}, {"a": "x"}]|} "$[1].a";
    refused "unknown enum name" verdict_type {|"maybe"|} "unknown enum name";
    refused "shared tag in a record type"
      {|{"record": {"a": [1, "int"], "b": [1, "int"]}}|} "{}" "share tag 1";
    refused "tag past the cap" {|"tag"|} "4097" "a tag is 0..4096";
    Alcotest.test_case "decode refuses a non-canonical int" `Quick (fun () ->
        match C.decode C.Int (T.Fork (T.Leaf, T.Fork (T.Leaf, T.Leaf))) with
        | Ok _ -> Alcotest.fail "a trailing false bit is not canonical"
        | Error _ -> ());
    Alcotest.test_case "JSON observation = the fixture's observation tree" `Quick
      (fun () ->
        Alcotest.(check string) "ternary"
          (Canon.encode (observation (Some (93, 93, 0, 0))))
          (Canon.encode (enc obs_type (obs_json "[93, 93, 0, 0]"))));
    Alcotest.test_case "verdict over JSON decodes to pass / fail / unverified" `Quick
      (fun () ->
        let verdict obs =
          let host ~site:_ ~name ~args:_ = `Error ("prim " ^ name) in
          match
            Tuna_interp.Eval.eval ~host ~fuel:50_000_000 ~size_cap:1_000_000
              ~program:(Lazy.force original).Bracket.tree
              [ enc obs_type obs; enc {|{"list": "string"}|} "[]" ]
          with
          | Tuna_interp.Eval.Normal (t, _) -> (
              match C.decode (ty verdict_type) t with
              | Ok (`String v) -> v
              | Ok _ | Error _ -> Alcotest.fail "verdict did not decode")
          | _ -> Alcotest.fail "verdict did not finish"
        in
        Alcotest.(check string) "pass" "pass" (verdict (obs_json "[93, 93, 0, 0]"));
        Alcotest.(check string) "fail" "fail" (verdict (obs_json ~exit:1 "[93, 93, 0, 0]"));
        Alcotest.(check string) "unverified" "unverified"
          (verdict (obs_json "[0, 0, 0, 0]"))) ]

(* typing TY3 (borg/typing.borg): the {"jay": T} codec encodes exactly
   the trees Jay's Rocq development (TyBin.v, booleans.v) exported, and
   decodes them back. *)
module JV = Tuna_server.Jay_values

let jay_vectors =
  lazy
    (let dir = "../reference/jay-typing-rules" in
     let read f =
       let ic = open_in_bin (Filename.concat dir f) in
       let rec go acc = match input_line ic with l -> go (l :: acc) | exception End_of_file -> close_in ic; List.rev acc in
       go []
     in
     let trees = Hashtbl.create 64 in
     List.iter
       (fun l ->
         let i = String.index l ' ' in
         Hashtbl.replace trees (String.sub l 0 i) (Tuna.Canon.parse (String.sub l (i + 1) (String.length l - i - 1))))
       (read "trees");
     fun label ->
       match List.find_map (fun l -> match String.split_on_char ' ' l with
           | [ _; l'; p; a; _ ] when l' = label -> Some (p, a) | _ -> None) (read "vectors") with
       | Some (p, a) -> (Hashtbl.find trees p, Hashtbl.find trees (List.hd (String.split_on_char ',' a)))
       | None -> Alcotest.fail ("no vector " ^ label))

let jay =
  let arg l = snd ((Lazy.force jay_vectors) l) in
  let enc ty j = match C.encode ty j with Ok t -> t | Error m -> Alcotest.fail m in
  let int_ty = match C.ty_of_json (`Assoc [ ("jay", `String "int") ]) with Ok t -> t | Error m -> Alcotest.fail m in
  let bool_ty = match C.ty_of_json (`Assoc [ ("jay", `String "bool") ]) with Ok t -> t | Error m -> Alcotest.fail m in
  [ Alcotest.test_case "jay int encodes as TyBin's int_of (i5, im6, ibig)" `Quick (fun () ->
        List.iter
          (fun (label, n) -> Alcotest.(check bool) label true (enc int_ty (`Int n) = arg label))
          [ ("i5_Int", 5); ("im6_Int", -6); ("ibig_Int", 1048579) ];
        Alcotest.(check bool) "Bin 5 is TyBin's b5" true (JV.bin 5 = arg "b5_Bin");
        Alcotest.(check bool) "Bin 0 is TyBin's bzero" true (JV.bin 0 = arg "bzero_Bin"));
    Alcotest.test_case "jay bool encodes as his tt / ff" `Quick (fun () ->
        Alcotest.(check bool) "tt" true (enc bool_ty (`Bool true) = arg "tt_Bool");
        Alcotest.(check bool) "ff" true (enc bool_ty (`Bool false) = arg "ff_Bool"));
    Alcotest.test_case "jay values round-trip; other trees do not decode" `Quick (fun () ->
        List.iter
          (fun n ->
            match C.decode int_ty (enc int_ty (`Int n)) with
            | Ok (`Int m) -> Alcotest.(check int) "int round trip" n m
            | _ -> Alcotest.fail (string_of_int n))
          [ 0; 1; -1; 2; 5; -6; 1048579; max_int; -max_int ];
        Alcotest.(check bool) "a v1 int is not a jay int" true
          (Result.is_error (C.decode int_ty (Tuna.Int_enc.positive_of_decimal "5")));
        Alcotest.(check bool) "min_int is refused" true (Result.is_error (C.encode int_ty (`Int min_int)));
        Alcotest.(check bool) "a ternary value is taken as the claimed tree" true
          (enc int_ty (`Assoc [ ("ternary", `String "0") ]) = Tuna.Tree.Leaf));
    Alcotest.test_case "the embedded checker is the bin vectors' infer program" `Quick (fun () ->
        let p = Tuna.Canon.parse (Lazy.force JV.checker_ternary) in
        Alcotest.(check bool) "checker = infer O_bin" true (p = fst ((Lazy.force jay_vectors) "bzero_Bin"));
        Alcotest.(check string) "checker hash" (Tuna.Hash.hex_of_tree p) (Lazy.force JV.checker_hash)) ]

let () =
  Alcotest.run "tuna tool-language"
    [ ("verdict-std agrees", agreement); ("program identity", identity);
      ("json codec", codec); ("jay typed values (typing TY3)", jay) ]
