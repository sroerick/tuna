(* M1 tests: tree sizes, canonical ternary roundtrip, hashes. *)

let leaf = Tuna.Tree.Leaf
let stem a = Tuna.Tree.Stem a
let fork (a, b) = Tuna.Tree.Fork (a, b)

(* _not = △ ((△) (△ △)) (△) — the delta that negates booleans.
   From the python reference: _not = ((((),),((),())),()),
   canonical encoding 22102000, size 8. *)
let not_tree = fork (fork (stem leaf, fork (leaf, leaf)), leaf)

let roundtrip_case name tree =
  Alcotest.test_case name `Quick (fun () ->
      let s = Tuna.Canon.encode tree in
      Alcotest.(check string) "encode stable" s (Tuna.Canon.encode tree);
      match Tuna.Canon.of_string s with
      | Ok t -> Alcotest.(check bool) "roundtrip" true (t = tree)
      | Error (off, msg) ->
          Alcotest.failf "parse failed at %d: %s" off msg)

let check_parse_error name input expected =
  Alcotest.test_case name `Quick (fun () ->
      match Tuna.Canon.of_string input with
      | Ok _ -> Alcotest.failf "%s unexpectedly parsed" name
      | Error (o, m) ->
          Alcotest.(check (pair int string)) name expected (o, m))

let () =
  Alcotest.run "common"
    [
      ( "tree"
      , [
          Alcotest.test_case "size(not)=8" `Quick (fun () ->
              Alcotest.(check int) "size not" 8 (Tuna.Tree.size not_tree));
          Alcotest.test_case "sizes of primitives" `Quick (fun () ->
              Alcotest.(check int) "leaf" 1 (Tuna.Tree.size leaf);
              Alcotest.(check int) "stem" 2 (Tuna.Tree.size (stem leaf));
              Alcotest.(check int) "fork" 3 (Tuna.Tree.size (fork (leaf, leaf))));
          Alcotest.test_case "height" `Quick (fun () ->
              Alcotest.(check int) "leaf height" 0 (Tuna.Tree.height leaf);
              Alcotest.(check int) "not height" 3 (Tuna.Tree.height not_tree));
        ] );
      ( "canon"
      , [
          roundtrip_case "leaf" leaf;
          roundtrip_case "stem" (stem leaf);
          roundtrip_case "fork" (fork (leaf, leaf));
          roundtrip_case "not" not_tree;
          roundtrip_case "nested"
            (fork (stem (fork (leaf, stem leaf)), fork (leaf, leaf)));
          Alcotest.test_case "encoding of not" `Quick (fun () ->
              (* ((((),),((),())),()) -> 22102000 *)
              Alcotest.(check string) "not encoding" "22102000"
                (Tuna.Canon.encode not_tree));
          Alcotest.test_case "canonical form is unique" `Quick (fun () ->
              let s = Tuna.Canon.encode not_tree in
              Alcotest.(check string) "hand-check not" "22102000" s;
              Alcotest.(check bool) "parse(encode x) = x" true
                (Tuna.Canon.parse s = not_tree);
              (* every parse of the encoding round-trips uniquely; two
                 different trees have different encodings *)
              Alcotest.(check bool) "leaf vs stem differ" false
                (String.equal (Tuna.Canon.encode leaf)
                   (Tuna.Canon.encode (stem leaf))));
          check_parse_error "bad char" "x" (0, "unexpected character 'x'");
          check_parse_error "trailing" "00" (1, "trailing characters after tree");
          check_parse_error "early end" "1" (1, "unexpected end of input");
        ] );
      ( "hash"
      , [
          Alcotest.test_case "sha256 of ternary string, lowercase hex" `Quick (fun () ->
              let s = Tuna.Canon.encode not_tree in
              let h = Tuna.Hash.hex_of_tree not_tree in
              Alcotest.(check string) "matches sha256 of encoding"
                (Tuna.Hash.hex_of_string s) h;
              Alcotest.(check string) "lowercase" (String.lowercase_ascii h) h;
              Alcotest.(check int) "length" 64 (String.length h));
          Alcotest.test_case "hand values" `Quick (fun () ->
              (* sha256("0") *)
              Alcotest.(check string) "sha256 leaf"
                "5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9"
                (Tuna.Hash.hex_of_tree leaf);
              (* sha256("22102000") — not's canonical form *)
              Alcotest.(check string) "sha256 of not's encoding"
                "1f6cae19aff40a81b0f8fcf17ea936269fcd2eaf27bf85148dbd8750a61ce320"
                (Tuna.Hash.hex_of_tree not_tree));
          Alcotest.test_case "hash distinguishes trees" `Quick (fun () ->
              Alcotest.(check bool) "not equal" false
                (String.equal (Tuna.Hash.hex_of_tree leaf)
                   (Tuna.Hash.hex_of_tree (stem leaf))));
          Alcotest.test_case "normalize_hex" `Quick (fun () ->
              Alcotest.(check string) "upper to lower" "abc"
                (Tuna.Hash.normalize_hex "ABC"));
        ] );
    ]
