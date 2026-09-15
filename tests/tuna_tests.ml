(* M2 tests: interpreter — apply verbatim, step counting, fuel/size budgets. *)

let leaf = Tuna.Tree.Leaf
let stem a = Tuna.Tree.Stem a
let fork (a, b) = Tuna.Tree.Fork (a, b)

(* _not = △ ((△) (△ △)) (△) — the delta that negates booleans.
   From the python reference: _not = ((((),),((),())),()),
   canonical encoding 22102000, size 8. *)
let not_tree = fork (fork (stem leaf, fork (leaf, leaf)), leaf)

let true_tree = stem leaf
let false_tree = leaf

(* omega: self-application fixed point that never terminates.
   f = △(△(△)△)△ = "221000" (Fork (Fork (Stem Leaf, Leaf), Leaf)):
   upstream-verified (python reference) f f = f — rule fork(fork,_)
   fires once per application of f to f, and the wrappers rebuild f
   unchanged. A run of f against a long arg list of f's therefore
   consumes exactly one step per arg and diverges; fuel N halts with
   Fuel_exhausted at exactly N. *)
let omega_f = fork (fork (stem leaf, leaf), leaf)
let omega_args n = List.init n (fun _ -> omega_f)

(* grower g = 2100: applying g to true = 10 gives Fork (b, Stem b),
   and the arg fold then grows one fork per application — exercises
   mid-run size_cap exhaustion. *)
let grower = fork (stem leaf, leaf)

(* M1 tests: tree sizes, canonical ternary roundtrip, hashes. *)

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

(* result pretty-viewer for checks *)
let result_of_run ~fuel ~size_cap program args =
  Tuna_interp.Eval.eval ~fuel ~size_cap ~program args

let check_normal name tree steps res =
  Alcotest.test_case name `Quick (fun () ->
      match res with
      | Tuna_interp.Eval.Normal (t, s) ->
          Alcotest.(check bool) (name ^ ": tree") true (t = tree);
          Alcotest.(check int) (name ^ ": steps") steps s
      | _ -> Alcotest.fail (name ^ ": expected Normal"))

let check_exhausted name expected_steps expect_fuel res =
  Alcotest.test_case name `Quick (fun () ->
      match res, expect_fuel with
      | Tuna_interp.Eval.Fuel_exhausted s, true
      | Tuna_interp.Eval.Size_exhausted s, false ->
          Alcotest.(check int) (name ^ ": steps") expected_steps s
      | _ -> Alcotest.fail (name ^ ": wrong status"))

let () =
  Alcotest.run "tuna"
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
      ( "eval"
      , [
          (* AGENTS.md rule 4 known-good: not true -> leaf in 2 steps. *)
          check_normal "not true -> leaf in 2 steps" leaf 2
            (result_of_run ~fuel:100 ~size_cap:1000 not_tree [ true_tree ]);
          check_normal "not false -> true in 1 step" true_tree 1
            (result_of_run ~fuel:100 ~size_cap:1000 not_tree [ false_tree ]);
          (* wrapper applications are not steps: apply Leaf b = Stem b
             in 0 steps, apply (Stem a) b = Fork(a,b) in 0 steps. *)
          check_normal "wrapper: leaf applied grows, 0 steps"
            (stem true_tree) 0
            (result_of_run ~fuel:0 ~size_cap:1000 leaf [ true_tree ]);
          check_normal "wrapper: stem applied grows, 0 steps"
            (fork (leaf, true_tree)) 0
            (result_of_run ~fuel:0 ~size_cap:1000 (stem leaf) [ true_tree ]);
          (* fuel 1: not true fires the fork(fork,_) triage rule, then
             needs fork(leaf,_) with zero fuel left -> exact halt at 1. *)
          check_exhausted "not true fuel 1 halts exactly at 1" 1 true
            (result_of_run ~fuel:1 ~size_cap:1000 not_tree [ true_tree ]);
          check_normal "not true fuel 2 completes" leaf 2
            (result_of_run ~fuel:2 ~size_cap:1000 not_tree [ true_tree ]);
          (* args fold left to right: not(not(true)) = fork(fork,_) on
             not (1 step: it reconstructs not), then not true (2 steps). *)
          check_normal "not not true -> leaf in 3 steps" leaf 3
            (result_of_run ~fuel:100 ~size_cap:1000 not_tree [ not_tree; true_tree ]);
          check_normal "omega 2 applications = f in 2 steps" omega_f 2
            (result_of_run ~fuel:100 ~size_cap:1000 omega_f (omega_args 2));
          (* omega: diverges; budget is exact — halts at exactly fuel. *)
          check_exhausted "omega fuel 0" 0 true
            (result_of_run ~fuel:0 ~size_cap:1000 omega_f (omega_args 200));
          check_exhausted "omega fuel 1" 1 true
            (result_of_run ~fuel:1 ~size_cap:1000 omega_f (omega_args 200));
          check_exhausted "omega fuel 2" 2 true
            (result_of_run ~fuel:2 ~size_cap:1000 omega_f (omega_args 200));
          check_exhausted "omega fuel 17" 17 true
            (result_of_run ~fuel:17 ~size_cap:1000 omega_f (omega_args 200));
          (* determinism: two runs, same result and same step count. *)
          Alcotest.test_case "determinism" `Quick (fun () ->
              let r1 =
                result_of_run ~fuel:50 ~size_cap:50 not_tree
                  [ not_tree; false_tree ]
              in
              let r2 =
                result_of_run ~fuel:50 ~size_cap:50 not_tree
                  [ not_tree; false_tree ]
              in
              Alcotest.(check bool) "same result" true (r1 = r2));
          (* size_cap trips mid-run on real growth. grower g = 2100
             (Fork (Stem Leaf, Leaf)): applying g to true = 10 gives
             Fork (b, Stem b), and the arg fold then grows one fork per
             application. With cap 8 the third application's wrapper
             (size 10 > 8) trips after 4 triage firings; with cap 10 the
             same run completes in the same 4 steps. *)
          check_exhausted "grower size_cap 8 trips after 4 steps" 4 false
            (result_of_run ~fuel:100 ~size_cap:8 grower [ true_tree; true_tree; true_tree ]);
          check_normal "grower cap 10 completes in 4 steps"
            (fork (stem leaf, fork (stem leaf, fork (leaf, true_tree)))) 4
            (result_of_run ~fuel:100 ~size_cap:10 grower [ true_tree; true_tree; true_tree ]);
          (* oversize argument: 0 steps, size exhausted (args are
             checked before any application). *)
          check_exhausted "oversize arg: 0 steps" 0 false
            (result_of_run ~fuel:100 ~size_cap:7 leaf [ not_tree ]);
          check_exhausted "omega fuel+size: fuel wins at exact 17" 17 true
            (result_of_run ~fuel:17 ~size_cap:6 omega_f (omega_args 200));
          (* oversize input: 0 steps, size exhausted. *)
          check_exhausted "oversize program: 0 steps" 0 false
            (result_of_run ~fuel:100 ~size_cap:7 not_tree []);
          (* fuel still applies across the arg fold. *)
          check_exhausted "fuel spans the arg fold" 1 true
            (result_of_run ~fuel:1 ~size_cap:1000 not_tree [ not_tree; true_tree ]);
        ] );
    ]
