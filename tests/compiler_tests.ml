(* M4 tests: compiler — surface reader, bracket abstraction with eta,
   compile-time reduction, provenance.

   Expected compiled forms cross-checked against the python reference
   (reference/tree-calculus/implementation/python/tree-calculus.py):
   id = 21100 (upstream's id tree; apply(id,x)=x), s i i = 212110021100
   (verified: apply(sii,sii) diverges). Extensional behavior is
   verified through the M2 interpreter (results, not encodings, except
   where the expected tree IS the SK normal form). *)

open Tuna.Tree

let leaf = Leaf
let stem a = Stem a
let fork (a, b) = Fork (a, b)

let not_tree = fork (fork (stem leaf, fork (leaf, leaf)), leaf)
let true_tree = stem leaf
let false_tree = leaf

let contains sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

(* ---------- helpers ---------- *)

let compiles name src expected_ternary =
  Alcotest.test_case name `Quick (fun () ->
      match Tuna_compiler.Bracket.compile_source src with
      | art -> Alcotest.(check string) "ternary" expected_ternary art.ternary
      | exception e ->
          Alcotest.failf "%s: unexpected failure: %s" src (Printexc.to_string e))

let compile_error name src path_sub msg_sub =
  Alcotest.test_case name `Quick (fun () ->
      match Tuna_compiler.Bracket.compile_source src with
      | _ -> Alcotest.failf "%s: expected a compile error" name
      | exception Tuna_compiler.Ir.Error (p, msg) ->
          let shown = Tuna_compiler.Ir.show_error (p, msg) in
          if path_sub <> "" then
            Alcotest.(check bool)
              (Printf.sprintf "diagnostic carries IR path %S: %S" path_sub shown)
              true (contains path_sub shown);
          if msg_sub <> "" then
            Alcotest.(check bool)
              (Printf.sprintf "message %S contains %S" msg msg_sub)
              true (contains msg_sub msg))

let compile_failed name src fuel =
  Alcotest.test_case name `Quick (fun () ->
      match Tuna_compiler.Bracket.compile_source ~fuel src with
      | _ -> Alcotest.failf "%s: expected Compile_failed" name
      | exception Tuna_compiler.Bracket.Compile_failed _ -> ()
      | exception e ->
          Alcotest.failf "%s: wrong failure: %s" name (Printexc.to_string e))

let runs name src args expected =
  Alcotest.test_case name `Quick (fun () ->
      let art = Tuna_compiler.Bracket.compile_source src in
      match Tuna_interp.Eval.eval ~fuel:1000 ~size_cap:1000 ~program:art.tree args with
      | Tuna_interp.Eval.Normal (t, _) ->
          Alcotest.(check string) "result" expected (Tuna.Canon.encode t)
      | _ -> Alcotest.failf "%s: expected normal %s" name expected)

(* ---------- reader: literals, sugar, comments, applications ---------- *)

let reader_tests =
  [
    compiles "leaf literal" "0" "0";
    compiles "tree literal by ternary" "%22102000" "22102000";
    compiles "comment lines are ignored" "; negation\n(lambda (x) x)\n; done\n" "21100";
    compiles "multi-arg lambda sugar" "(lambda (x y) x)" "10";
    compiles "eta reduction" "(lambda (f) (lambda (x) (f x)))" "21100";
    compiles "application is left-assoc"
      "((lambda (x) (lambda (y) x)) %10 %110)" "10";
  ]

(* ---------- compile: SK elimination with eta; compile IS reduction ---------- *)

let compile_tests =
  [
    compiles "compile identity -> upstream id tree" "(lambda (x) x)" "21100";
    compiles "compile K" "(lambda (x) (lambda (y) x))" "10";
    compiles "compile S applicator (s i i)"
      "(lambda (x) (x x))" "212110021100";
    compiles "compile-time eval of a closed redex" "((lambda (x) x) %10)" "10";
    compiles "compile-time eval, nested redexes"
      "((lambda (x) (lambda (y) x)) %10 %110)" "10";
    compiles "compile-time eval preserves literal shapes"
      "((lambda (f) f) %22102000)" "22102000";
    compile_failed "divergent term fails cleanly at fuel"
      "((lambda (x) (x x)) (lambda (y) (y y)))" 200;
    compile_error "unbound variable reported with IR path"
      "(lambda (x) y)" "IR path 0" "unbound variable";
    compile_error "unbound in application carries its path"
      "(lambda (x) (y x))" "IR path 0.0" "unbound variable";
    compile_error "empty parameter list" "(lambda () 0)" "" "empty parameter";
    compile_error "missing body" "(lambda (x))" "" "unexpected ')'";
    compile_error "bad token" "(lambda (x) 123)" "" "unexpected token";
    compile_error "bad tree literal" "%229" "" "bad tree literal";
  ]

(* ---------- extensional: compiled programs run correctly ---------- *)

(* zero = k i: (lambda (f) (lambda (x) x)) -> Fork (Leaf, i-tree) *)
let zero_src = "(lambda (f) (lambda (x) x))"
(* succ = λn.λf.λx. f ((n f) x) — church succ *)
let succ_src = "(lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))"

let extensional_tests =
  let zero_tree = (Tuna_compiler.Bracket.compile_source zero_src).tree in
  [
    runs "compiled zero applied to f then x yields x" zero_src [ not_tree; false_tree ] "0";
    runs "compiled succ: succ zero f false = not false = true"
      succ_src
      [ zero_tree; not_tree; false_tree ]
      "10";
    runs "compiled K projects its first argument"
      "(lambda (x) (lambda (y) x))" [ true_tree; false_tree ] "10";
    runs "compiled S: s not not false = not false (not false)"
      "(lambda (f) (lambda (g) (lambda (x) ((f x) (g x)))))"
      [ not_tree; not_tree; false_tree ]
      "2010";
  ]

(* ---------- provenance ---------- *)

let determinism =
  Alcotest.test_case "compile determinism (hash + tags)" `Quick (fun () ->
      let src = "(lambda (f) (lambda (g) (lambda (x) ((f x) (g x)))))" in
      let a = Tuna_compiler.Bracket.compile_source src in
      let b = Tuna_compiler.Bracket.compile_source src in
      Alcotest.(check string) "same hash" a.hash_hex b.hash_hex;
      Alcotest.(check int) "same tag count" (List.length a.tags) (List.length b.tags);
      List.iter2
        (fun (pa, ta) (pb, tb) ->
          Alcotest.(check bool) "same path" true (pa = pb);
          Alcotest.(check int) "same tag" ta tb)
        a.tags b.tags)

let provenance_id =
  Alcotest.test_case "provenance: tree path -> IR node id" `Quick (fun () ->
      (* ((lambda (x) x) 0): i applied to Leaf reduces to Leaf at
         compile time; the compiled node is the 0 literal moved through
         the triage rule, so the root's tag is the literal's id. *)
      let art = Tuna_compiler.Bracket.compile_source "((lambda (x) x) 0)" in
      Alcotest.(check string) "shape" "0" art.ternary;
      let lit_id =
        match art.ir with
        | Tuna_compiler.Ir.App { arg; _ } -> (
            match arg with
            | Tuna_compiler.Ir.Leaf_lit { id; _ } -> id
            | _ -> Alcotest.fail "arg should be the 0 literal")
        | _ -> Alcotest.fail "root should be the application node"
      in
      let prov = Tuna_compiler.Provenance.of_artifact art in
      Alcotest.(check (option int)) "root <- 0 literal" (Some lit_id)
        (Tuna_compiler.Provenance.tag_at prov []);
      Alcotest.(check (option int)) "out-of-bounds path is None" None
        (Tuna_compiler.Provenance.tag_at prov [ 0 ]);
      Alcotest.(check bool) "describe resolves the IR path" true
        (contains "IR path" (Tuna_compiler.Provenance.describe prov [])))

let provenance_lambda_tags =
  Alcotest.test_case "provenance: synthesized nodes tag the lambda" `Quick (fun () ->
      (* (lambda (x) x) compiles to i = Fork (Stem (Stem Leaf), Leaf);
         every node is synthesized by the abstraction of the lambda. *)
      let art = Tuna_compiler.Bracket.compile_source "(lambda (x) x)" in
      Alcotest.(check string) "ternary" "21100" art.ternary;
      let lam_id =
        match art.ir with
        | Tuna_compiler.Ir.Lam { id; _ } -> id
        | _ -> Alcotest.fail "root should be the lambda"
      in
      let prov = Tuna_compiler.Provenance.of_artifact art in
      Alcotest.(check int) "5 nodes" 5 (List.length prov.tags);
      List.iter
        (fun (p, tag) ->
          Alcotest.(check (option int))
            (Printf.sprintf "path %s tagged lam" (Tuna_compiler.Ir.show_path p))
            (Some lam_id) (Some tag))
        prov.tags)

let provenance_tests = [ determinism; provenance_id; provenance_lambda_tags ]

let () =
  Alcotest.run "tuna compiler"
    [
      ("reader", reader_tests);
      ("compile", compile_tests);
      ("extensional", extensional_tests);
      ("provenance", provenance_tests);
    ]
