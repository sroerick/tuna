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

  (* ---------- dialect: pair/cons aliases + string literals ---------- *)

  let dialect_tests =
    [
      compiles "pair aliases the leaf (the fork constructor)" "pair" "0";
      compiles "cons aliases the leaf too" "cons" "0";
      compiles "pair builds Fork(Leaf,Leaf)" "(pair 0 0)" "200";
      compiles "nested pair builds Fork(Fork(Leaf,Leaf),Leaf)"
        "(pair (pair 0 0) 0)" "22000";
      compiles "string literal is the Cstr tree of its text" "\"x\""
        (Tuna.Canon.encode (Tuna.Cstr.encode "x"));
      compiles "string literal with spaces lexes as one token" "\"hi there\""
        (Tuna.Canon.encode (Tuna.Cstr.encode "hi there"));
      compiles "lambda param shadows the pair builtin" "(lambda (pair) pair)"
        "21100";
      runs "pair applied at runtime forks" "(pair 0 0)" [] "200";
        runs "pair applied to runtime args" "(pair %10 %110)" [] "210110";
      Alcotest.test_case "runtime shields a constant-args prim" `Quick (fun () ->
          ignore
            (Tuna_compiler.Bracket.compile_source
               "(lambda (msg) (runtime (prim \"tree/get\" \"x\")))"));
      Alcotest.test_case "runtime zero-arg prim appends the param" `Quick (fun () ->
          ignore
            (Tuna_compiler.Bracket.compile_source
               "(lambda (msg) (runtime (prim \"now\")))"));
      compile_error "runtime at top level needs a param" "(runtime (prim \"now\"))" ""
        "enclosing lambda parameter";
      compile_error "runtime requires a prim call" "(lambda (msg) (runtime msg))" ""
        "expected a (prim";
      Alcotest.test_case "runtime arg already depends on a param is left alone" `Quick
        (fun () ->
          ignore
            (Tuna_compiler.Bracket.compile_source
               "(lambda (pair) (runtime (prim \"echo\" pair)))"));
    ]

(* ---------- dialect v0.1 sugar (borg/dialect.borg) ---------- *)

(* Desugar-equality: sugar source and its hand-written twin must
   compile to the SAME tree (hash) and the same compile steps — the law
   that makes sugar unable to smuggle semantics (acceptance 12.1). *)
let desugars name sugar twin expected =
  Alcotest.test_case name `Quick (fun () ->
      let a = Tuna_compiler.Bracket.compile_source sugar in
      let b = Tuna_compiler.Bracket.compile_source twin in
      Alcotest.(check string) (name ^ ": identical ternary") b.ternary a.ternary;
      Alcotest.(check string) (name ^ ": identical hash") b.hash_hex a.hash_hex;
      Alcotest.(check int) (name ^ ": identical steps") b.steps a.steps;
      match expected with
      | Some e -> Alcotest.(check string) (name ^ ": expected") e a.ternary
      | None -> ())

let sugar_tests =
  [
    (* brackets -> cons chains *)
    desugars "[1 2 3] is the pair chain" "[1 2 3]"
      "(pair 1 (pair 2 (pair 3 0)))" (Some "220210022020210022021021000");
    desugars "[] is 0" "[]" "0" (Some "0");
    desugars "[x] is (pair x 0)" "(lambda (x) [x])" "(lambda (x) (pair x 0))" None;
    desugars "nested brackets" "[[1 2] [3 4]]"
      "(pair (pair 1 (pair 2 0)) (pair (pair 3 (pair 4 0)) 0))" None;
    desugars "bracket with an application element" "(lambda (f) (lambda (x) [(f x) 1]))"
      "(lambda (f) (lambda (x) (pair (f x) (pair 1 0))))" None;
    (* numbers -> canonical law-5 Tree_lit *)
    desugars "1 decodes to int law 5" "1" "%202100" (Some "202100");
    desugars "2 decodes to int law 5" "2" "%20202100" None;
    desugars "42 decodes to int law 5" "42" "%202021020210202100" None;
    desugars "-7 decodes to int law 5" "-7" "%2102102102100" None;
    desugars "-0 canonicalizes to the one zero" "-0" "%200" None;
    desugars "00 canonicalizes to the one zero" "00" "%200" None;
    (* let -> nested lambda applications *)
    desugars "let single binding" "(let ((x 1)) x)" "((lambda (x) x) 1)" None;
    desugars "let sequential scoping" "(let ((x 1) (y x)) y)"
      "((lambda (x) ((lambda (y) y) x)) 1)" None;
    Alcotest.test_case "bare 0 stays the leaf literal (compat)" `Quick (fun () ->
        let a = Tuna_compiler.Bracket.compile_source "0" in
        Alcotest.(check string) "leaf" "0" a.ternary);
    Alcotest.test_case "lambda param shadows let (reserved head only)" `Quick
      (fun () ->
        let a = Tuna_compiler.Bracket.compile_source "(let ((let 1)) let)" in
        let b = Tuna_compiler.Bracket.compile_source "((lambda (let) let) 1)" in
        Alcotest.(check string) "identical" b.ternary a.ternary);
    compile_error "empty let binding list" "(let () 0)" "" "empty binding";
    compile_error "let binding without a body" "(let ((x 1)))" ""
      "unexpected ')'";
      (* match -> the triage application (dialect v0.3) *)
      (* the twin needs BOTH wrapper layers the desugar emits:
         inner = ((0 A) F) and outer = ((0 inner) C), so the hand-written
         form carries two 0s, not one.  As previously written the twin
         compiled to 2100 against the sugar's 202100 and this row failed
         on "identical ternary" -- a wrong fixture, not a wrong desugar. *)
      desugars "match dispatches to the triage application"
        "(match 0 ((leaf) 1) ((stem x) 2) ((fork a b) 3))"
        "(((0 ((0 1) (lambda (x) 2))) (lambda (a b) 3)) 0)" (Some "202100");
      runs "match takes the leaf branch for a leaf"
        "(match 0 ((leaf) 0) ((stem x) x) ((fork a b) a))" [] "0";
      (* subject %10 is Stem(Leaf), so the stem pattern binds x to the
         PAYLOAD (Leaf), and a branch body of `x` answers 0 -- which is
         also what the leaf branch would answer, so it cannot distinguish
         the two.  Return a distinct constant instead, so this row proves
         what its name claims: that a stem subject takes the stem branch. *)
      runs "match takes the stem branch for a stem"
        "(match %10 ((leaf) 0) ((stem x) %110) ((fork a b) a))" [] "110";
      runs "match binds the stem payload, not the whole stem"
        "(match %10 ((leaf) %110) ((stem x) x) ((fork a b) %110))" [] "0";
      compile_error "match needs all three branches"
        "(match 0 ((leaf) 0))" "" "needs leaf, stem and fork branches";
  ]

(* keyed literals: a {...} form maps each :name key to the dictionary
   name key-name and desugars to the bracket list of [key value] pairs,
   SORTED by key with duplicate keys refused (dialect v0.3: a record is
   canonical by construction - equal records are equal trees whatever
   order they were written in); with the key-* defs in the dictionary,
   the form and its hand twin compile to the identical tree. *)
let keyed_tests =
  let dict =
    [ ("key-state", Tuna.Canon.parse_exn "10")
    ; ("key-title", Tuna.Canon.parse_exn "110")
    ; ("key-who", Tuna.Canon.parse_exn "1110")
    ; ("key-when", Tuna.Canon.parse_exn "11110") ]
  in
  let comp src = Tuna_compiler.Bracket.compile_source ~dictionary:dict src in
  let keyed name form twin =
    Alcotest.test_case name `Quick (fun () ->
        let a = comp form and b = comp twin in
        Alcotest.(check string) (name ^ ": identical ternary") b.ternary a.ternary;
        Alcotest.(check int) (name ^ ": identical steps") b.steps a.steps)
  in
  [
    keyed "one key" "{:state 1}" "[[key-state 1]]";
    keyed "two keys" "{:state 1 :title \"x\"}"
      "[[key-state 1] [key-title \"x\"]]";
    keyed "keyed literal in a let" "(let ((r {:state 1})) r)"
      "(let ((r [[key-state 1]])) r)";
    keyed "keyed literal as an application argument"
      "(lambda (f) (f {:state 1}))" "(lambda (f) (f [[key-state 1]]))";
    keyed "empty keyed literal is the empty list" "{}" "[]";
      keyed "canonical: written order is irrelevant" "{:who 1110 :state 1}"
        "[[key-state 1] [key-who 1110]]";
      keyed "canonical: leading zeros collapse (law 5)" "{:who 01110 :state 1}"
        "{:who 1110 :state 1}";
      compile_error "duplicate keys are a reader error" "{:state 1 :state 2}" ""
        "duplicate key";
    Alcotest.test_case "keyed key resolves through the dictionary" `Quick
      (fun () ->
        (* a missing key names the missing key-<name> def *)
        match comp "{:nope 1}" with
        | _ -> Alcotest.fail "expected unbound key-nope"
        | exception Tuna_compiler.Ir.Error (_, msg) ->
            Alcotest.(check bool) "names key-nope" true
              (let needle = "key-nope" and n = String.length "key-nope" in
               let rec go i =
                 i + n <= String.length msg
                 && (String.sub msg i n = needle || go (i + 1))
               in
               go 0));
  ]

(* tool-language T3 (borg/tool-language.borg): #n tag literals and
   cond.  Every form is checked against its hand-written twin (dialect
   12.1).  cond leans on the name if, so the dictionary carries the std
   if tree, compiled here from its stdlib source. *)
let tool_language_tests =
  let std_if =
    (Tuna_compiler.Bracket.compile_source
       "(lambda (t) (lambda (f) (pair (pair f (pair %0 t)) %0)))").tree
  in
  let dict = [ ("if", std_if) ] in
  let comp src = Tuna_compiler.Bracket.compile_source ~dictionary:dict src in
  let twin name form hand =
    Alcotest.test_case name `Quick (fun () ->
        let a = comp form and b = comp hand in
        Alcotest.(check string) (name ^ ": identical ternary") b.ternary a.ternary;
        Alcotest.(check string) (name ^ ": identical hash") b.hash_hex a.hash_hex;
        Alcotest.(check int) (name ^ ": identical steps") b.steps a.steps)
  in
  let cond_runs name src args expected =
    Alcotest.test_case name `Quick (fun () ->
        let art = comp src in
        match
          Tuna_interp.Eval.eval ~fuel:10000 ~size_cap:10000 ~program:art.tree
            (List.map Tuna.Canon.parse_exn args)
        with
        | Tuna_interp.Eval.Normal (t, _) ->
            Alcotest.(check string) "result" expected (Tuna.Canon.encode t)
        | _ -> Alcotest.failf "%s: expected normal %s" name expected)
  in
  let cond_error name src msg = compile_error name src "" msg in
  let read_all path =
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic; s
  in
  [
    desugars "#0 is the leaf" "#0" "%0" (Some "0");
    desugars "#1 is stem leaf" "#1" "%10" (Some "10");
    desugars "#5 is stem^5 leaf" "#5" "%111110" (Some "111110");
    desugars "#n inside a record pair" "[[#2 1] [#3 2]]" "[[%110 1] [%1110 2]]" None;
    compile_error "#x is not a tag" "#x" "" "bad tag literal";
    compile_error "# alone is not a tag" "#" "" "bad tag literal";
    compile_error "#-1 is not a tag" "#-1" "" "bad tag literal";
    compile_error "#4097 is past the cap" "#4097" "" "bad tag literal";
    twin "cond with one clause is one if" "(lambda (a) (cond (a 1) (else 2)))"
      "(lambda (a) (if 1 2 a))";
    twin "cond nests first clause outermost"
      "(lambda (a b) (cond (a 1) (b 2) (else 3)))"
      "(lambda (a b) (if 1 (if 2 3 b) a))";
    twin "cond with only else is the else body" "(cond (else 7))" "7";
    twin "cond follows a let-bound if (shadowing wins)"
      "(let ((if (lambda (t) (lambda (f) (lambda (c) t))))) (lambda (a) (cond (a 1) (else 2))))"
      "(let ((if (lambda (t) (lambda (f) (lambda (c) t))))) (lambda (a) (if 1 2 a)))";
    cond_runs "cond takes the first true clause" "(lambda (a b) (cond (a #1) (b #2) (else #3)))"
      [ "10"; "10" ] "10";
    cond_runs "cond falls through a false clause" "(lambda (a b) (cond (a #1) (b #2) (else #3)))"
      [ "0"; "10" ] "110";
    cond_runs "cond reaches else" "(lambda (a b) (cond (a #1) (b #2) (else #3)))"
      [ "0"; "0" ] "1110";
    cond_error "cond needs an else clause" "(lambda (a) (cond (a 1)))" "needs an (else BODY)";
    cond_error "cond else must be last" "(lambda (a) (cond (else 1) (a 2)))"
      "else clause must be last";
    cond_error "cond clause needs test and body" "(lambda (a) (cond (a) (else 1)))" "";
    Alcotest.test_case "cond without any if in scope is unbound if" `Quick (fun () ->
        match Tuna_compiler.Bracket.compile_source "(lambda (a) (cond (a 1) (else 2)))" with
        | _ -> Alcotest.fail "expected unbound if"
        | exception Tuna_compiler.Ir.Error (_, msg) ->
            Alcotest.(check bool) ("names if: " ^ msg) true (contains "if" msg));
    (* T2 pinned names: name@<hex prefix of the resolved tree's sha256> *)
    (let if_hash = Tuna.Hash.hex_of_tree std_if in
     let pin n = "if@" ^ String.sub if_hash 0 n in
     twin "a holding 8-hex pin reads as the bare name"
       ("(lambda (a) (" ^ pin 8 ^ " 1 2 a))") "(lambda (a) (if 1 2 a))");
    (let if_hash = Tuna.Hash.hex_of_tree std_if in
     twin "a full 64-hex pin reads as the bare name"
       ("(lambda (a) (if@" ^ if_hash ^ " 1 2 a))") "(lambda (a) (if 1 2 a))");
    (let if_hash = Tuna.Hash.hex_of_tree std_if in
     let wrong = (if if_hash.[0] = '0' then "1" else "0") ^ String.sub if_hash 1 7 in
     Alcotest.test_case "a moved pin is an error naming both hashes" `Quick (fun () ->
         match comp ("(if@" ^ wrong ^ " 1 2 0)") with
         | _ -> Alcotest.fail "expected a pin mismatch"
         | exception Tuna_compiler.Ir.Error (_, msg) ->
             Alcotest.(check bool) ("names expected: " ^ msg) true (contains wrong msg);
             Alcotest.(check bool) ("names found: " ^ msg) true (contains if_hash msg)));
    compile_error "a pin on a lambda parameter is an error"
      "(lambda (x) x@12345678)" "" "lambda parameter cannot be pinned";
    compile_error "a pin on an unknown name is an error" "nope@12345678" ""
      "not in the dictionary";
    compile_error "a 7-hex pin is too short" "pair@1234567" "" "8 to 64 lowercase hex";
    compile_error "an uppercase pin is refused" "pair@ABCDEF12" "" "8 to 64 lowercase hex";
    compile_error "a pin needs a name before @ (a bare @ stays a bad token)" "@12345678" ""
      "unexpected token";
    compile_error "a pin's name part must be a name" "x#y@12345678" "" "must be a name";
    Alcotest.test_case "a builtin can be pinned (pair is the leaf)" `Quick (fun () ->
        let leaf_hash = Tuna.Hash.hex_of_tree Tuna.Tree.Leaf in
        let a = comp ("(pair@" ^ String.sub leaf_hash 0 8 ^ " 1 2)") and b = comp "(pair 1 2)" in
        Alcotest.(check string) "identical" b.ternary a.ternary);
    Alcotest.test_case "a pinned dictionary hit fires on_resolve with the bare name" `Quick
      (fun () ->
        let names = ref [] in
        let pinned = "if@" ^ String.sub (Tuna.Hash.hex_of_tree std_if) 0 8 in
        ignore
          (Tuna_compiler.Bracket.compile_source ~dictionary:dict
             ~on_resolve:(fun n _ -> names := n :: !names)
             ("(lambda (a) (" ^ pinned ^ " 1 2 a))"));
        Alcotest.(check (list string)) "resolved" [ "if" ] !names);
    Alcotest.test_case "verdict.sabra rewritten with cond and #n keeps its hash" `Quick
      (fun () ->
        let a = Tuna_compiler.Bracket.compile_source
            (read_all "../scripts/dialect/verdict-cond.sabra")
        and b = Tuna_compiler.Bracket.compile_source
            (read_all "../scripts/gate-receipts/verdict.sabra") in
        Alcotest.(check string) "identical hash" b.hash_hex a.hash_hex;
        Alcotest.(check int) "identical steps" b.steps a.steps);
  ]

let int_codec_tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ( "+42 is law 5",
        fun () ->
          Alcotest.(check string) "42" "202021020210202100"
            (Tuna.Canon.encode
               (Tuna.Int_enc.positive_of_decimal "42")) );
        ( "-42 flips the sign bit",
        fun () ->
          Alcotest.(check string) "-42" "2102021020210202100"
            (Tuna.Canon.encode
               (Tuna.Int_enc.negative_of_decimal "42")) );
      ( "zero is the one canonical form",
        fun () ->
          Alcotest.(check string) "0" "200"
            (Tuna.Canon.encode
               (Tuna.Int_enc.positive_of_decimal "0")) );
      ( "bare 0 is not claimed (leaf preserved)",
        fun () ->
          Alcotest.(check bool) "none" true
            (Tuna.Int_enc.of_decimal_atom "0" = None) );
      ( "-0 canonicalizes to the one zero",
        fun () ->
          Alcotest.(check string) "200" "200"
            (Tuna.Canon.encode
               (Tuna.Int_enc.negative_of_decimal "0")) );
      ( "leading zeros fold",
        fun () ->
          Alcotest.(check string) "123"
            (Tuna.Canon.encode (Tuna.Int_enc.positive_of_decimal "123"))
            (Tuna.Canon.encode (Tuna.Int_enc.positive_of_decimal "000123")) );
      ( "arbitrary precision (no OCaml int)",
        fun () ->
          (* 2^100 has a 101-bit magnitude; the decoder cannot have used
             an OCaml int to get here *)
          let t = Tuna.Int_enc.of_decimal_atom
              "1267650600228229401496703205376" in
          Alcotest.(check bool) "decoded" true (t <> None) );
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
    compile_error "bad token" "(lambda (x) @nope)" "" "unexpected token";
    (* 123 is now a valid literal; its desugar-equality twin is above. *)
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

(* -- compile budget (fuel + wall clock) -------------------------------- *)

(* Depth-d chain of identity applications: ((\y.y) ((\y.y) ...))
   bracket-abstracts to I(I(...I)) and fires ~1 triage rule per level
   during compile-time reduction. *)
let rec nested_identity d =
  if d = 0 then "(lambda (x) x)"
  else Printf.sprintf "((lambda (y) y) %s)" (nested_identity (d - 1))

let compile_budget_tests =
  let open Tuna_compiler in
  [ Alcotest.test_case "default compile fuel raised for reflective defs" `Quick
      (fun () ->
        (* 1e6 blocked crown-style reflective defs (bf2 bf2 not); the
           default must sit well above that *)
        Alcotest.(check int) "default fuel" 100_000_000
          Bracket.default_compile_fuel);
    Alcotest.test_case "fuel exhaustion is a clean compile failure" `Quick
      (fun () ->
        match
          Bracket.compile_source ~fuel:0 "((lambda (x) x) (lambda (x) x))"
        with
        | exception Bracket.Compile_failed msg ->
            Alcotest.(check bool) "fuel message" true
              (String.length msg > 0 && msg.[0] = 'c')
        | _ -> Alcotest.fail "expected Compile_failed");
    Alcotest.test_case "past wall-clock deadline aborts the compile" `Quick
      (fun () ->
        (* a past deadline aborts at the first 4096-firing poll; the
           fuel ceiling alone would let it run to completion *)
        match
          Bracket.compile_source ~fuel:100_000_000 ~deadline:0.0
            (nested_identity 10_000)
        with
          | exception Bracket.Compile_failed msg ->
              Alcotest.(check string) "wall-clock message"
                "compile-time evaluation exceeded the wall-clock budget \
                 (TUNA_COMPILE_MAX_SECONDS)"
                msg
        | _ -> Alcotest.fail "expected Compile_failed");
    Alcotest.test_case "untimed compile crosses the poll boundary fine" `Quick
      (fun () ->
        let art =
          Bracket.compile_source ~fuel:100_000_000 (nested_identity 10_000)
        in
        (* steps >= 4096 proves the clock poll point is reachable and
           harmless when the deadline is infinite *)
        Alcotest.(check bool) "fired past one poll window" true
          (art.Bracket.steps >= 4096);
        Alcotest.(check bool) "within fuel" true
          (art.Bracket.steps < 100_000_000));
  ]

(* ---------- def usage: the on_resolve census ----------
   The dictionary seam records every name that resolves at parse
   time (name, offset).  The census answers 'which defs does a
   program compile against' - compile adoption, the package-count
   view of the vocabulary.  These pins hold the contract: dict hits
   recorded with their source offsets, lambda params never recorded
   (shadowing wins), builtins never recorded, no dictionary = empty. *)
let resolved_of ?dictionary src =
  let names = ref [] in
  let _art =
    Tuna_compiler.Bracket.compile_source ?dictionary
      ~on_resolve:(fun name off -> names := (name, off) :: !names)
      src
  in
  List.rev !names

let def_usage_tests =
  [
    Alcotest.test_case "dictionary hit records name and offset" `Quick
      (fun () ->
        let dictionary = [ ("bump", Tuna.Cstr.encode "stub") ] in
        let r = resolved_of ~dictionary "(bump 0)" in
        Alcotest.(check int) "one hit" 1 (List.length r);
        (match r with
         | [ (name, off) ] ->
             Alcotest.(check string) "name" "bump" name;
             Alcotest.(check int) "offset at the atom" 1 off
         | _ -> Alcotest.fail "expected a single (name, off)"));
    Alcotest.test_case "lambda param shadows the dictionary and is not recorded" `Quick
      (fun () ->
        let dictionary =
          [ ("x", Tuna.Cstr.encode "stub"); ("bump", Tuna.Cstr.encode "stub") ]
        in
        let r = resolved_of ~dictionary "((lambda (x) x) (bump 0))" in
        (* the bound x never resolves; the dictionary bump does *)
        Alcotest.(check int) "only the dictionary hit" 1 (List.length r);
        (match r with [ (name, _) ] -> Alcotest.(check string) "name" "bump" name | _ -> ()));
    Alcotest.test_case "no dictionary, no census" `Quick
      (fun () ->
        let r = resolved_of "((lambda (x) x) 7)" in
        Alcotest.(check int) "empty" 0 (List.length r));
    Alcotest.test_case "two occurrences count twice" `Quick
      (fun () ->
        let dictionary = [ ("bump", Tuna.Cstr.encode "stub") ] in
        let r = resolved_of ~dictionary "(bump (bump 0))" in
        Alcotest.(check int) "two hits" 2 (List.length r));
  ]

let () =
  Alcotest.run "tuna compiler"
    [
        ("reader", reader_tests);
        ("dialect", dialect_tests);
        ("dialect sugar", sugar_tests);
        ("keyed literals", keyed_tests);
        ("tool-language", tool_language_tests);
        ("int codec", int_codec_tests);
        ("compile", compile_tests);
      ("extensional", extensional_tests);
      ("provenance", provenance_tests);
      ("def usage", def_usage_tests);
      ("compile budget", compile_budget_tests);
    ]
