(* M9 tests: the REPL surface's pure parts — command parsing, the
   name->tree dictionary read into the compiler (capture-safe
   substitution), and first-diff.  Pure — no Postgres needed. *)

let not_tree =
  match Tuna.Canon.of_string "22102000" with
  | Ok t -> t
  | Error _ -> assert false

let leaf = Tuna.Tree.Leaf
let true_tree = Tuna.Tree.Stem leaf

let eval_ter src (dict : (string * Tuna.Tree.t) list) ~inputs ~fuel =
  match Tuna_compiler.Bracket.compile_source ~dictionary:dict src with
  | exception Tuna_compiler.Ir.Error (p, msg) ->
      `Compile (Tuna_compiler.Ir.show_error (p, msg))
  | exception Tuna_compiler.Bracket.Compile_failed msg -> `Compile msg
  | art -> (
      match Tuna.Canon.of_string art.Tuna_compiler.Bracket.ternary with
      | Error _ -> `Compile "artifact unparseable"
      | Ok program -> (
          match Tuna_interp.Eval.eval ~fuel ~size_cap:1_000_000 ~program inputs with
          | Tuna_interp.Eval.Normal (t, steps) -> `Normal (Tuna.Canon.encode t, steps)
          | Tuna_interp.Eval.Fuel_exhausted s -> `Fuel s
          | Tuna_interp.Eval.Size_exhausted s -> `Size s))

let test_parse_command () =
  let show (c : Tuna_server.Repl_cmd.command) =
    match c with
    | Tuna_server.Repl_cmd.Eval s -> "eval:" ^ s
    | Tuna_server.Repl_cmd.Def (n, s) -> "def:" ^ n ^ ":" ^ s
    | Tuna_server.Repl_cmd.Undef n -> "undef:" ^ n
    | Tuna_server.Repl_cmd.Get (p, None) -> "get:" ^ p
    | Tuna_server.Repl_cmd.Get (p, Some h) -> "get:" ^ p ^ ":" ^ h
    | Tuna_server.Repl_cmd.Patch (p, t, None) -> "patch:" ^ p ^ ":" ^ t
    | Tuna_server.Repl_cmd.Patch (p, t, Some h) -> "patch:" ^ p ^ ":" ^ t ^ ":" ^ h
    | Tuna_server.Repl_cmd.FirstDiff (a, b) -> "fd:" ^ a ^ ":" ^ b
    | Tuna_server.Repl_cmd.Dict -> "dict"
  in
  let check name expected line =
    match Tuna_server.Repl_cmd.parse_command line with
    | c -> Alcotest.(check string) name expected (show c)
    | exception Tuna_server.Repl_cmd.Parse_error m ->
        Alcotest.failf "%s: unexpected parse error %s" name m
  in
  check "bare term" "eval:(f x)" "(f x)" |> ignore;
  check "eval keyword" "eval:(lambda (x) x)" "eval (lambda (x) x)";
  check "def" "def:not:%22102000" "def not %22102000" |> ignore;
  check "undef" "undef:not" "undef not" |> ignore;
  check "get last" "get:012" "get 012" |> ignore;
  check "get hash" "get:012:abc" "get 012 abc" |> ignore;
  check "patch" "patch:1:%0" "patch 1 %0" |> ignore;
  check "patch hash" "patch:1:%0:aa" "patch 1 %0 aa" |> ignore;
  check "first-diff" "fd:aa:bb" "first-diff aa bb" |> ignore;
  check "dict" "dict" "dict" |> ignore;
  (* malformed lines are Parse_error values, never crashes *)
  let expect_error name line =
    match Tuna_server.Repl_cmd.parse_command line with
    | _ -> Alcotest.failf "%s: expected parse error" name
    | exception Tuna_server.Repl_cmd.Parse_error _ -> ()
  in
  expect_error "empty" "";
  expect_error "unknown cmd" "frobnicate x y";
  expect_error "undef bad name" "undef %221";
  expect_error "get no path" "get";
  expect_error "fd one hash" "first-diff aa"

let test_dictionary_substitution () =
  (* a dictionary-bound name reads as a literal tree: (lambda (y) (f y))
     with f = not compiles to the not tree itself (eta) *)
  (match
     eval_ter "(lambda (y) (f y))" [ ("f", not_tree) ] ~inputs:[ true_tree ]
               ~fuel:1000
   with
   | `Normal (t, steps) ->
       Alcotest.(check string) "dict f=not applied to true" "0" t;
       Alcotest.(check int) "steps" 2 steps
   | _ -> Alcotest.fail "expected normal result");
  (* literal application at top level: compile IS reduction —
     (f %0) = not applied to leaf = true, reduced during compile *)
  (match
     eval_ter "(f %0)" [ ("f", not_tree) ] ~inputs:[] ~fuel:1000
   with
   | `Normal (t, _) ->
       Alcotest.(check string) "top-level dict application reduces at compile" "10" t
   | _ -> Alcotest.fail "expected normal form");
  (* free names NOT in the dictionary are still unbound errors *)
  Alcotest.(check bool) "unbound stays unbound"
    (match eval_ter "(g %0)" [] ~inputs:[] ~fuel:1000 with
     | `Compile m -> String.contains m 'g' && String.length m > 0
     | _ -> false)
    true

let test_capture_safety () =
  (* a lambda parameter shadowing a dictionary name binds the parameter,
     never the dictionary value: (lambda (f) (f %0)) is the same tree
     with or without f = not in the dictionary *)
  let with_dict =
    match
      Tuna_compiler.Bracket.compile_source ~dictionary:[ ("f", not_tree) ]
        "(lambda (f) (f %0))"
    with
    | a -> a.Tuna_compiler.Bracket.hash_hex
    | exception _ -> Alcotest.fail "compile failed with dict"
  in
  let without_dict =
    match Tuna_compiler.Bracket.compile_source "(lambda (f) (f %0))" with
    | a -> a.Tuna_compiler.Bracket.hash_hex
    | exception _ -> Alcotest.fail "compile failed without dict"
  in
  Alcotest.(check string) "shadowing lambda wins" without_dict with_dict;
  (* and the compiled term really applies its argument: applying it to
     not gives not leaf = true (1 step), not not-not behavior *)
  (match
     Tuna_compiler.Bracket.compile_source ~dictionary:[ ("f", not_tree) ]
       "(lambda (f) (f %0))"
   with
   | exception _ -> Alcotest.fail "compile failed"
   | art -> (
       match Tuna.Canon.of_string art.Tuna_compiler.Bracket.ternary with
       | exception _ | Error _ -> Alcotest.fail "artifact unparseable"
       | Ok program -> (
           match
             Tuna_interp.Eval.eval ~fuel:1000 ~size_cap:1_000_000 ~program
               [ not_tree ]
           with
           | Tuna_interp.Eval.Normal (t, 5) ->
               (* shadowed f applies its argument: not leaf = true.  5
                  steps (not 1): the artifact is the SK-machine form
                  s i (k 0)-shaped, and its combinator overhead is
                  charged — the counting rule is per triage firing *)
               Alcotest.(check string) "shadowed f applies its arg" "10"
                 (Tuna.Canon.encode t)
           | _ -> Alcotest.fail "expected not-leaf = true")))

let () =
  let open Alcotest in
  run "repl"
    [ ("commands", [ test_case "parse" `Quick test_parse_command ])
    ; ("dictionary", [ test_case "substitution" `Quick test_dictionary_substitution ])
    ; ( "capture"
      , [ test_case "shadowing" `Quick test_capture_safety ] ) ]
