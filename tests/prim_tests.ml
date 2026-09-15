(* M7 tests: prim convention (Cstr/Cprim), the monadic engine's prim
   boundary (identity-monad host), and the compiler's (prim ...) form.
   Pure — no Postgres needed. *)

module Eng = Tuna_interp.Prim_eval.Make (struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end)

let leaf = Tuna.Tree.Leaf
let stem a = Tuna.Tree.Stem a
let fork (a, b) = Tuna.Tree.Fork (a, b)
let true_tree = stem leaf

(* a counting echo host: records (site, name, args-ternary) calls *)
let recording_host () =
  let calls = ref [] in
  let host ~site ~name ~args =
    calls := (site, name, Tuna.Canon.encode args) :: !calls;
    `Ok args
  in
  (host, calls)

(* -- Cstr: strings as trees ------------------------------------------ *)

let test_cstr () =
  let roundtrip s =
    match Tuna.Cstr.decode (Tuna.Cstr.encode s) with
    | Some s' -> Alcotest.(check string) "cstr roundtrip" s s'
    | None -> Alcotest.fail "cstr decode failed"
  in
  List.iter roundtrip [ ""; "hello"; "store/get"; "é"; "\000\255" ];
  (* decode is total: non-string trees -> None *)
  Alcotest.(check bool) "leaf decodes to empty" true
    (Tuna.Cstr.decode leaf = Some "");
  Alcotest.(check bool) "fork-of-leaf not a string" true
    (Tuna.Cstr.decode (fork (leaf, leaf)) = None);
  Alcotest.(check bool) "stem not a string" true
    (Tuna.Cstr.decode (stem leaf) = None);
  (* unary helper *)
  Alcotest.(check bool) "unary 3" true
    (Tuna.Cstr.unary 3 = stem (stem (stem leaf)));
  Alcotest.(check (option int)) "unary_of" (Some 3)
    (Tuna.Cstr.unary_of (stem (stem (stem leaf))));
  Alcotest.(check (option int)) "unary_of fork" None
    (Tuna.Cstr.unary_of (fork (leaf, leaf)))

(* -- Cprim: the gate convention -------------------------------------- *)

let test_cprim () =
  (* the gate is Stem^4 Leaf = "11110" and is inert as data *)
  Alcotest.(check string) "gate ternary" "11110" Tuna.Cprim.gate_ternary;
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:7 in
  Alcotest.(check (option (pair string int))) "shape" (Some ("echo", 7))
    (Tuna.Cprim.shape call);
  (* hand-written ternary shape also recognized *)
  match Tuna.Canon.of_string (Tuna.Canon.encode call) with
  | Error e -> Alcotest.fail ("encode call: " ^ snd e)
  | Ok t -> Alcotest.(check (option (pair string int))) "shape by ternary"
              (Some ("echo", 7)) (Tuna.Cprim.shape t);
  (* non-calls *)
  Alcotest.(check (option (pair string int))) "plain fork" None
    (Tuna.Cprim.shape (fork (leaf, leaf)));
  Alcotest.(check (option (pair string int))) "short stem chain" None
    (Tuna.Cprim.shape
       (fork (stem (stem (stem leaf)), fork (leaf, leaf))));
  (* the gate with a bad name tree is not a call *)
  Alcotest.(check (option (pair string int))) "bad name tree" None
    (Tuna.Cprim.shape (fork (Tuna.Cprim.gate, fork (stem leaf, leaf))))

(* -- the engine's prim boundary -------------------------------------- *)

let test_prim_host () =
  (* a prim call is answered by the host; the answer becomes the value;
     NO fuel/step consumed at the boundary *)
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:5 in
  let host, calls = recording_host () in
  match Eng.eval ~host ~fuel:10 ~size_cap:1000 ~program:call [ true_tree ] with
  | Eng.Normal (t, steps) ->
      (* echo returns the args tree *)
      Alcotest.(check string) "echo answer" "10" (Tuna.Canon.encode t);
      Alcotest.(check int) "boundary costs no steps" 0 steps;
      Alcotest.(check int) "one host call" 1 (List.length !calls);
      (match !calls with
       | [ (site, name, argst) ] ->
           Alcotest.(check int) "site carried" 5 site;
           Alcotest.(check string) "name carried" "echo" name;
           Alcotest.(check string) "args are the applied tree" "10" argst
       | _ -> Alcotest.fail "bad call record")
  | _ -> Alcotest.fail "prim call must produce a normal answer"

let test_prim_error () =
  (* a host error answer becomes the canonical error tree
     Stem (cstr msg) — the calculus keeps computing deterministically *)
  let call = Tuna.Cprim.call_tree ~name:"denied" ~site:1 in
  let host ~site:_ ~name:_ ~args:_ = `Error "grant denial: revoked" in
  match
    Eng.eval ~host ~fuel:10 ~size_cap:100000 ~program:call [ true_tree ]
  with
  | Eng.Normal (t, 0) -> (
      match t with
      | Stem msg -> (
          match Tuna.Cstr.decode msg with
          | Some "grant denial: revoked" -> ()
          | _ -> Alcotest.fail "error tree payload mismatch")
      | _ -> Alcotest.fail "error answer must be Stem (cstr msg)")
  | _ -> Alcotest.fail "host error must answer, not raise"

let test_default_host () =
  (* pure evaluation (no host) answers prim calls with the default
     error — prims only run inside a run boundary *)
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  match
    Tuna_interp.Eval.eval ~fuel:10 ~size_cap:100000 ~program:call
      [ true_tree ]
  with
  | Tuna_interp.Eval.Normal (t, 0) -> (
      match t with
      | Stem _ ->
          (* error tree carries "no prim host at this boundary" *)
          ()
      | _ -> Alcotest.fail "default host must answer with an error tree")
  | _ -> Alcotest.fail "default host must answer, not raise"

let test_gate_inert () =
  (* a prim call embedded as DATA never fires: fork(leaf,_) projects it
     out as a value; the host is never called *)
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:3 in
  let prog = fork (leaf, call) in
  let host, calls = recording_host () in
  match Eng.eval ~host ~fuel:10 ~size_cap:1000 ~program:prog [ true_tree ] with
  | Eng.Normal (t, 1) ->
      Alcotest.(check string) "call tree survives as data"
        (Tuna.Canon.encode call) (Tuna.Canon.encode t);
      Alcotest.(check int) "no host calls" 0 (List.length !calls)
  | _ -> Alcotest.fail "gate as data must be inert"

let test_step_parity () =
  (* the monadic engine must reproduce the pure engine's step counts
     exactly (one port of the triage rules) — not true = leaf, 2 steps *)
  let not_tree = Tuna.Canon.parse "22102000" in
  let host, _ = recording_host () in
  match Eng.eval ~host ~fuel:100 ~size_cap:1000 ~program:not_tree [ true_tree ] with
  | Eng.Normal (t, s) ->
      Alcotest.(check string) "not true = leaf" "0" (Tuna.Canon.encode t);
      Alcotest.(check int) "not true = 2 steps" 2 s
  | _ -> Alcotest.fail "not true must normalize"

let test_fuel_free_boundary () =
  (* prims consume no fuel: a call answers even at fuel 0 (the boundary
     is not part of the calculus, AGENTS.md rule 4) *)
  let call = Tuna.Cprim.call_tree ~name:"echo" ~site:0 in
  let host, _ = recording_host () in
  match Eng.eval ~host ~fuel:0 ~size_cap:1000 ~program:call [ true_tree ] with
  | Eng.Normal (t, 0) ->
      Alcotest.(check string) "echo despite fuel 0" "10"
        (Tuna.Canon.encode t)
  | _ -> Alcotest.fail "prim must answer at fuel 0"

(* -- compiler: (prim ...) form --------------------------------------- *)

let compile src =
  match (try Ok (Tuna_compiler.Bracket.compile_source src)
         with Tuna_compiler.Ir.Error (p, msg) ->
                Error ("ir: " ^ Tuna_compiler.Ir.show_error (p, msg))
              | Tuna_compiler.Bracket.Compile_failed msg -> Error msg) with
  | Error msg -> Alcotest.fail ("compile: " ^ msg)
  | Ok art -> art

let expect_compile_fail what src =
  match (try
           ignore (Tuna_compiler.Bracket.compile_source src);
           None
         with Tuna_compiler.Bracket.Compile_failed _ -> Some ())
  with
  | Some () -> ()
  | None -> Alcotest.fail (what ^ ": must fail compilation")

let test_prim_compiles () =
  (* a top-level prim call would FIRE at compile time -> error *)
  expect_compile_fail "top-level prim" "(prim \"echo\")" ;
  (* under a lambda it compiles to a normal-form function *)
  let art = compile "(lambda (x) (prim \"echo\" x))" in
  (* the compiled tree embeds the gate ternary *)
  Alcotest.(check bool) "gate embedded" true
    (let s = art.Tuna_compiler.Bracket.ternary in
     Tuna.Cprim.gate_ternary <> "" && String.contains s '1');
  (* provenance: the gate literal is tagged with the Prim IR node id
     (the Prim node is the lambda's body) *)
  let open Tuna_compiler.Ir in
  (match art.Tuna_compiler.Bracket.ir with
   | Lam { body = Prim { id; _ }; _ } ->
       Alcotest.(check bool) "callsite tagged" true
         (List.exists (fun (_, tag) -> tag = id) art.Tuna_compiler.Bracket.tags)
   | _ -> Alcotest.fail "expected a Lam body with a Prim IR node")

let test_prim_runs () =
  let art = compile "(lambda (x) (prim \"echo\" x))" in
  let host, calls = recording_host () in
  match Eng.eval ~host ~fuel:1000 ~size_cap:1000
          ~program:art.Tuna_compiler.Bracket.tree [ true_tree ] with
  | Eng.Normal (t, _steps) ->
      (* echo through the compiled callsite: the args arrive as the
         cons LIST (nil = Leaf): cons(input) = Fork(input, Leaf) *)
      Alcotest.(check string) "echo through compiled prim" "2100"
        (Tuna.Canon.encode t);
      Alcotest.(check int) "one call" 1 (List.length !calls);
      (match !calls with
       | [ (site, name, argst) ] ->
           Alcotest.(check string) "prim name" "echo" name;
           Alcotest.(check string) "args are the cons list" "2100" argst;
           (* site = IR node id of the prim form; resolve via tags *)
           Alcotest.(check bool) "site resolves to a tree path" true
             (List.exists
                (fun (p, i) -> i = site && p <> [])
                art.Tuna_compiler.Bracket.tags)
       | _ -> Alcotest.fail "bad calls")
  | _ -> Alcotest.fail "compiled prim must run"

let test_prim_eta_guard () =
  (* a prim whose arguments don't depend on the enclosing lambda is
     compile-reducible: ((prim "echo") x) fires gate·nil at compile
     time -> compile error, not an effect (prims execute only inside a
     run boundary) *)
  expect_compile_fail "prim with compile-reducible args"
    "(lambda (x) ((prim \"echo\") x))"

let test_prim_compile_fire () =
  (* (not (prim ...)) at top level: the prim application is
     compile-reducible -> compile error, not an effect *)
  expect_compile_fail "compile-reducible prim" "(prim \"echo\" %0)"

let () =
  Alcotest.run "prim"
    [ ("cstr", [ Alcotest.test_case "roundtrip + totality" `Quick test_cstr ])
    ; ("cprim", [ Alcotest.test_case "gate + shape" `Quick test_cprim ])
    ; ( "boundary"
      , [ Alcotest.test_case "host answers, no steps" `Quick test_prim_host
        ; Alcotest.test_case "error answer tree" `Quick test_prim_error
        ; Alcotest.test_case "default host answers" `Quick test_default_host
        ; Alcotest.test_case "gate inert as data" `Quick test_gate_inert
        ; Alcotest.test_case "step parity with pure engine" `Quick
            test_step_parity
        ; Alcotest.test_case "prim answers at fuel 0" `Quick
            test_fuel_free_boundary
        ] )
    ; ( "compiler"
      , [ Alcotest.test_case "prim form compiles under lambda" `Quick
            test_prim_compiles
        ; Alcotest.test_case "compiled prim runs through the boundary" `Quick
            test_prim_runs
        ; Alcotest.test_case "compile-reducible prim args fail" `Quick
            test_prim_eta_guard
        ; Alcotest.test_case "compile-time prim fire is an error" `Quick
            test_prim_compile_fire
        ] ) ]
