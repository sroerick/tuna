(* math prim differential (borg/math-prims.borg acceptance 1): the host
   prims in Tuna_server.Math_prims are gated against the STDLIB
   REFERENCE SEMANTICS (the seeded sabra law-5 int ops) on a pinned
   vector table.  Small vectors are evaluated with the stdlib defs
   live (v0, exact); the big vectors are golden ternaries pinned by
   tools/stdprobe/golden.exe under the same reference.  Junk probes are
   prim-side canonicalization checks ONLY (the chapter's recorded
   split: the stdlib pins a junk-bit passthrough the prim does NOT
   mirror).  Pure suite - no PG. *)

module T = Tuna.Tree

let compile ~dictionary src =
  Tuna_compiler.Bracket.compile_source ~fuel:100000000 ~size_cap:100000000
    ~deadline:Float.infinity ~dictionary src

(* the seeded dictionary, compiled in record order (same walk as
   tools/stdprobe/freeze.ml; the defs come from the dune-rule-generated
   Stdlib_defs_test embedded from stdlib/v1/core.defs) *)
let dictionary =
  List.fold_left
    (fun acc (name, src) ->
      let art = compile ~dictionary:acc src in
      (name, art.Tuna_compiler.Bracket.tree) :: acc)
    [] Stdlib_defs_test.defs

let dict = Hashtbl.create 64
let () = List.iter (fun (n, t) -> Hashtbl.replace dict n t) dictionary

let std def args =
  match
    Tuna_interp.Eval.eval ~fuel:100_000_000 ~size_cap:1_000_000
      ~program:(Hashtbl.find dict def) args
  with
  | Tuna_interp.Eval.Normal (t, _) -> Tuna.Canon.encode t
  | r ->
      Alcotest.failf "stdlib reference anomaly on %s: %s" def
        (match r with
         | Tuna_interp.Eval.Loop n -> Printf.sprintf "loop@%d" n
         | Fuel_exhausted n -> Printf.sprintf "fuel@%d" n
         | Size_exhausted n -> Printf.sprintf "size@%d" n
         | Deadline_exceeded n -> Printf.sprintf "deadline@%d" n
         | Normal _ -> "?")

(* (name, def, prim, vectors-of-(arg-ternary-builts function, args)) *)
let check_bin def prim pairs () =
  List.iteri
    (fun i (a, b) ->
      let expected = std def [ a; b ] in
      let got = Tuna.Canon.encode (prim a b) in
      Alcotest.(check string)
        (Printf.sprintf "%s vector %d: prim == stdlib" (String.map (fun c -> if c = '/' then '.' else c) def) i)
        expected got)
    pairs

let check_un def prim singles () =
  List.iteri
    (fun i a ->
      let expected = std def [ a ] in
      let got = Tuna.Canon.encode (prim a) in
      Alcotest.(check string)
        (Printf.sprintf "%s vector %d: prim == stdlib" def i)
        expected got)
    singles

let int_of_ternary s =
  match Tuna.Canon.of_string s with Ok t -> t | Error (_, m) -> Alcotest.fail m

let int_of_bits (sign, bits) =
  let rec go i =
    if i = String.length bits then T.Leaf
    else T.Fork ((if bits.[i] = '1' then T.Stem T.Leaf else T.Leaf), go (i + 1))
  in
  T.Fork ((if sign = 1 then T.Stem T.Leaf else T.Leaf), go 0)

let check_golden name prim vectors () =
  List.iteri
    (fun i (a, b, expected) ->
      let got = Tuna.Canon.encode (prim (int_of_bits a) (int_of_bits b)) in
      Alcotest.(check string)
        (Printf.sprintf "%s golden %d: prim == stdlib golden" name i)
        expected got)
    vectors

let zero = int_of_ternary "200"
let neg1 = int_of_ternary "2102100"
let pos1 = int_of_ternary "202100"
let pos2 = int_of_ternary "20202100"
let neg2 = int_of_ternary "210202100"
let pos3 = int_of_ternary "202102100"
let neg3 = int_of_ternary "2102102100"
let neg0_junk = int_of_ternary "2100" (* -0: canonicalizes to +0 *)
let pos5 = int_of_ternary "20210202100"

(* -- differential vectors (canonical domain, mixed signs) ---------- *)

let add_vectors =
  [ (zero, zero); (pos1, neg1); (neg1, neg1); (pos3, neg1); (neg2, pos1)
  ; (pos5, pos3); (neg0_junk, pos1); (pos2, neg3); (neg3, neg3) ]

let sub_vectors =
  [ (pos3, zero); (zero, pos3); (neg3, neg3); (pos2, neg3); (neg3, neg2); (pos5, pos3) ]

let mul_vectors =
  [ (zero, pos5); (pos1, pos1); (pos2, pos3); (neg2, pos3); (neg2, neg3); (neg3, zero) ]

let cmp_vectors =
  [ (zero, zero); (pos1, neg1); (neg1, pos1); (neg2, neg3); (pos5, pos3); (neg0_junk, zero) ]

let neg_singles = [ zero; neg0_junk; pos1; neg1; pos3; pos5 ]

let () =
  let open Alcotest in
  run "math-prim-differential"
    [ ( "differential"
      , [ test_case "add" `Quick (check_bin "int-add" Tuna_server.Math_prims.add add_vectors)
        ; test_case "sub" `Quick (check_bin "int-sub" Tuna_server.Math_prims.sub sub_vectors)
        ; test_case "mul" `Quick (check_bin "int-mul" Tuna_server.Math_prims.mul mul_vectors)
        ; test_case "cmp" `Quick (check_bin "int-cmp" Tuna_server.Math_prims.cmp cmp_vectors)
        ; test_case "neg" `Quick (check_un "int-neg" Tuna_server.Math_prims.neg neg_singles) ] )
    ; ( "goldens (tests/math_prim_golden.ml; GENERATED under the reference)"
      , [ test_case "add 64-bit" `Quick (check_golden "math/add" Tuna_server.Math_prims.add Math_prim_golden.v_add)
        ; test_case "sub 64-bit" `Quick (check_golden "math/sub" Tuna_server.Math_prims.sub Math_prim_golden.v_sub)
        ; test_case "cmp 64-bit" `Quick (check_golden "math/cmp" Tuna_server.Math_prims.cmp Math_prim_golden.v_cmp)
        ; test_case "mul 8-bit" `Quick (check_golden "math/mul" Tuna_server.Math_prims.mul Math_prim_golden.v_mul) ] )
    ; ( "junk-canonicalization (prim-side only; the recorded split)"
      , [ test_case "junk bit low reads 0" `Quick (fun () ->
            let junk_mag = int_of_ternary "22002100" in (* [fork, t] == [f,t]=2 strictly *)
            check string "neg([junk,t]) = -2" "210202100"
              (Tuna.Canon.encode (Tuna_server.Math_prims.neg (T.Fork (T.Leaf, junk_mag))))
          )
        ; test_case "junk top-level leaf -> +0" `Quick (fun () ->
            check string "neg(leaf) = +0" "200"
              (Tuna.Canon.encode (Tuna_server.Math_prims.neg T.Leaf));
            check string "junk-stem-int + 1 = +1" "202100"
              (Tuna.Canon.encode (Tuna_server.Math_prims.add (T.Stem T.Leaf) pos1))
          )
        ; test_case "junk fork sign reads positive" `Quick (fun () ->
            check string "fork(fork(0,0), [t]) neg = -1" "2102100"
              (Tuna.Canon.encode (Tuna_server.Math_prims.neg (int_of_ternary "22002100")))
          ) ] ) ]
