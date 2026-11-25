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

let () =
  Alcotest.run "tuna tool-language"
    [ ("verdict-std agrees", agreement); ("program identity", identity) ]
