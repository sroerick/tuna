(* eval-modes E2 (borg/eval-modes.borg): accelerators under fast1.

   The chapter's tier 1 (DIFFERENTIAL, required to register): every
   accelerator answers exactly what its reference tree answers - on
   generated vectors that include malformed lists, junk trees and
   non-canonical ints - or declines and lets the v0 rewrite answer.
   Plus: the registry is pinned to the frozen stdlib; the whole
   differential corpus answers identically under fast1; partials are
   recognised at run time; a same-function tree under a different hash
   is not accelerated.  No PG. *)

open Tuna
module T = Tree
module B = Tuna_compiler.Bracket
module F = Tuna_interp.Flat
module A = Tuna_interp.Accel

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let dictionary =
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
           (name, (B.compile_source ~dictionary:dict src).B.tree) :: dict)
         [])

let compile src = (B.compile_source ~dictionary:(Lazy.force dictionary) src).B.tree
let def name = List.assoc name (Lazy.force dictionary)

let no_host ~site:_ ~name ~args:_ = `Error ("no prim " ^ name)

(* run under v0 or fast1; answer (result, accelerated calls, declines) *)
let run ?(fuel = 200_000_000) ?(size_cap = 50_000_000) ~fast program args =
  let m =
    F.start ?accel:(if fast then Some A.fast1 else None) ~fuel ~size_cap ~program
      ~args ()
  in
  let r = F.run ~host:no_host m in
  (r, F.accel_calls m, F.accel_declines m)

let show = function
  | Tuna_interp.Prim_eval.Normal (t, _) -> "normal " ^ Canon.encode t
  | Loop _ -> "loop"
  | Fuel_exhausted _ -> "fuel"
  | Size_exhausted _ -> "size"
  | Deadline_exceeded _ -> "deadline"

(* -- generated vectors (deterministic) -------------------------------- *)

let rng = Random.State.make [| 20261125 |]
let pick xs = List.nth xs (Random.State.int rng (List.length xs))

let rec junk d =
  if d = 0 then T.Leaf
  else
    match Random.State.int rng 3 with
    | 0 -> T.Leaf
    | 1 -> T.Stem (junk (d - 1))
    | _ -> T.Fork (junk (d - 1), junk (d - 1))

let list xs ending = List.fold_right (fun h t -> T.Fork (h, t)) xs ending

let lists () =
  let n = Random.State.int rng 6 in
  let xs = List.init n (fun _ -> junk 3) in
  list xs (pick [ T.Leaf; T.Leaf; T.Stem T.Leaf; T.Stem (junk 2) ])

let strings () =
  let alphabet = [ "a"; "<"; ">"; "&"; "\""; "'"; " "; "z"; "{"; "\xc3\xa9" ] in
  let s = String.concat "" (List.init (Random.State.int rng 8) (fun _ -> pick alphabet)) in
  if Random.State.int rng 5 = 0 then T.Fork (junk 3, Cstr.encode s) else Cstr.encode s

(* small magnitudes: the REFERENCE must finish for a vector to check
   anything (in-calculus int-mul is the F6 slope) *)
let canonical_int ?(bound = 256) () =
  let n = Random.State.int rng (2 * bound) - bound in
  if n < 0 then Int_enc.negative_of_decimal (string_of_int (-n))
  else Int_enc.positive_of_decimal (string_of_int n)

let ints ?bound () =
  match Random.State.int rng 6 with
  | 0 -> (* trailing false bit: not canonical *)
      (match canonical_int ?bound () with
       | T.Fork (s, m) -> T.Fork (s, list (List.map (fun x -> x) (let rec el = function T.Fork (h, t) -> h :: el t | _ -> [] in el m) @ [ T.Leaf ]) T.Leaf)
       | t -> t)
  | 1 -> T.Fork (T.Stem T.Leaf, T.Leaf) (* negative zero *)
  | 2 -> junk 3
  | _ -> canonical_int ?bound ()

let gen = function
  | "tree-eq" ->
      fun () ->
        let a = junk 4 in
        [ a; (if Random.State.bool rng then a else junk 4) ]
  | "list-append" -> fun () -> [ lists (); lists () ]
  | "list-length" | "list-reverse" -> fun () -> [ lists () ]
  | "html-escape" -> fun () -> [ strings () ]
  | "int-mul" -> fun () -> [ ints ~bound:16 (); ints ~bound:16 () ]
  | _ -> fun () -> [ ints (); ints () ]

let differential (e : A.entry) =
  Alcotest.test_case (Printf.sprintf "%s: 60 vectors agree with the reference" e.A.name) `Quick
    (fun () ->
      let f = def e.A.name in
      (* a coverage floor, not the correctness check: int natives
         accept only when BOTH inputs are canonical, ~1/4 of vectors *)
      let floor = if String.length e.A.name > 4 && String.sub e.A.name 0 4 = "int-" then 5 else 20 in
      let accepted = ref 0 in
      for _ = 1 to 60 do
        let args = gen e.A.name () in
        let r0, _, _ = run ~fast:false f args in
        let r1, calls, _ = run ~fast:true f args in
        accepted := !accepted + calls;
        Alcotest.(check string)
          (String.concat " " (List.map Canon.encode args))
          (show r0) (show r1)
      done;
      Alcotest.(check bool) (Printf.sprintf "natives answered (%d calls, floor %d)" !accepted floor) true
        (!accepted >= floor))

let registry =
  [ Alcotest.test_case "every registry row is the frozen stdlib tree" `Quick (fun () ->
        List.iter
          (fun e ->
            let t = def e.A.name in
            Alcotest.(check string) (e.A.name ^ " hash") e.A.hash (Hash.hex_of_tree t);
            Alcotest.(check int) (e.A.name ^ " size") e.A.size (T.size t))
          A.fast1) ]

let corpus =
  [ Alcotest.test_case "every corpus entry answers identically under fast1" `Quick (fun () ->
        let dir = "../scripts/diff-corpus" in
        let files =
          Sys.readdir dir |> Array.to_list
          |> List.filter (fun f -> Filename.check_suffix f ".corpus")
        in
        let n = ref 0 in
        List.iter
          (fun file ->
            let fields =
              read_all (Filename.concat dir file)
              |> String.split_on_char '\n'
              |> List.filter_map (fun l ->
                     match String.index_opt l ' ' with
                     | Some i -> Some (String.sub l 0 i, String.sub l (i + 1) (String.length l - i - 1))
                     | None -> None)
            in
            let get k = List.assoc_opt k fields in
            match (get "program", get "arg", get "fuel", get "size_cap") with
            | Some p, Some a, Some fu, Some sc -> (
                let tree s = match Canon.of_string s with Ok t -> t | Error _ -> Alcotest.fail file in
                let fuel = int_of_string fu and size_cap = int_of_string sc in
                let r0, _, _ = run ~fuel ~size_cap ~fast:false (tree p) [ tree a ] in
                let r1, _, _ = run ~fuel ~size_cap ~fast:true (tree p) [ tree a ] in
                match r0 with
                | Tuna_interp.Prim_eval.Normal _ ->
                    incr n;
                    Alcotest.(check string) file (show r0) (show r1)
                | _ -> ())
            | _ -> ())
          files;
        Alcotest.(check bool) (Printf.sprintf "%d normal entries compared" !n) true (!n > 50)) ]

let keying =
  [ Alcotest.test_case "a runtime partial (tree-eq x) is recognised per element" `Quick (fun () ->
        let p = compile "(lambda (x) (lambda (xs) (list-map (tree-eq x) xs)))" in
        let x = compile "#3" and xs = compile "[#1 #3 #2 #3 #3]" in
        let r0, _, _ = run ~fast:false p [ x; xs ] in
        let r1, calls, _ = run ~fast:true p [ x; xs ] in
        Alcotest.(check string) "same answer" (show r0) (show r1);
        Alcotest.(check bool) (Printf.sprintf "%d tree-eq calls answered natively" calls) true
          (calls >= 5));
    Alcotest.test_case "the same function under a different hash is not accelerated" `Quick
      (fun () ->
        (* append by explicit recursion: no registered tree inside it *)
        let p =
          compile
            "(rec-fix (lambda (self) (lambda (xs) (lambda (ys) ((tree-case ys (lambda (c) ys) (lambda (h) (lambda (t) (pair h (self t ys))))) xs)))))"
        in
        Alcotest.(check bool) "different tree" true (Hash.hex_of_tree p <> (List.find (fun e -> e.A.name = "list-append") A.fast1).A.hash);
        let args = [ compile "[#1 #2]"; compile "[#3]" ] in
        let r0, _, _ = run ~fast:false p args in
        let r1, calls, _ = run ~fast:true p args in
        Alcotest.(check string) "same answer" (show r0) (show r1);
        Alcotest.(check int) "no native call" 0 calls);
    Alcotest.test_case "every canonical int pair is answered natively" `Quick (fun () ->
        List.iter
          (fun name ->
            for _ = 1 to 15 do
              let args = [ canonical_int ~bound:16 (); canonical_int ~bound:16 () ] in
              let r0, _, _ = run ~fast:false (def name) args in
              let r1, calls, declines = run ~fast:true (def name) args in
              let label = name ^ " " ^ String.concat " " (List.map Canon.encode args) in
              Alcotest.(check string) label (show r0) (show r1);
              Alcotest.(check (pair int int)) (label ^ " answered") (1, 0) (calls, declines)
            done)
          [ "int-add"; "int-sub"; "int-mul"; "int-cmp" ]);
    Alcotest.test_case "a declined call rewrites as v0 (negative zero)" `Quick (fun () ->
        let args = [ T.Fork (T.Stem T.Leaf, T.Leaf); compile "5" ] in
        let r0, _, _ = run ~fast:false (def "int-add") args in
        let r1, calls, declines = run ~fast:true (def "int-add") args in
        Alcotest.(check string) "same answer" (show r0) (show r1);
        Alcotest.(check (pair int int)) "declined, not answered" (0, 1) (calls, declines)) ]

let () =
  Alcotest.run "tuna accelerators"
    [ ("registry", registry);
      ("differential", List.map differential A.fast1);
      ("corpus", corpus);
      ("keying", keying) ]
