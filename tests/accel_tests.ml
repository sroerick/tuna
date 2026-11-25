(* eval-modes E2 (borg/eval-modes.borg): accelerators under fast1.

   The chapter's tier 1 (DIFFERENTIAL, required to register): every
   accelerator answers exactly what its reference tree answers - on
   generated vectors that include malformed lists, junk trees and
   non-canonical ints - or declines and lets the v0 rewrite answer.
   Plus: the registry is pinned to the frozen stdlib; the whole
   differential corpus answers identically under fast1; partials are
   recognised at run time; a same-function tree under a different hash
   is not accelerated.  No PG.

   TWO SIZES.  The reference side of the int vectors is in-calculus
   arithmetic (the F6 slope) and dominates the cost, so `dune runtest`
   runs a SMOKE set of them (8 vectors per int op, 2 canonical pairs
   per op) and the FULL set (60 and 15) runs under
   `dune build --root . @tests/accel-full` (TUNA_ACCEL_FULL=1), which
   is required before any change to interpreter/lib/accel.ml or the
   accelerator path in flat.ml.  Every other case is identical in
   both. *)

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

(* run under v0 or a fast registry (fast1 by default); answer (result,
   accelerated calls, declines) *)
let run ?(fuel = 200_000_000) ?(size_cap = 50_000_000) ?(reg = A.fast1) ?(host = no_host)
    ~fast program args =
  let m =
    F.start ?accel:(if fast then Some reg else None) ~fuel ~size_cap ~program ~args ()
  in
  let r = F.run ~host m in
  (r, F.accel_calls m, F.accel_declines m)

(* a prim host that answers "log" with a running counter (unary) and
   records every call: the journal order is part of the semantics *)
let logging_host () =
  let log = ref [] in
  let host ~site:_ ~name ~args =
    log := (name ^ ":" ^ Canon.encode args) :: !log;
    `Done (Cstr.unary (List.length !log mod 3))
  in
  (host, fun () -> List.rev !log)

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

let full = Sys.getenv_opt "TUNA_ACCEL_FULL" = Some "1"
let is_int e = String.length e.A.name > 4 && String.sub e.A.name 0 4 = "int-"

let differential (e : A.entry) =
  let n = if is_int e && not full then 8 else 60 in
  Alcotest.test_case (Printf.sprintf "%s: %d vectors agree with the reference" e.A.name n) `Quick
    (fun () ->
      let f = def e.A.name in
      (* a coverage floor, not the correctness check: int natives
         accept only when BOTH inputs are canonical, ~1/4 of vectors
         (the canonical-pair case below checks acceptance exactly) *)
      let floor = if is_int e then (if full then 5 else 1) else 20 in
      let accepted = ref 0 in
      for _ = 1 to n do
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
          A.fast1.A.entries) ]

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
        Alcotest.(check bool) "different tree" true (Hash.hex_of_tree p <> (List.find (fun e -> e.A.name = "list-append") A.fast1.A.entries).A.hash);
        let args = [ compile "[#1 #2]"; compile "[#3]" ] in
        let r0, _, _ = run ~fast:false p args in
        let r1, calls, _ = run ~fast:true p args in
        Alcotest.(check string) "same answer" (show r0) (show r1);
        Alcotest.(check int) "no native call" 0 calls);
    Alcotest.test_case "every canonical int pair is answered natively" `Quick (fun () ->
        List.iter
          (fun name ->
            for _ = 1 to (if full then 15 else 2) do
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

(* -- fast2: records, the higher-order list family, compile-time partials *)

let records () =
  let key () = pick [ compile "#1"; compile "#2"; compile "#3"; junk 2 ] in
  let entry () =
    match Random.State.int rng 5 with
    | 0 -> junk 3
    | _ -> list [ key (); junk 2 ] (pick [ T.Leaf; T.Leaf; T.Stem T.Leaf ])
  in
  list (List.init (Random.State.int rng 5) (fun _ -> entry ())) (pick [ T.Leaf; T.Stem T.Leaf ])

let fns =
  lazy
    [ compile "is-stem"; compile "not"; compile "(lambda (x) (pair x x))";
      compile "(lambda (x) (tree-eq x #1))" ]

let fold_fns =
  lazy
    [ compile "(lambda (h) (lambda (acc) (pair (pair h %0) acc)))";
      compile "(lambda (h) (lambda (acc) (if acc h (is-leaf acc))))";
      compile "list-append" ]

let gen2 = function
  | "rec-val" | "rec-has" | "rec-get" ->
      fun () -> [ pick [ compile "#1"; compile "#2"; junk 2 ]; records () ]
  | "list-filter" | "list-map" -> fun () -> [ pick (Lazy.force fns); lists () ]
  | "list-fold" -> fun () -> [ pick (Lazy.force fold_fns); lists (); lists () ]
  | _ -> fun () -> []

let differential2 (e : A.entry) =
  Alcotest.test_case (Printf.sprintf "fast2 %s: 60 vectors agree with the reference" e.A.name) `Quick
    (fun () ->
      let f = def e.A.name in
      let accepted = ref 0 in
      for _ = 1 to 60 do
        let args = gen2 e.A.name () in
        let r0, _, _ = run ~fast:false f args in
        let r1, calls, _ = run ~reg:A.fast2 ~fast:true f args in
        accepted := !accepted + calls;
        Alcotest.(check string)
          (String.concat " " (List.map Canon.encode args))
          (show r0) (show r1)
      done;
      Alcotest.(check bool) (Printf.sprintf "natives answered (%d calls)" !accepted) true
        (!accepted >= 60))

let fast2_new = List.filter (fun e -> not (List.memq e A.fast1.A.entries)) A.fast2.A.entries

let fast2_cases =
  [ Alcotest.test_case "fast2 registry rows and embedded trees are the frozen stdlib" `Quick (fun () ->
        List.iter
          (fun e ->
            let t = def e.A.name in
            Alcotest.(check string) (e.A.name ^ " hash") e.A.hash (Hash.hex_of_tree t);
            Alcotest.(check int) (e.A.name ^ " size") e.A.size (T.size t);
            match A.tree_of_hash e.A.hash with
            | Some t' -> Alcotest.(check bool) (e.A.name ^ " embedded") true (t = t')
            | None -> Alcotest.fail (e.A.name ^ " not embedded"))
          A.fast2.A.entries);
    Alcotest.test_case "prim calls inside fn happen in the reference order" `Quick (fun () ->
        List.iter
          (fun (label, src, args) ->
            let p = compile src in
            let h0, log0 = logging_host () and h1, log1 = logging_host () in
            let r0, _, _ = run ~host:h0 ~fast:false p args in
            let r1, calls, _ = run ~host:h1 ~reg:A.fast2 ~fast:true p args in
            Alcotest.(check string) (label ^ " answer") (show r0) (show r1);
            Alcotest.(check (list string)) (label ^ " journal order") (log0 ()) (log1 ());
            Alcotest.(check bool) (Printf.sprintf "%s accelerated (%d)" label calls) true (calls > 0))
          [ ("list-map", "(lambda (xs) (list-map (lambda (h) (prim \"log\" h)) xs))",
             [ compile "[#1 #2 #3 #4]" ]);
            ("list-filter", "(lambda (xs) (list-filter (lambda (h) (prim \"log\" h)) xs))",
             [ compile "[#1 #2 #3 #4 #5]" ]);
            ("list-fold",
             "(lambda (xs) (list-fold (lambda (h) ((lambda (p) (lambda (acc) (pair p (pair (prim \"acc\" acc) %0)))) (prim \"log\" h))) #9 xs))",
             [ compile "[#1 #2 #3]" ]) ]);
    Alcotest.test_case "compile-time partials are accelerated under fast2, not fast1" `Quick (fun () ->
        List.iter
          (fun (src, args) ->
            let p = compile src in
            let r0, _, _ = run ~fast:false p args in
            let r1, _, _ = run ~reg:A.fast1 ~fast:true p args in
            let r2, c2, _ = run ~reg:A.fast2 ~fast:true p args in
            Alcotest.(check string) (src ^ " fast1") (show r0) (show r1);
            Alcotest.(check string) (src ^ " fast2") (show r0) (show r2);
            (* fast1 may accelerate pieces inside (tree-eq in rec-get's
               fold); fast2 answers the whole partial, so it takes fewer
               steps *)
            let steps = function Tuna_interp.Prim_eval.Normal (_, n) -> n | _ -> max_int in
            Alcotest.(check bool)
              (Printf.sprintf "%s: fast2 %d steps (%d native) < fast1 %d" src (steps r2) c2 (steps r1))
              true (steps r2 < steps r1))
          [ ("(lambda (r) (rec-val #2 r))", [ compile "[[#1 %10] [#2 %110]]" ]);
            ("(lambda (r) (rec-has #3 r))", [ compile "[[#1 %10] [#2 %110]]" ]);
            ("(lambda (xs) (list-filter is-stem xs))", [ compile "[%0 %10 %0 %10]" ]);
            ("(lambda (xs) (list-map not xs))", [ compile "[%0 %10]" ]) ]);
    Alcotest.test_case "every corpus entry answers identically under fast2" `Quick (fun () ->
        let dir = "../scripts/diff-corpus" in
        let n = ref 0 in
        Array.iter
          (fun file ->
            if Filename.check_suffix file ".corpus" then
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
                  let r2, _, _ = run ~fuel ~size_cap ~reg:A.fast2 ~fast:true (tree p) [ tree a ] in
                  match r0 with
                  | Tuna_interp.Prim_eval.Normal _ ->
                      incr n;
                      Alcotest.(check string) file (show r0) (show r2)
                  | _ -> ())
              | _ -> ())
          (Sys.readdir dir);
        Alcotest.(check bool) (Printf.sprintf "%d normal entries compared" !n) true (!n > 50)) ]

let () =
  Alcotest.run "tuna accelerators"
    [ ("registry", registry);
      ("differential", List.map differential A.fast1.A.entries);
      ("corpus", corpus);
      ("keying", keying);
      ("fast2 differential", List.map differential2 fast2_new);
      ("fast2", fast2_cases) ]
