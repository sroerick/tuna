(* eval-modes E3 (borg/eval-modes.borg): fast3, Jay's checker natives.

   The chapter's narrowed law: a checker native answers exactly what the
   reference answers WHENEVER THE REFERENCE TERMINATES, and declines
   when its ontology carries a prim.  These tests check that law against
   the v0 engine on GENERATED inputs (the vendored vectors in
   jay_tests.ml check his stated results):

   - equal1: the native's closed form for the partial (equal p) is the
     value v0 computes for it (his equal_eval);
   - infer and tp on two of his ontologies (O_list, and O_bin from
     typing.borg TY2): well-typed Bin and Int values, the same values with
     one subtree replaced by junk, junk trees, and type applications
     drawn from a pool of his types and junk - fast3 and v0 must give
     the same tree wherever v0 finishes within its fuel;
   - an ontology carrying a prim: the infer and row natives decline,
     and the run still answers as v0.

   Seeded generators, so a failure reproduces. *)

module F = Tuna_interp.Flat
module A = Tuna_interp.Accel
open Tuna.Tree

let dir = "../reference/jay-typing-rules"

let lines file =
  let ic = open_in_bin (Filename.concat dir file) in
  let rec go acc =
    match input_line ic with l -> go (l :: acc) | exception End_of_file -> close_in ic; List.rev acc
  in
  List.filter (fun l -> l <> "") (go [])

let trees =
  lazy
    (let h = Hashtbl.create 64 in
     List.iter
       (fun l ->
         let i = String.index l ' ' in
         Hashtbl.replace h (String.sub l 0 i)
           (Tuna.Canon.parse (String.sub l (i + 1) (String.length l - i - 1))))
       (lines "trees");
     h)

let vectors =
  lazy
    (List.map
       (fun l ->
         match String.split_on_char ' ' l with
         | [ s; lab; p; a; e ] ->
             let t = Hashtbl.find (Lazy.force trees) in
             (s, lab, t p, (if a = "-" then [] else List.map t (String.split_on_char ',' a)), t e)
         | _ -> failwith l)
       (lines "vectors"))

let fast3 = lazy (Option.get (A.registry_of_semantics "fast3"))
let family name = List.find (fun f -> f.A.fname = name) (Lazy.force fast3).A.families
let skel name = Option.get (A.family_skel (family name))
let host ~site:_ ~name ~args:_ = `Error ("prim " ^ name)

let native_calls = ref 0

let run ?accel ?(size_cap = 500_000_000) ?(host = host) ~fuel p args =
  let m = F.start ?accel ~fuel ~size_cap ~program:p ~args () in
  let r = F.run ~host m in
  native_calls := !native_calls + F.accel_calls m;
  r

let status = function
  | Tuna_interp.Prim_eval.Normal _ -> "normal"
  | Tuna_interp.Prim_eval.Fuel_exhausted _ -> "fuel"
  | Tuna_interp.Prim_eval.Size_exhausted _ -> "size"
  | _ -> "other"

let steps_of = function
  | Tuna_interp.Prim_eval.Normal (_, s) | Tuna_interp.Prim_eval.Fuel_exhausted s
  | Tuna_interp.Prim_eval.Size_exhausted s -> s
  | _ -> -1

(* the ontologies his vectors type against: every infer[O] program *)
let ontologies =
  lazy
    (List.fold_left
       (fun acc (_, _, p, _, _) ->
         match A.fit (skel "infer") p 1 with
         | Some [ o ] when not (List.exists (fun o' -> o' = o) acc) -> o :: acc
         | _ -> acc)
       [] (Lazy.force vectors))

let rng = Random.State.make [| 0xe3; 0xfa57 |]

let rec junk d =
  if d = 0 then Leaf
  else
    match Random.State.int rng 3 with
    | 0 -> Leaf
    | 1 -> Stem (junk (d - 1))
    | _ -> Fork (junk (d - 1), junk (d - 1))

(* TyBin.v's values: bzero, beven m, bodd m; Int = tagged (swap (sign * bin)) *)
let tagged f n = Fork (Stem (Fork (Leaf, f)), Fork (Stem (Stem Leaf), Fork (Leaf, n)))
let compose g f = Fork (Stem (Fork (Leaf, g)), f) (* g 'C f *)
let s2 u v = Fork (Stem u, v)
let k1 x = Fork (Leaf, x)
let bin_nm = Fork (Leaf, Leaf)
let sel_e = compose A.jfst A.jsnd
let sel_o = compose A.jsnd A.jsnd

let rec bin_value bits =
  match bits with
  | [] -> tagged A.jfst bin_nm
  | b :: rest -> tagged (s2 (if b then sel_o else sel_e) (k1 (bin_value rest))) bin_nm

let random_bin () = bin_value (List.init (Random.State.int rng 6) (fun _ -> Random.State.bool rng))

(* replace one random subtree with junk *)
let rec mutate t =
  match t with
  | Leaf | Stem Leaf -> junk 3
  | _ when Random.State.int rng 5 = 0 -> junk 3
  | Stem a -> Stem (mutate a)
  | Fork (a, b) -> if Random.State.bool rng then Fork (mutate a, b) else Fork (a, mutate b)

let fuel = 20_000_000
let typed = ref 0 and refused = ref 0

(* compare under both engines; true when v0 finished (so compared) *)
let agree what p args =
  match run ~fuel p args with
  | Tuna_interp.Prim_eval.Normal (want, _) -> (
      match run ~accel:(Lazy.force fast3) ~fuel p args with
      | Tuna_interp.Prim_eval.Normal (got, _) ->
          if got <> want then Alcotest.fail (what ^ ": fast3 and v0 differ");
          (match want with Stem _ -> incr typed | _ -> incr refused);
          true
      | _ -> Alcotest.fail (what ^ ": v0 finished, fast3 did not"))
  | Tuna_interp.Prim_eval.Size_exhausted _ ->
      (* the cap is part of the answer (EM2/EM5) *)
      let got = run ~accel:(Lazy.force fast3) ~fuel p args in
      if status got <> "size" then Alcotest.fail (what ^ ": v0 hit the size cap, fast3 answered " ^ status got);
      true
  | _ -> false

let differential name gen n =
  Alcotest.test_case name `Quick (fun () ->
      let os = Lazy.force ontologies in
      Alcotest.(check bool) "at least two ontologies" true (List.length os >= 2);
      let compared = ref 0 in
      native_calls := 0;
      typed := 0;
      refused := 0;
      List.iteri
        (fun oi o ->
          for i = 1 to n do
            let p, args = gen o in
            if agree (Printf.sprintf "%s ontology %d case %d" name oi i) p args then incr compared
          done)
        os;
      Printf.printf "%s: %d of %d compared (v0 finished): %d answered Sm _, %d other\n%!" name !compared
        (n * List.length os) !typed !refused;
      Alcotest.(check bool)
        (Printf.sprintf "%s: %d of %d compared (v0 finished)" name !compared (n * List.length os))
        true
        (!compared >= n * List.length os * 3 / 4);
      (* a broken recognition would fall back to v0 and still agree: the
         natives must actually have answered (review finding 2026-11-25) *)
      Alcotest.(check bool) (name ^ ": checker natives fired") true (!native_calls > 0))

let infer_of o = A.instantiate (skel "infer") [| o |]
let tp_of o = A.instantiate (skel "tp") [| o |]

(* a pool of types: his stated result types, his arguments, junk *)
let type_pool =
  lazy
    (List.concat_map
       (fun (_, _, _, args, e) -> (match e with Stem t -> [ t ] | _ -> []) @ args)
       (Lazy.force vectors)
    |> List.filter (fun t -> size t < 400)
    |> List.sort_uniq compare)

let pick l = List.nth l (Random.State.int rng (List.length l))

let cases =
  [ Alcotest.test_case "equal1 is the v0 value of (equal p) (his equal_eval)" `Quick (fun () ->
        let equal = A.jay_tree "equal" in
        for _ = 1 to 200 do
          let p = junk 7 in
          match run ~fuel equal [ p ] with
          | Tuna_interp.Prim_eval.Normal (v, _) ->
              if v <> A.equal1 p then Alcotest.fail "equal1 differs from (equal p)"
          | _ -> Alcotest.fail "v0 (equal p) did not finish"
        done);
    differential "infer: well-typed Bin and Int values" (fun o ->
        let b = random_bin () in
        let v =
          if Random.State.bool rng then b
          else
            let sign = if Random.State.bool rng then tagged A.jfst (Stem (Stem (Stem (Stem Leaf))))
              else tagged A.jsnd (Stem (Stem (Stem (Stem Leaf)))) in
            (* Int: tagged (swap (sign * bin)), swap p = S2[I, K1 p] *)
            tagged (s2 A.ji (k1 (s2 (s2 A.ji (k1 sign)) (k1 b)))) (Fork (Leaf, Stem Leaf))
        in
        (infer_of o, [ v ])) 25;
    differential "infer: mutated values and junk" (fun o ->
        let v = if Random.State.bool rng then mutate (random_bin ()) else junk 6 in
        (infer_of o, [ v ])) 25;
    differential "infer: his vector arguments, mutated" (fun o ->
        let args = List.concat_map (fun (_, _, _, a, _) -> a) (Lazy.force vectors) in
        let small = List.filter (fun t -> size t < 3000) args in
        (infer_of o, [ mutate (pick small) ])) 20;
    differential "tp: type applications from his types and junk" (fun o ->
        let pool = Lazy.force type_pool in
        let g () = if Random.State.int rng 4 = 0 then junk 5 else pick pool in
        (tp_of o, [ g (); g () ])) 40;
    Alcotest.test_case "an ontology carrying a prim: infer and row natives decline" `Quick (fun () ->
        let o = List.hd (Lazy.force ontologies) in
        let prim = Tuna.Cprim.call_tree ~name:"oracle" ~site:0 in
        let o' = A.instantiate (skel "row") [| Fork (Stem Leaf, Fork (Leaf, Leaf)); prim; o |] in
        let p = infer_of o' in
        let st = A.start ~reduce:F.reference_reduce (Lazy.force fast3) in
        let p = A.prepare ~reduce:F.reference_reduce st p in
        let v = bin_value [ true; false ] in
        (match A.lookup st p with
         | Some (e, []) ->
             Alcotest.(check string) "infer[O'] recognised" "infer" e.A.name;
             Alcotest.(check bool) "infer native declines" true (e.A.impl [ v ] = None)
         | _ -> Alcotest.fail "infer[O'] not recognised");
        (match A.lookup st o' with
         | Some (e, []) ->
             Alcotest.(check string) "the prim-carrying row recognised" "row" e.A.name;
             Alcotest.(check bool) "row native declines" true (e.A.impl [ bin_nm ] = None)
         | _ -> Alcotest.fail "row not recognised");
        Alcotest.(check bool) "the run answers as v0" true (agree "prim ontology" (infer_of o') [ v ])) ]

(* review findings 2026-11-25: the size cap, EM6 in steps, and the
   corrected law (d) on prims a parameter builds at run time *)
let rec big n = if n <= 1 then Leaf else Fork (big (n / 2), big (n - (n / 2) - 1))

let review_cases =
  [ Alcotest.test_case "a value a native builds is size-capped like v0's (EM5)" `Quick (fun () ->
        (* tp[O] @ triage[f0, Sm G, y] @ Sm X answers tp G X: a pair of
           ~60k-node trees, over a 100k cap *)
        let no_rows = Fork (Leaf, Fork (Leaf, Leaf)) in
        let tp = tp_of no_rows in
        let f = Fork (Fork (Leaf, Stem (big 60_001)), Leaf) and x = Stem (big 60_001) in
        let cap = 100_000 in
        let v0 = run ~size_cap:cap ~fuel tp [ f; x ] in
        native_calls := 0;
        let f3 = run ~accel:(Lazy.force fast3) ~size_cap:cap ~fuel tp [ f; x ] in
        Alcotest.(check string) "v0 hits the cap" "size" (status v0);
        Alcotest.(check string) "fast3 hits the cap" "size" (status f3);
        Alcotest.(check bool) "the tp native answered" true (!native_calls > 0));
    Alcotest.test_case "typing a 21-bit Int: natives fire, at most a tenth of v0 steps (EM6)" `Quick (fun () ->
        let _, _, p, args, want =
          List.find (fun (_, lab, _, _, _) -> lab = "ibig_Int") (Lazy.force vectors)
        in
        let v0 = run ~fuel p args in
        native_calls := 0;
        let f3 = run ~accel:(Lazy.force fast3) ~fuel p args in
        (match (v0, f3) with
         | Tuna_interp.Prim_eval.Normal (a, _), Tuna_interp.Prim_eval.Normal (b, _) ->
             Alcotest.(check bool) "both answer his stated type" true (a = want && b = want)
         | _ -> Alcotest.fail "both must finish");
        Alcotest.(check bool) "natives fired" true (!native_calls > 0);
        Alcotest.(check bool)
          (Printf.sprintf "fast3 %d steps <= v0 %d / 10" (steps_of f3) (steps_of v0))
          true
          (steps_of f3 * 10 <= steps_of v0));
    Alcotest.test_case "law (d) as written: a prim a parameter builds in discarded work is skipped" `Quick
      (fun () ->
        (* row[m, D, rest] @ m is a hit: v0 still evaluates rest @ m (rows
           are eager), and this rest builds and fires a prim call there;
           the native skips it.  Same answer, one prim call fewer - the
           corrected law (eval-modes E3 (d)) says exactly this. *)
        let calls = ref 0 in
        let counting ~site:_ ~name:_ ~args:_ = incr calls; `Done Leaf in
        let cs = match Tuna.Cprim.call_tree ~name:"oracle" ~site:0 with Fork (_, c) -> c | _ -> assert false in
        let rest = s2 (s2 (k1 (Stem Tuna.Cprim.gate)) (k1 cs)) A.ji in
        let m = Fork (Leaf, Leaf) in
        let row = A.instantiate (skel "row") [| m; Stem Leaf; rest |] in
        let v0 = run ~host:counting ~fuel row [ m ] in
        let v0_calls = !calls in
        calls := 0;
        let f3 = run ~host:counting ~accel:(Lazy.force fast3) ~fuel row [ m ] in
        (match (v0, f3) with
         | Tuna_interp.Prim_eval.Normal (a, _), Tuna_interp.Prim_eval.Normal (b, _) ->
             Alcotest.(check bool) "same answer" true (a = b)
         | _ -> Alcotest.fail "both must finish");
        Alcotest.(check (pair int int)) "prim calls v0, fast3" (1, 0) (v0_calls, !calls)) ]

let () =
  Alcotest.run "fast3: Jay's checker natives against v0" [ ("e3", cases); ("review", review_cases) ]
