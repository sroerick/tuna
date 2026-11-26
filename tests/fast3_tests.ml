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

let run ?accel ~fuel p args =
  let m = F.start ?accel ~fuel ~size_cap:500_000_000 ~program:p ~args () in
  F.run ~host m

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
  | _ -> false

let differential name gen n =
  Alcotest.test_case name `Quick (fun () ->
      let os = Lazy.force ontologies in
      Alcotest.(check bool) "at least two ontologies" true (List.length os >= 2);
      let compared = ref 0 in
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
        (!compared >= n * List.length os * 3 / 4))

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

let () = Alcotest.run "fast3: Jay's checker natives against v0" [ ("e3", cases) ]
