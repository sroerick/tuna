(* Jay's typed tree calculus on tuna's engine.

   reference/jay-typing-rules holds trees exported from Barry Jay's Rocq
   development "Typing Rules Become Programs in Tree Calculus" (MIT; see
   its README): his type checker infer / type_app / infer_expression for
   two ontologies, his example programs and values, and the result each
   of his proved Examples states.  Each vector applies his program to his
   arguments on the v0 engine (no accelerators) and requires his result
   exactly - a Sm T type, Lf for a refusal, or a value for an evaluation
   example.

   All 30 run under `dune runtest` (~25s): whole-expression typing is
   millions of v0 steps (infer typing itself is 5.2M), ~1us/step since
   the core caches subtree sizes for the cap check. *)

module F = Tuna_interp.Flat
module T = Tuna.Tree

let dir = "../reference/jay-typing-rules"
let lines file =
  let ic = open_in_bin (Filename.concat dir file) in
  let rec go acc = match input_line ic with l -> go (l :: acc) | exception End_of_file -> close_in ic; List.rev acc in
  List.filter (fun l -> l <> "") (go [])

let trees =
  lazy
    (let h = Hashtbl.create 64 in
     List.iter
       (fun l ->
         match String.index_opt l ' ' with
         | Some i -> Hashtbl.replace h (String.sub l 0 i) (Tuna.Canon.parse (String.sub l (i + 1) (String.length l - i - 1)))
         | None -> failwith ("bad trees line: " ^ l))
       (lines "trees");
     h)

let tree id =
  match Hashtbl.find_opt (Lazy.force trees) id with Some t -> t | None -> Alcotest.fail ("no tree " ^ id)

let host ~site:_ ~name ~args:_ = `Error ("prim " ^ name)

let vector line =
  match String.split_on_char ' ' line with
  | [ suite; label; p; args; want ] ->
      Some
        ( suite,
          Alcotest.test_case label `Quick (fun () ->
              let args = if args = "-" then [] else List.map tree (String.split_on_char ',' args) in
              let m = F.start ~fuel:20_000_000_000 ~size_cap:500_000_000 ~program:(tree p) ~args () in
              match F.run ~host m with
              | Tuna_interp.Prim_eval.Normal (got, _) ->
                  Alcotest.(check bool)
                    (Printf.sprintf "%s/%s: Jay's stated result (%s)" suite label want)
                    true (got = tree want)
              | _ -> Alcotest.fail (label ^ ": no normal form within budget")) )
  | _ -> None

(* TY1 (borg/typing.borg): Jay's is_concrete, exported with his spec's
   vectors (suite "concrete"), says K for every v1 stdlib def and every
   corpus program - no untyped tuna tree forges a label.

   TY-F4 OBSERVED 2026-11-25 (FINDINGS F17): five frozen v1 rows are not
   concrete - mag-cmp-fn contains the data constant
   Fork(%110, Fork(%0, %10)), which is exactly Jay's label[true]
   (S2[K, K1 K]), and mag-cmp / int-add / int-sub / int-cmp inherit
   it.  v1 rows are immutable, so the fix is stdlib v2.  The test pins
   the non-concrete set to EXACTLY these names: a new forged label
   fails it, and so does one disappearing (update the finding). *)
let known_nonconcrete_defs = [ "mag-cmp-fn"; "mag-cmp"; "int-add"; "int-sub"; "int-cmp" ]

let known_nonconcrete_corpus =
  List.map (fun n -> "stdlib_" ^ n ^ ".corpus") [ "int-add"; "int-cmp"; "int-sub"; "mag-cmp-fn"; "mag-cmp" ]
let is_concrete =
  lazy
    (match
       List.find_map
         (fun l ->
           match String.split_on_char ' ' l with
           | [ "concrete"; _; p; _; _ ] -> Some (tree p)
           | _ -> None)
         (lines "vectors")
     with
    | Some t -> t
    | None -> Alcotest.fail "no concrete suite in vectors")

let concrete t =
  let m = F.start ~fuel:2_000_000_000 ~size_cap:500_000_000 ~program:(Lazy.force is_concrete) ~args:[ t ] () in
  match F.run ~host m with
  | Tuna_interp.Prim_eval.Normal (T.Stem T.Leaf, _) -> `K
  | Tuna_interp.Prim_eval.Normal (T.Fork (T.Leaf, _), _) -> `KI
  | _ -> `Other

let read_file p =
  let ic = open_in_bin p in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let ty1 =
  let check what known items =
    let bad = List.filter (fun (_, t) -> concrete t <> `K) items in
    Alcotest.(check (list string))
      (Printf.sprintf "%s: %d checked; non-concrete set is exactly the recorded TY-F4 set" what
         (List.length items))
      (List.sort compare known) (List.sort compare (List.map fst bad))
  in
  [ Alcotest.test_case "v1 stdlib defs are concrete except the recorded TY-F4 set" `Quick (fun () ->
        read_file "../stdlib/v1/trees" |> String.split_on_char '\n'
        |> List.filter_map (fun l ->
               match String.split_on_char ' ' l with
               | [ name; _; tern ] -> Some (name, Tuna.Canon.parse tern)
               | _ -> None)
        |> check "stdlib v1" known_nonconcrete_defs);
    Alcotest.test_case "corpus programs are concrete except the recorded TY-F4 set" `Quick (fun () ->
        let dir = "../scripts/diff-corpus" in
        Sys.readdir dir |> Array.to_list |> List.sort compare
        |> List.filter_map (fun f ->
               if Filename.check_suffix f ".corpus" then
                 read_file (Filename.concat dir f) |> String.split_on_char '\n'
                 |> List.find_map (fun l ->
                        if String.length l > 8 && String.sub l 0 8 = "program " then
                          Some (f, Tuna.Canon.parse (String.sub l 8 (String.length l - 8)))
                        else None)
               else None)
        |> check "corpus" known_nonconcrete_corpus) ]

let () =
  let vs = List.filter_map vector (lines "vectors") in
  let suites = List.sort_uniq compare (List.map fst vs) in
  let shape =
    Alcotest.test_case "every vector line is well formed and every id resolves" `Quick (fun () ->
        Alcotest.(check int) "vector count" (List.length (lines "vectors")) (List.length vs);
        List.iter
          (fun l ->
            match String.split_on_char ' ' l with
            | [ _; _; p; a; w ] ->
                ignore (tree p); ignore (tree w);
                if a <> "-" then List.iter (fun x -> ignore (tree x)) (String.split_on_char ',' a)
            | _ -> ())
          (lines "vectors"))
  in
  Alcotest.run "tuna runs Jay's typed tree calculus"
    (("format", [ shape ]) :: ("ty1-concreteness", ty1)
    :: List.map (fun s -> (s, List.filter_map (fun (s', c) -> if s' = s then Some c else None) vs)) suites)
