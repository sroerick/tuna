(* Jay's typed tree calculus on tuna's engine.

   reference/jay-typing-rules holds trees exported from Barry Jay's Rocq
   development "Typing Rules Become Programs in Tree Calculus" (MIT; see
   its README): his type checker infer / type_app / infer_expression for
   two ontologies, his example programs and values, and the result each
   of his proved Examples states.  Each vector applies his program to his
   arguments on the v0 engine (no accelerators) and requires his result
   exactly - a Sm T type, Lf for a refusal, or a value for an evaluation
   example.

   TWO SIZES: the typing of whole expressions is millions of v0 steps
   (infer typing itself is 5.2M), so `dune runtest` runs the vectors in
   [slow] only under TUNA_JAY_FULL=1 (dune build --root . @tests/jay-full). *)

module F = Tuna_interp.Flat
module T = Tuna.Tree

let dir = "../reference/jay-typing-rules"
let full = Sys.getenv_opt "TUNA_JAY_FULL" = Some "1"

let slow =
  [ "infer_self_typed"; "rev_append_typed"; "reverse_typed"; "append_typed"; "length_typed";
    "filter_typed"; "map_typed"; "foldright_typed"; "append_pairs_typed" ]

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
      let speed = if List.mem label slow then `Slow else `Quick in
      Some
        ( suite,
          speed,
          Alcotest.test_case label speed (fun () ->
              let args = if args = "-" then [] else List.map tree (String.split_on_char ',' args) in
              let m = F.start ~fuel:20_000_000_000 ~size_cap:500_000_000 ~program:(tree p) ~args () in
              match F.run ~host m with
              | Tuna_interp.Prim_eval.Normal (got, _) ->
                  Alcotest.(check bool)
                    (Printf.sprintf "%s/%s: Jay's stated result (%s)" suite label want)
                    true (got = tree want)
              | _ -> Alcotest.fail (label ^ ": no normal form within budget")) )
  | _ -> None

let () =
  let vs = List.filter_map vector (lines "vectors") in
  let suites = List.sort_uniq compare (List.map (fun (s, _, _) -> s) vs) in
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
  Alcotest.run ~argv:(if full then [| "jay"; "--show-errors" |] else [| "jay"; "--show-errors"; "--quick-tests" |])
    "tuna runs Jay's typed tree calculus"
    (("format", [ shape ])
    :: List.map (fun s -> (s, List.filter_map (fun (s', _, c) -> if s' = s then Some c else None) vs)) suites)
