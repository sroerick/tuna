(* Tuna_local.Local_repl: one REPL round against the local store
   (borg/local.borg L2).  The grammar, compile-with-lock and patch are
   the server's own (Tuna_repl); only the backing differs.  Resolution
   is the server's: lambda param > own defs > frozen std > builtins.

   Rounds mirror Tuna_server.Repl_cmd.execute: eval and def compile,
   run the program on the round's inputs (a def round runs with none)
   and become the session's last result; undef, get, patch, first-diff
   and dict are structural.  Prim calls are not answered here (L3). *)

module B = Tuna_compiler.Bracket
module C = Tuna_repl.Repl_core
module P = Tuna_repl.Patch
module S = Local_store

type outcome = {
  kind : string;
  status : string option;
  ternary : string;
  hash : string;
  steps : int;
  program_hash : string option;
  note : string;
  dict_rows : (string * string) list;
  lock : (string * string) list;
}

let outcome ?status ?program_hash ?(note = "") ?(dict_rows = []) ?(ternary = "")
    ?(hash = "") ?(steps = 0) ?(lock = []) kind =
  { kind; status; ternary; hash; steps; program_hash; note; dict_rows; lock }

type run_opts = {
  fuel : int;
  size_cap : int;
  mode : Tuna_interp.Eval.mode;
  accel : Tuna_interp.Accel.registry option;
  inputs : Tuna.Tree.t list;
}

let status_of = function
  | Tuna_interp.Eval.Normal (t, s) -> ("normal", Some t, s)
  | Tuna_interp.Eval.Loop s -> ("loop", None, s)
  | Tuna_interp.Eval.Fuel_exhausted s -> ("fuel_exhausted", None, s)
  | Tuna_interp.Eval.Size_exhausted s -> ("size_exhausted", None, s)
  | Tuna_interp.Eval.Deadline_exceeded s -> ("deadline_exceeded", None, s)

(* own defs over std: the compiler is last-wins, so own rows go LAST *)
let effective_dictionary st = Tuna_stdlib.Std_dict.dictionary () @ S.dict st

(* compile, store the program, run it, store the result, move [last] *)
let round st opts ~kind ~inputs src =
  let dictionary = effective_dictionary st in
  let art, _counts, lock = C.compile_with_dict ~dictionary src in
  let phash = S.put_value st art.B.tree in
  let status, result, steps =
    status_of
      (Tuna_interp.Eval.eval ~fuel:opts.fuel ~size_cap:opts.size_cap
         ~mode:opts.mode ?accel:opts.accel ~program:art.B.tree inputs)
  in
  let ternary, hash =
    match result with
    | Some t ->
        let h = S.put_value st t in
        S.set_last st h;
        (Tuna.Canon.encode t, h)
    | None -> ("", "")
  in
  (art, outcome ~status ~program_hash:phash ~ternary ~hash ~steps ~lock kind)

let base_tree st = function
  | Some h -> (
      match S.get_value st h with
      | Some t -> Ok (t, Some h)
      | None -> Error (Printf.sprintf "unknown program hash %s" h))
  | None -> (
      match S.last st with
      | None -> Error "no previous round yet — eval something first"
      | Some h -> (
          match S.get_value st h with
          | Some t -> Ok (t, Some h)
          | None -> Error (Printf.sprintf "last result %s is missing" h)))

let exec st opts (cmd : C.command) : (outcome, string) result =
  match cmd with
  | C.Eval src -> Ok (snd (round st opts ~kind:"eval" ~inputs:opts.inputs src))
  | C.Def (name, src) ->
      let art, o = round st opts ~kind:"def" ~inputs:[] src in
      S.bind st ~name ~value_hash:art.B.hash_hex;
      Ok
        { o with ternary = art.B.ternary; hash = art.B.hash_hex;
                 note = Printf.sprintf "defined %s" name }
  | C.Undef name ->
      if List.mem_assoc name (S.dict_hashes st) then
        S.bind st ~name ~value_hash:S.undef_marker;
      Ok (outcome ~note:("undefined " ^ name) "undef")
  | C.Get (path, h) -> (
      match P.normalize_path path with
      | None -> Error (Printf.sprintf "bad path %S" path)
      | Some p -> (
          match base_tree st h with
          | Error e -> Error e
          | Ok (t, phash) -> (
              match P.at_path t (P.digits p) with
              | None -> Error (Printf.sprintf "path %S escapes the tree" path)
              | Some sub ->
                  Ok
                    (outcome ~ternary:(Tuna.Canon.encode sub)
                       ~hash:(Tuna.Hash.hex_of_tree sub) ?program_hash:phash
                       ~note:(Printf.sprintf "subtree at path %s" p) "get"))))
  | C.Patch (path, new_ternary, h) -> (
      match P.normalize_path path with
      | None -> Error (Printf.sprintf "bad path %S" path)
      | Some p -> (
          let t = String.trim new_ternary in
          let bare =
            if String.length t > 0 && t.[0] = '%' then
              String.sub t 1 (String.length t - 1)
            else t
          in
          match Tuna.Canon.of_string bare with
          | Error (off, msg) ->
              Error (Printf.sprintf "new_ternary parse error at offset %d: %s" off msg)
          | Ok new_sub -> (
              match base_tree st h with
              | Error e -> Error e
              | Ok (tree, phash) -> (
                  match P.at_path tree (P.digits p) with
                  | None -> Error (Printf.sprintf "path %S escapes the tree" path)
                  | Some sub -> (
                      match
                        P.apply_patch ~old_tree:tree ~path:p ~believed:None
                          ~expected_old_hash:(Tuna.Hash.hex_of_tree sub) ~new_sub
                      with
                      | P.Bad_path _ -> Error "bad path (unreachable)"
                      | P.Conflict _ -> Error "patch conflict"
                      | P.Applied { ternary; hash } ->
                          (match Tuna.Canon.of_string ternary with
                           | Ok nt -> ignore (S.put_value st nt)
                           | Error _ -> ());
                          Ok
                            (outcome ~ternary ~hash ?program_hash:phash
                               ~note:
                                 (Printf.sprintf "patched path %s: %s -> %s" p
                                    (Tuna.Canon.encode sub) ternary)
                               "patch"))))))
  | C.FirstDiff (a, b) -> (
      match (S.get_value st a, S.get_value st b) with
      | None, _ -> Error (Printf.sprintf "unknown program hash %s" a)
      | _, None -> Error (Printf.sprintf "unknown program hash %s" b)
      | Some ta, Some tb -> (
          match P.first_diff ta tb with
          | None ->
              Ok
                (outcome ~hash:a
                   ~note:(Printf.sprintf "no structural difference between %s and %s" a b)
                   "first-diff")
          | Some d ->
              let sub_of t =
                match P.at_path t (P.digits d) with
                | Some s -> Tuna.Hash.hex_of_tree s
                | None -> Tuna.Hash.hex_of_tree t
              in
              Ok
                (outcome ~ternary:d ~hash:d
                   ~note:
                     (Printf.sprintf "first difference at path %s: %s (%s) vs %s (%s)"
                        d a (sub_of ta) b (sub_of tb))
                   "first-diff")))
  | C.Dict ->
      let rows =
        List.map (fun (n, t) -> (n, Tuna.Canon.encode t)) (S.dict st)
      in
      Ok
        (outcome ~dict_rows:rows
           ~note:(if rows = [] then "dictionary is empty" else "dictionary entries")
           "dict")

(* parse + execute; compile errors are values, as on the server *)
let execute st opts line : (outcome, string) result =
  match C.parse_command line with
  | exception C.Parse_error msg -> Error msg
  | cmd -> (
      try exec st opts cmd with
      | Tuna_compiler.Ir.Error (p, msg) ->
          Error ("compile error: " ^ Tuna_compiler.Ir.show_error (p, msg))
      | B.Compile_failed msg -> Error ("compile failed: " ^ msg)
      | Failure msg -> Error msg)

(* key-value rendering, one field per line (as `tuna compile`) *)
let render (o : outcome) =
  let b = Buffer.create 256 in
  let line k v = if v <> "" then Buffer.add_string b (k ^ " " ^ v ^ "\n") in
  line "kind" o.kind;
  line "status" (Option.value o.status ~default:"");
  line "ternary" o.ternary;
  line "hash" o.hash;
  if o.status <> None then line "steps" (string_of_int o.steps);
  line "program" (Option.value o.program_hash ~default:"");
  List.iter (fun (n, h) -> line "lock" (n ^ " " ^ h)) o.lock;
  List.iter (fun (n, t) -> line "def" (n ^ " " ^ t)) o.dict_rows;
  line "note" o.note;
  Buffer.contents b
