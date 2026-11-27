(* Tuna_repl.Repl_core: the REPL command grammar and compile-with-lock,
   shared by the server REPL (Tuna_server.Repl_cmd re-exports it) and
   the local REPL (borg/local.borg L2) - one implementation, no copy.
   Pure: no store, no monad.  Grammar documented in
   server/lib/repl_cmd.ml. *)


type command =
  | Eval of string  (* term source *)
  | Def of string * string  (* name, term *)
  | Undef of string
  | Get of string * string option  (* path, optional program hash *)
  | Patch of string * string * string option  (* path, new ternary, target *)
  | FirstDiff of string * string
  | Dict
  | DictVisibility of string option * bool
    (* (target identity, public?); None = the caller's own dictionary *)

exception Parse_error of string

let parse_error fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let is_term_source s =
  String.length s > 0
  && (s.[0] = '(' || s.[0] = '%' || s.[0] = '[' || s.[0] = '{' || s.[0] = '"'
     || (s.[0] >= '0' && s.[0] <= '9')
     || (s.[0] = '-' && String.length s > 1 && s.[1] >= '0' && s.[1] <= '9'))

let parse_command (line : string) : command =
  let trimmed = String.trim line in
  let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' trimmed) in
  match words with
  | [] -> parse_error "empty command"
  | [ "dict" ] -> Dict
  | "undef" :: rest -> (
      match rest with
      | [ name ] when Tuna_compiler.Sexp.is_var_atom name -> Undef name
      | [ name ] -> parse_error "undef: bad name %S" name
      | _ -> parse_error "usage: undef <name>")
  | "def" :: rest -> (
      match rest with
      | name :: term_words when Tuna_compiler.Sexp.is_var_atom name ->
          Def (name, String.concat " " term_words)
      | [ _ ] ->
          parse_error "usage: def <name> <term> (name must be an identifier)"
      | _ -> parse_error "usage: def <name> <term>")
  | "eval" :: rest ->
      let src = String.concat " " rest in
      if src = "" then parse_error "usage: eval <term>" else Eval src
  | "get" :: rest -> (
      match rest with
      | [ path ] -> Get (path, None)
      | [ path; hash ] -> Get (path, Some hash)
      | _ -> parse_error "usage: get <path> [program-hash]")
  | "patch" :: rest -> (
      match rest with
      | [ path; new_ternary ] -> Patch (path, new_ternary, None)
      | [ path; new_ternary; hash ] -> Patch (path, new_ternary, Some hash)
      | _ -> parse_error "usage: patch <path> <new_ternary> [program-hash]")
  | "first-diff" :: rest -> (
      match rest with
      | [ a; b ] -> FirstDiff (a, b)
      | _ -> parse_error "usage: first-diff <hashA> <hashB>")
  | "dict-visibility" :: rest -> (
      match rest with
      | [ ("public" | "private") as v ] -> DictVisibility (None, v = "public")
      | [ target; ("public" | "private") as v ] ->
          DictVisibility (Some target, v = "public")
      | _ ->
          parse_error
            "usage: dict-visibility [<identity>] public|private")
  | _ when is_term_source trimmed -> Eval trimmed
  | _ ->
      parse_error
        "unknown command (try: eval / def / undef / get / patch / \
         first-diff / dict / dict-visibility)"

(* compile + the dictionary-usage census: every dictionary hit fires
   on_resolve (def-usage, borg/call-sites.borg cousin).  The collector
   rides the parse; the aggregate lands in the program row's ir JSON
   (resolved_defs, name -> occurrence count) so /api/defs/usage can
   answer 'which defs does the town speak, and who speaks them'
   without a second write path. *)
(* tool-language T2: the lock - each resolved dictionary name with the
   hash of the tree it read as, under the compiler's own last-wins
   lookup, so an author can pin exactly what a compile used. *)
let lock_of ~(dictionary : (string * Tuna.Tree.t) list)
    (counts : (string * int) list) : (string * string) list =
  let tbl = Hashtbl.create 64 in
  List.iter (fun (n, t) -> Hashtbl.replace tbl n t) dictionary;
  List.filter_map
    (fun (name, _) ->
      Option.map
        (fun t -> (name, Tuna.Hash.hex_of_tree t))
        (Hashtbl.find_opt tbl name))
    counts
  |> List.sort compare

let compile_with_dict ?(fuel = Tuna_compiler.Bracket.default_compile_fuel)
    ?(size_cap = Tuna_compiler.Bracket.default_compile_size_cap) ?(deadline = Float.infinity)
    ~dictionary src =
  let resolved = ref [] in
  let artifact =
    Tuna_compiler.Bracket.compile_source ~fuel ~size_cap ~deadline ~dictionary
      ~on_resolve:(fun name off -> resolved := (name, off) :: !resolved)
      src
  in
  (* fold (name, off) into name -> count, sorted by count then name *)
  let counts =
    List.fold_left
      (fun acc (name, _) ->
        match List.assoc_opt name acc with
        | Some c -> List.remove_assoc name acc |> fun a -> (name, c + 1) :: a
        | None -> (name, 1) :: acc)
      [] !resolved
    |> List.sort (fun (a, ca) (b, cb) ->
           if ca <> cb then cb - ca else compare a b)
  in
  (artifact, counts, lock_of ~dictionary counts)

