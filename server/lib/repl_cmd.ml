(* Tuna_server.Repl_cmd: the REPL engine (M9).

   One round = one command line executed against a store pool, under an
   identity.  The identity's name->tree dictionary (repl_dict) is read
   into the compiler, so a round is compile+eval against the
   dictionary — compile IS reduction still holds: dictionary-bound
   names are read as tree literals at parse time (capture-safe: a
   shadowing lambda parameter wins, since it is already in scope).

   Journaled rounds (eval, def) are FIRST-CLASS RUNS: the compiled
   artifact is upserted as a program and executed through the SAME
   Run.execute_run boundary as POST /api/runs — live grant checks, the
   prim boundary, the journal.  Each round links parent_run_id to the
   identity's previous journaled round (repl_state.last_run_id), so the
   REPL transcript is a walkable run-parent chain per session
   (SPEC.md §5: "the transcript is itself a run row").

   Structural commands (get / patch / first-diff / dict / undef) are
   store queries: nothing computes, so they journal no rows — but patch
   still produces a new immutable program row (old rows are history).

   Command grammar:
     eval <term>                        journaled round on the term
                                        (inputs come from the caller)
     def <name> <term>                  compile + store under name
                                        (journaled: result = the tree)
     undef <name>
     get <path> [hash]                  subtree at tree path of the last
                                        round's result (or a program)
     patch <path> <new_ternary> [hash]  structural replace on the last
                                        round's result (or a program)
     first-diff <hashA> <hashB>         first differing tree path
     dict                               list the dictionary
     <term>                             (no keyword) = eval

   Paths use the canonical tree-path convention (0 = stem child,
   1 = fork left, 2 = fork right; leading '/' tolerated; "" = root),
   the same digits as the patch API and provenance tags. *)

open Lwt.Infix

module J = Yojson.Basic
module S = Tuna_store.Store
module B = Tuna_compiler.Bracket
module P = Patch

(* The ir column (provenance-lite: tree path -> IR node id + span) for
   a compiled artifact.  LIVES HERE (not Api) so Run-boundary consumers
   don't pull the Api module in — Api delegates to this one. *)
let ir_json_of_artifact (a : B.artifact) : J.t =
  let span_of_id id =
    match Tuna_compiler.Ir.find_id a.B.ir id with
    | None -> None
    | Some ip -> (
        match Tuna_compiler.Ir.at_path a.B.ir ip with
        | None -> None
        | Some n ->
            let { Tuna_compiler.Ir.off; len } = Tuna_compiler.Ir.span_of n in
            Some (`Assoc [ ("off", `Int off); ("len", `Int len) ]))
  in
  `Assoc
    [ ("kind", `String "compiled")
    ; ("steps", `Int a.B.steps)
    ; ( "tags"
      , `List
          (List.map
             (fun (path, id) ->
               `Assoc
                 [ ( "path"
                   , `String (String.concat "" (List.map string_of_int path)) )
                 ; ("ir", `Int id)
                 ; ("span", (match span_of_id id with Some s -> s | None -> `Null)) ])
             a.B.tags)) ]

(* -- command parsing (pure) ------------------------------------------ *)

type command =
  | Eval of string  (* term source *)
  | Def of string * string  (* name, term *)
  | Undef of string
  | Get of string * string option  (* path, optional program hash *)
  | Patch of string * string * string option  (* path, new ternary, target *)
  | FirstDiff of string * string
  | Dict

exception Parse_error of string

let parse_error fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let is_term_source s =
  String.length s > 0 && (s.[0] = '(' || s.[0] = '%')

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
  | _ when is_term_source trimmed -> Eval trimmed
  | _ ->
      parse_error
        "unknown command (try: eval / def / undef / get / patch / \
         first-diff / dict)"

(* -- outcome (structured; both surfaces render it) ------------------- *)

type outcome = {
  o_kind : string  (* eval|def|undef|get|patch|first-diff|dict *)
; o_status : string option  (* journaled rounds: normal|fuel_exhausted|... *)
; o_ternary : string  (* result / subtree / path ("": none) *)
; o_hash : string
; o_steps : int
; o_run_id : string option
; o_program_hash : string option
; o_note : string
; o_dict_rows : (string * string) list  (* dict listing: name -> ternary *)
}

let outcome ?status ?run_id ?program_hash ?note ?dict_rows ?ternary ?hash
    ?(steps = 0) ~kind () =
  let opt_str = function Some s -> s | None -> "" in
  { o_kind = kind
  ; o_status = status
  ; o_ternary = opt_str ternary
  ; o_hash = opt_str hash
  ; o_steps = steps
  ; o_run_id = run_id
  ; o_program_hash = program_hash
  ; o_note = opt_str note
  ; o_dict_rows = Option.value dict_rows ~default:[] }

type round_result = (outcome, int * string) result

let err code msg : round_result Lwt.t = Lwt.return (Error (code, msg))
let err_ msg : round_result Lwt.t = Lwt.return (Error (400, msg))

(* -- journaled rounds (eval / def) ------------------------------------ *)

let compile_with_dict ?(fuel = B.default_compile_fuel)
    ?(size_cap = B.default_compile_size_cap) ?(deadline = Float.infinity)
    ~dictionary src =
  B.compile_source ~fuel ~size_cap ~deadline ~dictionary src

(* Compile the artifact, upsert it, run it through the run boundary,
   and chain the run row to the identity's previous round. *)
let journaled_round pool ~caller ~artifact ~input_trees ~grant_ids ~fuel
    ~size_cap () =
  let ir = J.to_string (ir_json_of_artifact artifact) in
  S.upsert_program pool ~hash:artifact.B.hash_hex ~ternary:artifact.B.ternary
    ~ir:(Some ir) ~created_by:(Some caller)
  >>= fun _ ->
  S.repl_state_get pool ~identity_id:caller
  >>= fun parent ->
  Run.execute_run pool ~caller ~program_hash:artifact.B.hash_hex
    ~input_trees ~grant_ids ~parent_run_id:parent ~fuel ~size_cap ()
  >>= (function
        | Error (code, msg) -> Lwt.return (Error (code, msg))
        | Ok (row, js) ->
            S.repl_state_put pool ~identity_id:caller ~run_id:row.S.r_id
            >>= fun () ->
            Lwt.return (Ok (row, js, artifact.B.hash_hex)))

let run_outcome kind (row : S.run) program_hash =
  outcome ~kind ~status:(S.Run_status.to_string row.S.r_status)
    ~run_id:row.S.r_id ~program_hash
    ?ternary:row.S.r_result_ternary
    ?hash:row.S.r_result_hash
    ~steps:(Option.value row.S.r_step_count ~default:0)
    ()

let do_eval pool ~caller ~grant_ids ~dictionary ~input_trees ~fuel ~size_cap
    ?(compile_fuel = B.default_compile_fuel)
    ?(compile_size_cap = B.default_compile_size_cap)
    ?(compile_deadline = Float.infinity) src =
    match
      compile_with_dict ~fuel:compile_fuel ~size_cap:compile_size_cap
        ~deadline:compile_deadline ~dictionary src
    with
    | exception Tuna_compiler.Ir.Error (p, msg) ->
        err_ ("compile error: " ^ Tuna_compiler.Ir.show_error (p, msg))
    | exception B.Compile_failed msg -> err_ ("compile failed: " ^ msg)
    | artifact -> (
        journaled_round pool ~caller ~artifact ~input_trees ~grant_ids ~fuel
          ~size_cap ()
        >>= (function
              | Error (code, msg) -> Lwt.return (Error (code, msg))
              | Ok (row, _js, phash) ->
                  Lwt.return (Ok (run_outcome "eval" row phash))))

let do_def pool ~caller ~dictionary
    ?(compile_fuel = B.default_compile_fuel)
    ?(compile_size_cap = B.default_compile_size_cap)
    ?(compile_deadline = Float.infinity) name src =
  match
    compile_with_dict ~fuel:compile_fuel ~size_cap:compile_size_cap
      ~deadline:compile_deadline ~dictionary src
  with
  | exception Tuna_compiler.Ir.Error (p, msg) ->
      err_ ("compile error: " ^ Tuna_compiler.Ir.show_error (p, msg))
  | exception B.Compile_failed msg -> err_ ("compile failed: " ^ msg)
  | artifact -> (
      S.dict_set pool ~identity_id:caller ~name ~ternary:artifact.B.ternary
      >>= fun () ->
      journaled_round pool ~caller ~artifact ~input_trees:[] ~grant_ids:[]
        ~fuel:1_000_000 ~size_cap:1_000_000 ()
      >>= (function
            | Error (code, msg) -> Lwt.return (Error (code, msg))
            | Ok (row, _js, phash) ->
                Lwt.return
                  (Ok
                     ({ (run_outcome "def" row phash) with
                        o_ternary = artifact.B.ternary
                      ; o_hash = artifact.B.hash_hex
                      ; o_note = Printf.sprintf "defined %s" name }))))

(* -- structural commands ---------------------------------------------- *)

(* The last journaled round's result tree: what get/patch address when
   no program hash is given. *)
let last_result_tree pool ~caller =
  S.repl_state_get pool ~identity_id:caller
  >>= (function
        | None -> Lwt.return (Error "no previous round yet — eval something first")
        | Some run_id -> (
            S.fetch_run pool run_id >>= function
            | None -> Lwt.return (Error "repl_state points at a vanished run")
            | Some row -> (
                match row.S.r_result_ternary with
                | None ->
                    Lwt.return
                      (Error (Printf.sprintf "last round (%s) produced no result tree" run_id))
                | Some t -> (
                    match Tuna.Canon.of_string t with
                    | Error (off, msg) ->
                        Lwt.return
                          (Error
                             (Printf.sprintf
                                "stored result unparseable at offset %d: %s" off
                                msg))
                    | Ok tree ->
                        Lwt.return (Ok (tree, row.S.r_result_hash))))))

let program_tree pool h =
  S.fetch_program pool h >>= function
  | None -> Lwt.return (Error (Printf.sprintf "unknown program hash %s" h))
  | Some prog -> (
      match Tuna.Canon.of_string prog.S.p_ternary with
      | Error (off, msg) ->
          Lwt.return
            (Error
               (Printf.sprintf "stored program unparseable at offset %d: %s" off
                  msg))
      | Ok t -> Lwt.return (Ok (t, Some prog.S.p_hash)))

let do_get pool ~caller path hash_opt =
  match P.normalize_path path with
  | None -> err_ (Printf.sprintf "bad path %S" path)
  | Some p -> (
      let base =
        match hash_opt with
        | None -> last_result_tree pool ~caller
        | Some h -> program_tree pool h
      in
      base >>= function
      | Error msg -> err_ msg
      | Ok (t, phash) -> (
          match P.at_path t (P.digits p) with
          | None -> err_ (Printf.sprintf "path %S escapes the tree" path)
          | Some sub ->
              let ternary = Tuna.Canon.encode sub in
              Lwt.return
                (Ok
                   (outcome ~kind:"get" ~ternary ~hash:(Tuna.Hash.hex_of_tree sub)
                      ?program_hash:phash
                      ~note:(Printf.sprintf "subtree at path %s" p)
                      ()))))

(* Structural replace on the last round's result (or a named program).
   The expected-old-hash is pinned by the subtree we hold in hand — CAS
   holds unless the store changed under us.  Produces a NEW program row
   (the old row stays as immutable history). *)
let do_patch pool ~caller path new_ternary hash_opt =
  match P.normalize_path path with
  | None -> err_ (Printf.sprintf "bad path %S" path)
  | Some p -> (
      (* accept both the surface literal (%0) and the bare ternary (0) *)
      let t = String.trim new_ternary in
      let bare =
        if String.length t > 0 && t.[0] = '%' then String.sub t 1 (String.length t - 1)
        else t
      in
      match Tuna.Canon.of_string bare with
      | Error (off, msg) ->
          err_
            (Printf.sprintf "new_ternary parse error at offset %d: %s" off msg)
      | Ok new_sub -> (
          let base =
            match hash_opt with
            | None -> last_result_tree pool ~caller
            | Some h -> program_tree pool h
          in
          base >>= function
          | Error msg -> err_ msg
          | Ok (t, phash) -> (
              match P.at_path t (P.digits p) with
              | None -> err_ (Printf.sprintf "path %S escapes the tree" path)
              | Some sub -> (
                  match
                    P.apply_patch ~old_tree:t ~path:p ~believed:None
                      ~expected_old_hash:(Tuna.Hash.hex_of_tree sub) ~new_sub
                  with
                  | P.Bad_path _ -> err_ "bad path (unreachable)"
                  | P.Conflict _ ->
                      err_ "patch conflict: store changed under us"
                  | P.Applied { ternary; hash } ->
                      S.upsert_program pool ~hash ~ternary ~ir:None
                        ~created_by:(Some caller)
                      >>= fun _ ->
                      Lwt.return
                        (Ok
                           (outcome ~kind:"patch" ~ternary ~hash
                              ?program_hash:phash
                              ~note:
                                (Printf.sprintf "patched path %s: %s -> %s" p
                                   (Tuna.Canon.encode sub)
                                   ternary)
                              ()))))))

let do_first_diff pool a b =
  S.fetch_program pool a >>= fun pa ->
  S.fetch_program pool b >>= fun pb ->
  (match (pa, pb) with
   | None, _ -> err_ (Printf.sprintf "unknown program hash %s" a)
   | _, None -> err_ (Printf.sprintf "unknown program hash %s" b)
   | Some pa, Some pb -> (
       match
         (Tuna.Canon.of_string pa.S.p_ternary, Tuna.Canon.of_string pb.S.p_ternary)
       with
       | Error (off, msg), _ | _, Error (off, msg) ->
           err_
             (Printf.sprintf "stored program unparseable at offset %d: %s" off
                msg)
       | Ok ta, Ok tb -> (
           match P.first_diff ta tb with
           | None ->
               Lwt.return
                 (Ok
                    (outcome ~kind:"first-diff"
                       ~note:
                         (Printf.sprintf "no structural difference between %s and %s"
                            a b)
                       ~hash:a ()))
           | Some d -> (
               let sub_of t h =
                 match P.at_path t (P.digits d) with
                 | Some s -> Tuna.Hash.hex_of_tree s
                 | None -> h (* identical ancestors; whole-tree hash *)
               in
               Lwt.return
                 (Ok
                    (outcome ~kind:"first-diff"
                       ~ternary:d ~hash:d
                       ~note:
                         (Printf.sprintf
                            "first difference at path %s: %s (%s) vs %s (%s)"
                            d a
                            (sub_of ta (Tuna.Hash.hex_of_tree ta))
                            b
                            (sub_of tb (Tuna.Hash.hex_of_tree tb)))
                       ()))))))

let do_dict pool ~caller =
  S.dict_list pool ~identity_id:caller
  >>= fun rows ->
  Lwt.return
    (Ok
       (outcome ~kind:"dict"
          ~dict_rows:(List.map (fun d -> (d.S.d_name, d.S.d_ternary)) rows)
          ~note:
            (if rows = [] then "dictionary is empty" else "dictionary entries")
          ()))

let do_undef pool ~caller name =
  S.dict_del pool ~identity_id:caller ~name >>= fun () ->
  Lwt.return (Ok (outcome ~kind:"undef" ~note:("undefined " ^ name) ()))

(* -- the round -------------------------------------------------------- *)

(* Parse the caller's input ternaries (one per element, as in the run
   API).  First error wins. *)
let parse_inputs (inputs : string list) :
    (Tuna.Tree.t list, string) result =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> (
        match Tuna.Canon.of_string (String.trim s) with
        | Ok t -> go (t :: acc) rest
        | Error (off, msg) ->
            Error (Printf.sprintf "input parse error at offset %d: %s" off msg))
  in
  go [] (List.filter (fun s -> String.trim s <> "") inputs)

(* Read the identity's dictionary, parse the command, execute it.
   [inputs] / [grant_ids] / [fuel] / [size_cap] apply to eval rounds;
   [compile_fuel] / [compile_size_cap] / [compile_deadline] bound the
   compile-time reduction phase (defaults: bracket.ml's, untimed — the
   server passes the TUNA_COMPILE_MAX_SECONDS clock).  Errors are
   values, never exceptions. *)
let execute pool ~caller ~command ~inputs ~grant_ids ~fuel ~size_cap
    ?(compile_fuel = B.default_compile_fuel)
    ?(compile_size_cap = B.default_compile_size_cap)
    ?(compile_deadline = Float.infinity) () :
    round_result Lwt.t =
  Lwt.catch
    (fun () ->
      S.dict_list pool ~identity_id:caller
      >>= fun rows ->
      (match
         List.fold_left
           (fun acc d ->
             match acc with
             | Error e -> Error e
             | Ok acc -> (
                 match Tuna.Canon.of_string d.S.d_ternary with
                 | Ok t -> Ok ((d.S.d_name, t) :: acc)
                 | Error (off, msg) ->
                     Error
                       (Printf.sprintf
                          "dictionary entry %s unparseable at offset %d: %s"
                          d.S.d_name off msg)))
           (Ok []) rows
       with
       | Error msg -> err_ msg
       | Ok dictionary ->
      match parse_command command with
      | exception Parse_error msg -> err_ msg
        | Eval src -> (
            (match parse_inputs inputs with
             | Error msg -> err_ msg
             | Ok input_trees ->
                 do_eval pool ~caller ~grant_ids ~dictionary ~input_trees ~fuel
                   ~size_cap ~compile_fuel ~compile_size_cap ~compile_deadline
                   src))
        | Def (name, src) ->
            do_def pool ~caller ~dictionary ~compile_fuel ~compile_size_cap
              ~compile_deadline name src
      | Undef name -> do_undef pool ~caller name
      | Get (path, h) -> do_get pool ~caller path h
      | Patch (path, t, h) -> do_patch pool ~caller path t h
      | FirstDiff (a, b) -> do_first_diff pool a b
      | Dict -> do_dict pool ~caller))
    (fun exn ->
       let msg =
         match exn with
         | S.Store_error s -> "store error: " ^ s
         | Tuna_compiler.Ir.Error (p, msg) ->
             "compile error: " ^ Tuna_compiler.Ir.show_error (p, msg)
         | B.Compile_failed msg -> "compile failed: " ^ msg
         | e -> Printexc.to_string e
       in
       err_ msg)
