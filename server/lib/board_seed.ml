(* Tuna_server.Board_seed — the board chapter's program tier
   (borg/board.borg laws L2-L4).

   OWNER: a dedicated non-admin identity `board`, same spelling as
   Stdlib_seed's sabralib (token via TUNA_BOARD_TOKEN, generated and
   printed ONCE otherwise; only its sha256 is stored).

   The four member programs (board-add/view/flip/del) live as
   scripts/dialect/*.sabra - the BOOK's artifacts - embedded by a dune
   rule (Board_embed, generated from those files; never hand-edited).
   Boot seeds each as an ORDINARY def round (the execute callback IS
   Repl_cmd.execute, injected from api.ml: no cycle), so every program
   lands as a program row (hash-citable, quads attached), a repl_dict
   row, and a journaled run attributed to the board identity.

   IDEMPOTENCE: unlike sabralib's skip-if-exists (additive-only), the
   product tier re-defs when the source CHANGED - a stale dict row
   whose stored program source differs from the embedded source is
   quietly replaced; boots with unchanged sources journal nothing.
   The page resolves programs BY NAME (program_hash below), so a
   re-def flips the live behavior on the next request without any page
   code change: programs as data, the product way. *)

open Tuna_store.Direct
module S = Tuna_store.Store

let identity_name = "board"
let token_env = "TUNA_BOARD_TOKEN"

let ensure_identity pool =
  S.fetch_identity_by_name pool identity_name >>= function
  | Some i -> return i
  | None ->
      let token, generated =
        match Sys.getenv_opt token_env with
        | Some t when t <> "" -> (t, false)
        | _ -> (Tokens.random_token_hex (), true)
      in
      S.bootstrap_identity pool ~is_admin:false ~name:identity_name
        ~token ()
      >>= fun i ->
      if generated then (
        print_string (token_env ^ "=" ^ token ^ "\n\n");
        flush stdout);
      Web.log "boot: created board identity %s" i.S.i_id;
      return i

let seed pool ~execute () =
  ensure_identity pool >>= fun i ->
  S.dict_list pool ~identity_id:i.S.i_id >>= fun existing ->
  let by_name d = (d.S.d_name, d.S.d_ternary) in
  let have = List.map by_name existing in
  let srcs = List.map (fun (n, s) -> (n, String.trim s)) Board_embed.programs in
  (* [needs]: no row yet, or the stored program's retained source (0015)
     differs from the embedded source.  The ternary hash is the join. *)
  let needs name src =
    match List.assoc_opt name have with
    | None -> return true
    | Some tern ->
        S.fetch_program pool (Tuna.Hash.hex_of_string tern)
        >>= (function
              | Some p -> (
                  match p.S.p_source with
                  | Some stored -> return (String.trim stored <> src)
                  | None -> return true)
              | None -> return true)
  in
  let rec go = function
    | [] -> return ()
    | (name, src) :: rest ->
        needs name src >>= fun stale ->
        if not stale then go rest
        else
          execute ~caller:i.S.i_id ~command:("def " ^ name ^ " " ^ src)
          >>= fun _ -> go rest
  in
  go srcs

let boot pool ~execute () = seed pool ~execute ()

(* the page rim resolves a member program by name: the CURRENT dict
   row's ternary hash is the program hash (def rounds store programs
   content-addressed; Run.execute_run fetches by that hash). *)
let program_hash pool ~name : (string, string) result =
  ensure_identity pool >>= fun i ->
  S.dict_list pool ~identity_id:i.S.i_id >>= fun rows ->
  match List.find_opt (fun d -> d.S.d_name = name) rows with
  | None -> return (Error ("board program " ^ name ^ " is not seeded"))
  | Some d -> return (Ok (Tuna.Hash.hex_of_string d.S.d_ternary))
