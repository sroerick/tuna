(* sabra stdlib v1 seeding + dictionary assembly
   (borg/stdlib.borg section stdlib-v1 subsection seeding).

   OWNER: a dedicated non-admin identity sabralib (Stdlib_embed carries
   the generated record list; the transcript grammar is the REPL's own,
   no second grammar).  Booted like fed peers: TUNA_STDLIB_TOKEN if
   supplied, else generated and printed ONCE (only its sha256 is
   stored; PP bootstrap law).

   SEEDING replays each def record through the ORDINARY def round -
   the execute callback IS Repl_cmd.execute, INJECTED from api.ml so
   this module never imports repl machinery (no cycle with the
   dictionary assembly below).  Every stdlib entry therefore lands as
   a program row (hash-citable, IR + provenance attached), a
   repl_dict row, and a journaled run attributed to sabralib,
   parent-chained in file order.  ADDITIVE: skip-if-exists, never
   overwrite.

   RESOLUTION ORDER (parse time; the ONLY new host behavior):
   lambda param > identity repl_dict > sabralib repl_dict > reader
   builtins.  Repl_cmd assembles the effective dictionary as
   std-rows-under-identity-rows (the compiler folds the assoc list
   into a Hashtbl where the LAST of a name wins).  An identity that
   pins its own `not` shadows std locally; undef restores it.  The
   compiler is untouched. *)

open Lwt.Infix
module S = Tuna_store.Store

let identity_name = Stdlib_embed.identity_name
let token_env = "TUNA_STDLIB_TOKEN"

(* The std rows that sit UNDER a caller's own dictionary.  Empty when
   the caller IS sabralib (its own rows are the std rows) or sabralib
   has not been booted yet (pre-seed boot ordering). *)
let dict_rows_under pool ~caller : S.dict_entry list Lwt.t =
  S.fetch_identity_by_name pool identity_name >>= function
  | None -> Lwt.return []
  | Some i when i.S.i_id = caller -> Lwt.return []
  | Some i -> S.dict_list pool ~identity_id:i.S.i_id

let ensure_identity pool =
  S.fetch_identity_by_name pool identity_name >>= function
  | Some i -> Lwt.return i
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
        print_string (token_env ^ "=" ^ token ^ "\n");
        flush stdout);
      Dream.log "boot: created sabralib identity %s" i.S.i_id;
      Lwt.return i

let seed pool ~execute () =
  ensure_identity pool >>= fun i ->
  S.dict_list pool ~identity_id:i.S.i_id >>= fun existing ->
  let have = List.map (fun d -> d.S.d_name) existing in
  Lwt_list.iter_s
    (fun (name, src) ->
      if List.mem name have then Lwt.return ()
      else
        execute ~caller:i.S.i_id ~command:("def " ^ name ^ " " ^ src)
        >>= function
        | Ok () -> Lwt.return ()
        | Error msg ->
            Lwt.fail (Failure (Printf.sprintf "stdlib seed: def %s: %s" name msg)))
    Stdlib_embed.defs
  >>= fun () ->
  Dream.log "boot: sabralib seeded (%d defs, %d pre-existing)"
    (List.length Stdlib_embed.defs) (List.length have);
  Lwt.return ()

(* Explicit reseed (admin escape hatch; PP's reseed-package carried
   over): touches ONLY the sabralib dictionary.  Undefs names that
   left the vocabulary, then replays every def round - content-address
   makes same-content rounds no-ops on the program side, fresh
   journaled rounds on the run side. *)
let reseed pool ~execute () =
  ensure_identity pool >>= fun i ->
  S.dict_list pool ~identity_id:i.S.i_id >>= fun existing ->
  let current = List.map fst Stdlib_embed.defs in
  Lwt_list.iter_s
    (fun d ->
      if List.mem d.S.d_name current then Lwt.return ()
      else S.dict_del pool ~identity_id:i.S.i_id ~name:d.S.d_name)
    existing
  >>= fun () ->
  Lwt_list.iter_s
    (fun (name, src) ->
      execute ~caller:i.S.i_id ~command:("def " ^ name ^ " " ^ src)
      >>= function
      | Ok () -> Lwt.return ()
      | Error msg ->
          Lwt.fail
            (Failure (Printf.sprintf "stdlib reseed: def %s: %s" name msg)))
    Stdlib_embed.defs

let boot pool ~execute () = seed pool ~execute ()
