(* Tuna HTTP API (M6): the JSON agent surface.

   Routes (all /api/* require Authorization: Bearer <identity token>;
   /health is open):

     POST /api/programs                     {ternary} | {source}
     GET  /api/programs/:hash
     POST /api/programs/:hash/patch         CAS structural patch
     POST /api/runs                         synchronous v0-style run
     GET  /api/runs?caller=&program=        newest-first list
     GET  /api/runs/:id                     row + journal
     GET  /api/journals/:run_id
     POST /api/journals/:run_id/fork        derived (counterfactual) journal

   Identity bootstrap is the server binary's job (bin/main.ml): read
   TUNA_BOOTSTRAP_TOKEN or generate one, print it, insert the root
   identity via Store.bootstrap_identity.  Every API request is then
   attributable: the bearer token resolves to an identity row and run
   rows carry its id (SPEC.md §3 Auth).

   v0 run semantics: POST /api/runs evaluates synchronously with the
   M2 stepper, inserts the run row first and updates it with the
   result (status/result_ternary/step_count).  Pure tree programs
   journal nothing; the prim boundary (grants checked per call,
   journaled) lands in M7 — the grants[] array on POST /api/runs is
   already validated against the grants table (exists, unrevoked,
   belongs to caller) and is what the M7 boundary will consume.

   Style note: handlers chain with `>>= fun x ->` / `>>= function` in
   tail position — no parenthesized-match pyramids. *)

open Lwt.Infix

module J = Yojson.Basic
module JU = J.Util
module P = Patch
module B = Tuna_compiler.Bracket
module Db = Tuna_store.Db
module Store = Tuna_store.Store

type auth = { auth_id : string; auth_name : string; auth_is_admin : bool }

(* -- json helpers ---------------------------------------------------- *)

let j_ok ?(code = 200) j = Dream.json ~code (J.to_string j)

let j_err ?(code = 400) msg =
  Dream.json ~code (J.to_string (`Assoc [ ("error", `String msg) ]))

let member_opt k (j : J.t) : J.t option =
  match JU.member k j with `Null -> None | v -> Some v

let get_string_opt j k =
  match member_opt k j with Some (`String s) -> Some s | _ -> None

let get_int_opt j k = match member_opt k j with Some (`Int i) -> Some i | _ -> None

let get_list j k : J.t list =
  match member_opt k j with Some (`List l) -> l | _ -> []

let strings_of j k =
  List.filter_map (function `String s -> Some s | _ -> None) (get_list j k)

let body_json req =
  Dream.body req >>= fun body ->
  match J.from_string body with
  | j -> Lwt.return (Ok j)
  | exception Yojson.Json_error _ -> Lwt.return (Error "invalid JSON body")

let parse_ternary s =
  match Tuna.Canon.of_string s with
  | Ok t -> Ok t
  | Error (off, msg) ->
      Error (Printf.sprintf "ternary parse error at offset %d: %s" off msg)

let opt_str = function Some s -> `String s | None -> `Null
let opt_int = function Some i -> `Int i | None -> `Null

(* -- auth ------------------------------------------------------------ *)

let bearer_token req =
  match Dream.header req "Authorization" with
  | Some h when String.length h > 7
                && String.equal
                     (String.lowercase_ascii (String.sub h 0 7))
                     "bearer " ->
      Some (String.trim (String.sub h 7 (String.length h - 7)))
  | _ -> None

let authenticate pool req =
  match bearer_token req with
  | None -> Lwt.return None
  | Some token ->
      Store.verify_token pool token
      >>= function
      | None -> Lwt.return None
      | Some i ->
          Lwt.return
            (Some
               { auth_id = i.Store.i_id
               ; auth_name = i.Store.i_name
               ; auth_is_admin = i.Store.i_is_admin })

(* 5xx guard: domain/store errors become a JSON 500 with the message;
   Dream's default handler would otherwise answer html. *)
let guard p =
  Lwt.catch (fun () -> p) (fun exn ->
      let msg =
        match exn with
        | Store.Store_error s -> "store error: " ^ s
        | e -> Printexc.to_string e
      in
      Dream.log "handler error: %s" msg;
       (j_err ~code:500 msg))

let with_auth pool handler req =
  authenticate pool req
  >>= function
  | None ->
       (j_err ~code:401 "unauthorized: valid bearer token required")
  | Some auth -> guard (handler auth req)

(* -- row json -------------------------------------------------------- *)

let run_json (r : Store.run) : J.t =
  `Assoc
    [ ("id", `String r.r_id)
    ; ("program_hash", `String r.r_program_hash)
    ; ("input_hashes", `List (List.map (fun s -> `String s) r.r_input_hashes))
    ; ("fuel", `Int r.r_fuel)
    ; ("size_cap", `Int r.r_size_cap)
    ; ("result_hash", opt_str r.r_result_hash)
    ; ("result_ternary", opt_str r.r_result_ternary)
    ; ("step_count", opt_int r.r_step_count)
    ; ("status", `String (Store.Run_status.to_string r.r_status))
    ; ("caller", opt_str r.r_caller)
    ; ("parent_run_id", opt_str r.r_parent_run_id)
    ; ("verify_status", opt_str r.r_verify_status)
    ; ("created_at", opt_str r.r_created_at) ]

let journal_json (j : Store.journal) : J.t =
  `Assoc
    [ ("run_id", `String j.j_run_id)
    ; ("seq", `Int j.j_seq)
    ; ("callsite_path", `String j.j_callsite_path)
    ; ("prim", `String j.j_prim)
    ; ("prim_contract", `String j.j_prim_contract)
    ; ("grant_id", opt_str j.j_grant_id)
    ; ("args_ternary", opt_str j.j_args_ternary)
    ; ("args_hash", opt_str j.j_args_hash)
    ; ("result_ternary", opt_str j.j_result_ternary)
    ; ("result_hash", opt_str j.j_result_hash)
    ; ("error", opt_str j.j_error)
    ; ("wall_ms", opt_int j.j_wall_ms)
    ; ("host_build", `String j.j_host_build)
    ; ("prev_hash", `String j.j_prev_hash)
    ; ("row_hash", `String j.j_row_hash) ]

(* ir column for compiled programs: provenance (tree path -> IR node
   id + IR path span) — the diagnostics join for journal rows and
   divergence reports (call-sites.provenance). *)
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
             a.B.tags) ) ]

let program_json (p : Store.program) : J.t =
  let size =
    match Tuna.Canon.of_string p.p_ternary with
    | Ok t -> Tuna.Tree.size t
    | Error _ -> 0 (* never: rows are written from parsed trees *)
  in
  `Assoc
    [ ("hash", `String p.p_hash)
    ; ("ternary", `String p.p_ternary)
    ; ("size", `Int size)
    ; ("ir", (match p.p_ir with Some s -> J.from_string s | None -> `Null))
    ; ("created_by", opt_str p.p_created_by) ]

(* -- /health (open) -------------------------------------------------- *)

(* /health is open; db ping must not raise — a dead PG answers db:false *)
let health pool _req =
  Lwt.catch
    (fun () -> Db.ping pool >>= fun () -> Lwt.return true)
    (fun _ -> Lwt.return false)
  >>= fun up ->
   (j_ok (`Assoc [ ("status", `String "ok"); ("db", `Bool up) ]))

(* -- programs -------------------------------------------------------- *)

let post_program pool auth req =
  Dream.body req >>= fun body ->
  match J.from_string body with
  | exception Yojson.Json_error _ ->  (j_err "body must be a JSON object")
  | j ->
      let created_by = Some auth.auth_id in
      let ternary = get_string_opt j "ternary" in
      let source = get_string_opt j "source" in
      match (ternary, source) with
      | Some t, _ -> (
          match parse_ternary t with
          | Error msg ->  (j_err msg)
          | Ok _tree ->
              let hash = Tuna.Hash.hex_of_string t in
              Store.upsert_program pool ~hash ~ternary:t ~ir:None ~created_by
              >>= fun _row ->
               (j_ok ~code:201 (`Assoc [ ("hash", `String hash) ])))
      | None, Some src -> (
          match
            (try
               Ok (B.compile_source src)
             with
             | Tuna_compiler.Ir.Error (p, msg) ->
                 Error ("compile error: " ^ Tuna_compiler.Ir.show_error (p, msg))
             | B.Compile_failed msg -> Error ("compile failed: " ^ msg))
          with
          | Error msg ->  (j_err msg)
          | Ok art ->
              let ir = J.to_string (ir_json_of_artifact art) in
              let hash = art.B.hash_hex in
              Store.upsert_program pool ~hash ~ternary:art.B.ternary ~ir:(Some ir)
                ~created_by
              >>= fun _row ->
               (j_ok ~code:201 (`Assoc [ ("hash", `String hash) ])))
      | None, None ->
           (j_err "body must contain \"ternary\" or \"source\"")

let get_program pool _auth req =
  let hash = Dream.param req "hash" in
  Store.fetch_program pool hash
  >>= function
  | None ->  (j_err ~code:404 "unknown program hash")
  | Some p ->  (j_ok (program_json p))

let patch_program pool auth req =
  let hash = Dream.param req "hash" in
  Dream.body req >>= fun body ->
  match J.from_string body with
  | exception Yojson.Json_error _ ->  (j_err "body must be a JSON object")
  | j ->
      let path = get_string_opt j "path" in
      let expected = get_string_opt j "expected_old_hash" in
      let new_t = get_string_opt j "new_ternary" in
      let old_t = get_string_opt j "old_ternary" in
      match (path, expected, new_t) with
      | None, _, _ ->  (j_err "missing \"path\"")
      | _, None, _ ->  (j_err "missing \"expected_old_hash\"")
      | _, _, None ->  (j_err "missing \"new_ternary\"")
      | Some path, Some expected, Some new_t -> (
          match parse_ternary new_t with
          | Error msg ->  (j_err ("new_ternary: " ^ msg))
          | Ok new_sub ->
              let believed =
            match old_t with
            | None -> Ok None
            | Some s -> (
                match parse_ternary s with
                | Ok t -> Ok (Some t)
                | Error msg -> Error msg)
          in
          match believed with
          | Error msg ->  (j_err ("old_ternary: " ^ msg))
          | Ok believed ->
              Store.fetch_program pool hash
              >>= function
              | None ->  (j_err ~code:404 "unknown program hash")
              | Some prog -> (
                  match parse_ternary prog.p_ternary with
                  | Error msg ->
                      
                        (j_err ~code:500 ("stored program unparseable: " ^ msg))
                  | Ok old_tree -> (
                      match
                        P.apply_patch ~old_tree ~path ~believed
                          ~expected_old_hash:expected ~new_sub
                      with
                      | P.Bad_path _ ->
                          
                            (j_err ~code:404
                               (Printf.sprintf
                                  "path %S does not address a subtree" path))
                      | P.Conflict { expected_old_hash; actual_old_hash; first_diff }
                        ->
                          
                            (j_ok ~code:409
                               (`Assoc
                                 [ ( "error"
                                   , `String
                                       "patch conflict: subtree hash mismatch" )
                                 ; ("path", `String path)
                                 ; ("expected_old_hash", `String expected_old_hash)
                                 ; ("actual_old_hash", `String actual_old_hash)
                                 ; ( "first_diff"
                                   , (match first_diff with
                                      | Some s -> `String s
                                      | None -> `Null) ) ]))
                      | P.Applied { ternary; hash } ->
                          let created_by = Some auth.auth_id in
                          Store.upsert_program pool ~hash ~ternary ~ir:None
                            ~created_by
                          >>= fun _row ->
                          
                            (j_ok
                               (`Assoc
                                 [ ("hash", `String hash)
                                 ; ("ternary", `String ternary)
                                 ; ("patched_from", `String prog.p_hash) ])))))

(* -- runs ------------------------------------------------------------ *)

(* Up-front grant validation now lives in Run.execute_run (it re-uses
   Store.check_grant the same way); this module calls it directly. *)
let post_run pool auth req =
  body_json req >>= function
  | Error msg ->  (j_err msg)
  | Ok j ->
      let fuel = Option.value (get_int_opt j "fuel") ~default:10_000 in
      let size_cap = Option.value (get_int_opt j "size_cap") ~default:10_000 in
      match get_string_opt j "program_hash" with
      | None ->  (j_err "missing \"program_hash\"")
      | Some program_hash -> (
          let inputs_rev =
            List.fold_left
              (fun acc t ->
                match (acc, parse_ternary t) with
                | Ok acc, Ok t -> Ok (t :: acc)
                | Error e, _ | _, Error e -> Error e)
              (Ok [])
              (strings_of j "inputs")
          in
          match inputs_rev with
          | Error msg ->  (j_err msg)
          | Ok rev ->
              let input_trees = List.rev rev in
              Run.execute_run pool ~caller:auth.auth_id ~program_hash
                ~input_trees ~grant_ids:(strings_of j "grants") ~fuel
                ~size_cap ()
              >>= (function
                    | Error (code, msg) -> j_err ~code msg
                    | Ok (row, js) ->
                        (j_ok ~code:201
                           (`Assoc
                             [ ("run", run_json row)
                             ; ( "journal"
                               , `List (List.map journal_json js) )
                             ]))))

(* run-row level verify response; the divergence is addressed structure,
   not prose (replay.divergence-surface) *)
let verdict_json run_id (v : Replay.verdict) : J.t =
  match v with
  | Replay.Verified _ ->
      `Assoc [ ("run_id", `String run_id); ("verify", `String "verified") ]
  | Replay.Bad_chain msg ->
      `Assoc
        [ ("run_id", `String run_id)
        ; ("verify", `String "failed")
        ; ("reason", `String ("journal hash chain broken: " ^ msg)) ]
  | Replay.Diverged d ->
      `Assoc
        [ ("run_id", `String run_id)
        ; ("verify", `String "failed")
        ; ("reason", `String d.Replay.reason)
        ; ("divergence_seq", opt_int d.Replay.div_seq)
        ; ("callsite_path", `String d.Replay.callsite_path)
        ; ("prim", `String d.Replay.prim)
        ; ("first_diff_path", `String d.Replay.first_diff_path)
        ; ("recorded_hash", opt_str d.Replay.recorded_hash)
        ; ("replayed_hash", opt_str d.Replay.replayed_hash) ]
  | Replay.Unverifiable msg ->
      `Assoc
        [ ("run_id", `String run_id)
        ; ("verify", `String "unverifiable")
        ; ("reason", `String msg) ]

(* GET /api/runs/:id — auto-verify on fetch: an unverified finished run
   is replay-verified inline and the verdict recorded (verify_status). *)
let get_run pool _auth req =
  let id = Dream.param req "id" in
  Store.fetch_run pool id
  >>= function
  | None ->  (j_err ~code:404 "unknown run id")
  | Some r ->
      (match (r.Store.r_verify_status, r.Store.r_status) with
       | None, Store.Run_status.Running -> Lwt.return ()
       | None, _ ->
           Replay.verify_and_record pool ~run_id:id
           >>= fun _ -> Lwt.return ()
       | Some _, _ -> Lwt.return ())
      >>= fun () ->
      Store.fetch_run pool id
      >>= function
      | None ->  (j_err ~code:500 "run row vanished")
      | Some r ->
          Store.fetch_journals pool id >>= fun js ->
          
            (j_ok
               (`Assoc
                 [ ("run", run_json r)
                 ; ("journal", `List (List.map journal_json js)) ]))

(* GET /api/runs/verify?all=1 — the verification sweeper: replay-verify
   every finished run (cap 200, newest first).  Without all=1, returns
   the current verify statuses only.  NOTE: routed BEFORE /api/runs/:id. *)
let verify_sweep pool _auth req =
  match Dream.query req "all" with
  | Some "1" ->
      Store.list_runs pool ~caller:None ~program:None ~limit:200 ()
      >>= fun rs ->
      let rec go acc = function
        | [] -> Lwt.return (List.rev acc)
        | r :: rest -> (
            Replay.verify_and_record pool ~run_id:r.Store.r_id
            >>= fun v -> go (verdict_json r.Store.r_id v :: acc) rest)
      in
      go [] rs >>= fun vs ->
       (j_ok (`Assoc [ ("verified", `List vs) ]))
  | _ ->
      Store.list_runs pool ~caller:None ~program:None ~limit:200 ()
      >>= fun rs ->
       (j_ok
          (`Assoc
            [ ( "runs"
              , `List
                  (List.map
                     (fun r ->
                       `Assoc
                         [ ("id", `String r.Store.r_id)
                         ; ("verify_status", opt_str r.Store.r_verify_status)
                         ; ("status", `String (Store.Run_status.to_string r.Store.r_status)) ])
                     rs)) ]))

let list_runs pool _auth req =
  let caller = Dream.query req "caller" in
  let program = Dream.query req "program" in
  Store.list_runs pool ~caller ~program ~limit:100 () >>= fun rs ->
   (j_ok (`Assoc [ ("runs", `List (List.map run_json rs)) ]))

(* -- journals -------------------------------------------------------- *)

let get_journal pool _auth req =
  let run_id = Dream.param req "run_id" in
  Store.fetch_run pool run_id
  >>= function
  | None ->  (j_err ~code:404 "unknown run id")
  | Some _ ->
      Store.fetch_journals pool run_id >>= fun js ->
      
        (j_ok
           (`Assoc
             [ ("run_id", `String run_id)
             ; ("journal", `List (List.map journal_json js)) ]))

type edit = Set_result of Tuna.Tree.t | Set_error of string | Clear

(* Copy the parent run's journal into a derived run with the edits
   applied (chain rebuilt over the NEW run id), then re-execute
   journal-fed (Replay.reexecute): the counterfactual outcome becomes
   the derived row's status/result.  A divergent edit (e.g. a cleared
   row) leaves the run in Error; history itself is never mutated. *)
let fork pool ~parent_run_id ~(edits : (int * edit) list) =
  Store.fetch_run pool parent_run_id
  >>= (function
        | None -> Lwt.fail (Failure "unknown run")
        | Some parent ->
            Store.fetch_journals pool parent_run_id
            >>= fun js ->
            Store.insert_run pool ~program_hash:parent.r_program_hash
              ~inputs:parent.r_input_hashes ~caller:parent.r_caller
              ~parent_run_id:(Some parent_run_id) ~fuel:parent.r_fuel
              ~size_cap:parent.r_size_cap ()
            >>= fun new_id ->
            let rec copy = function
              | [] -> Lwt.return ()
              | j :: rest -> (
                  let result_ternary, e_error =
                    match List.assoc j.Store.j_seq edits with
                    | Set_result t -> (Some (Tuna.Canon.encode t), None)
                    | Set_error e -> (None, Some e)
                    | exception Not_found ->
                        (j.Store.j_result_ternary, j.Store.j_error)
                    | Clear -> (None, None)
                  in
                  let ev : Store.journal_event =
                    { e_callsite_path = j.Store.j_callsite_path
                    ; e_prim = j.Store.j_prim
                    ; e_prim_contract = j.Store.j_prim_contract
                    ; e_grant_id = j.Store.j_grant_id
                    ; e_args_ternary = j.Store.j_args_ternary
                    ; e_result_ternary = result_ternary
                    ; e_error = e_error
                    ; e_wall_ms = j.Store.j_wall_ms }
                  in
                  Store.append_journal pool ~run_id:new_id ev
                  >>= fun _ -> copy rest)
            in
            copy js >>= fun () ->
            Store.insert_derived_journal pool ~run_id:new_id
              ~parent_run_id:parent_run_id ()
            >>= fun () ->
            Replay.reexecute pool ~run_id:new_id
            >>= fun v ->
            Store.fetch_run pool new_id
            >>= (function
                  | None -> Lwt.fail (Failure "fork run vanished")
                  | Some row ->
                      Store.fetch_journals pool new_id
                      >>= fun njs ->
                      Lwt.return (row, njs, v)))

let rec validate_edits acc = function
  | [] -> Ok (List.rev acc)
  | e :: rest -> (
      match get_int_opt e "seq" with
      | None -> Error "each edit needs integer \"seq\""
      | Some seq -> (
          let result = get_string_opt e "result_ternary" in
          let error = get_string_opt e "error" in
          match (result, error) with
          | Some _, Some _ ->
              Error
                (Printf.sprintf
                   "edit for seq %d sets both result_ternary and error" seq)
          | Some t, None -> (
              match parse_ternary t with
              | Error msg -> Error (Printf.sprintf "edit for seq %d: %s" seq msg)
              | Ok tr -> validate_edits ((seq, Set_result tr) :: acc) rest)
          | None, Some err -> validate_edits ((seq, Set_error err) :: acc) rest
          | None, None -> validate_edits ((seq, Clear) :: acc) rest))

(* fork: counterfactual replay (journal.counterfactual-edits).  The
   derived run gets a fresh journal with the edits applied and the hash
   chain rebuilt over the NEW run id (row fingerprints include run_id,
   so every row_hash changes).  The derived run is then RE-EXECUTED
   journal-fed (replay engine): the edited answers flow through the
   program, the derived row carries the counterfactual outcome, and
   its own verification is recorded. *)
let fork_journal pool _auth req =
  let run_id = Dream.param req "run_id" in
  body_json req >>= function
  | Error msg ->  (j_err msg)
  | Ok j -> (
      match validate_edits [] (get_list j "edits") with
      | Error msg ->  (j_err msg)
      | Ok edits ->
          (* edits are counterfactual REPLACEMENTS of recorded rows: an
             edit naming a seq the parent journal does not hold is a
             request to overwrite history that never happened -> 400 *)
          Store.fetch_run pool run_id >>= (function
          | None -> j_err ~code:404 "unknown run id"
          | Some _ ->
              Store.fetch_journals pool run_id >>= fun js ->
              let known = List.length js in
              match List.find_opt (fun (seq, _) -> seq >= known) edits with
              | Some (seq, _) ->
                  j_err
                    (Printf.sprintf
                       "edit seq %d is beyond the parent journal (%d rows)"
                       seq known)
              | None ->
                  fork pool ~parent_run_id:run_id ~edits
                  >>= fun (row, njs, v) ->
                  j_ok ~code:201
                    (`Assoc
                      [ ("run", run_json row)
                      ; ("forked_from", `String run_id)
                      ; ("verify", verdict_json row.Store.r_id v)
                      ; ("journal", `List (List.map journal_json njs)) ])))

(* -- grants (JSON surface; the M8 UI admin page sits on top) ---------- *)

let post_grant pool auth req =
  body_json req >>= function
  | Error msg ->  (j_err msg)
  | Ok j -> (
      let prim = get_string_opt j "prim" in
      let attenuation = get_string_opt j "args_attenuation" in
      match prim with
      | None ->  (j_err "missing \"prim\"")
      | Some prim ->
          let attenuation =
            Option.value attenuation ~default:"{}" (* jsonb: admit-all *)
          in
          (try
             J.from_string attenuation |> ignore;
             Store.mint_grant pool ~prim ~args_attenuation:attenuation
               ~caller:auth.auth_id ~minted_by:(Some auth.auth_id) ()
             >>= fun g ->
              (j_ok ~code:201
                 (`Assoc
                   [ ("id", `String g.Store.g_id)
                   ; ("prim", `String g.Store.g_prim)
                   ; ("caller", `String g.Store.g_caller)
                   ; ("args_attenuation", `String g.Store.g_args_attenuation) ]))
           with Yojson.Json_error _ ->
             (j_err "args_attenuation must be JSON")))

let revoke_grant pool auth req =
  let id = Dream.param req "id" in
  Store.fetch_grant pool id
  >>= (function
        | None ->  (j_err ~code:404 "unknown grant id")
        | Some g ->
            (* only the grant's caller (or an admin) may revoke; the
               grant row is the capability *)
            if g.Store.g_caller <> auth.auth_id && not auth.auth_is_admin then
              (j_err ~code:403 "grant belongs to another identity")
            else
              Store.revoke_grant pool id
              >>= fun () ->
               (j_ok (`Assoc [ ("revoked", `String id) ])))

(* -- router / server ------------------------------------------------- *)

(* route list, mounted alongside the M8 page routes by bin/main.ml *)
let api_routes pool =
  [ Dream.post "/api/programs" (with_auth pool (post_program pool))
    ; Dream.get "/api/programs/:hash" (with_auth pool (get_program pool))
    ; Dream.post "/api/programs/:hash/patch"
        (with_auth pool (patch_program pool))
    ; Dream.post "/api/runs" (with_auth pool (post_run pool))
    ; Dream.get "/api/runs" (with_auth pool (list_runs pool))
    ; Dream.get "/api/runs/verify" (with_auth pool (verify_sweep pool))
    ; Dream.get "/api/runs/:id" (with_auth pool (get_run pool))
    ; Dream.get "/api/journals/:run_id" (with_auth pool (get_journal pool))
    ; Dream.post "/api/journals/:run_id/fork"
        (with_auth pool (fork_journal pool))
    ; Dream.post "/api/grants" (with_auth pool (post_grant pool))
    ; Dream.post "/api/grants/:id/revoke" (with_auth pool (revoke_grant pool)) ]

(* assemble the full router: health + JSON API + human pages + static *)
let router ?(static_dir = "server/static") pool =
  Dream.router
    (Dream.get "/health" (health pool)
    :: api_routes pool
    @ Pages.open_routes pool
    @ Pages.routes pool
    @ [ Dream.get "/static/**" (Dream.static static_dir) ])

(* Called by bin/main.ml inside its own Lwt_main.run: bootstraps the
   root identity, then serves without spawning another event loop. *)
(* Identity bootstrap: PP_BOOTSTRAP pattern.  First boot (no root row):
   the token (TUNA_BOOTSTRAP_TOKEN or freshly generated) is printed once
   and its sha256 stored.  Re-boots: the supplied token MUST verify
   against the stored hash — a generated token against an existing root
   is a hard boot error, never a silent new credential. *)
let serve ~port ~bootstrap_token =
  let cfg = Db.config_from_env () in
  Db.init cfg >>= fun pool ->
  let boot =
    Store.fetch_identity_by_name pool "root"
    >>= function
    | Some root -> (
        Store.verify_token pool bootstrap_token
        >>= function
        | Some r when r.Store.i_id = root.Store.i_id -> Lwt.return pool
        | _ ->
            Lwt.fail
              (Failure
                 "root identity already exists: boot requires its original \
                  token in TUNA_BOOTSTRAP_TOKEN") )
    | None ->
        print_string ("TUNA_BOOTSTRAP_TOKEN=" ^ bootstrap_token ^ "\n");
        flush stdout;
        Store.bootstrap_identity pool ~name:"root" ~token:bootstrap_token ()
        >>= fun root ->
        Dream.log "boot: created root identity %s" root.Store.i_id;
        Lwt.return pool
  in
  boot >>= fun pool ->
  Dream.log "boot: identity bootstrap ok";
  Dream.serve ~interface:"127.0.0.1" ~port
    (Dream.logger
    @@ router ~static_dir:
         (Option.value (Sys.getenv_opt "TUNA_STATIC_DIR") ~default:"server/static")
         pool)
