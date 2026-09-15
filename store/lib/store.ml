(* Store accessors over migrations/0001-init.sql: identities,
   programs, runs, journals (hash-chained appends), grants.

   Conventions:
   - uuids are passed through SQL casts as text ($1::uuid) and read
     back via `id::text` — the store layer never touches Uuidm.
   - tree hashes are sha256 over the canonical ternary string,
     lowercase hex (Tuna.Hash).
   - the journal hash chain is per run: row_hash =
     sha256(fingerprint row) over a fixed-field encoding; seq 0 chains
     from [genesis]. *)
open Lwt.Infix

module Db_alias = Db
module V = Pgx.Value

exception Store_error of string

let store_error fmt = Printf.ksprintf (fun s -> raise (Store_error s)) fmt

(* -- row plumbing ---------------------------------------------------- *)

(* pgx rows are [V.t list]; select columns in the exact order the
   decoders below expect.  V.t = v option (None = SQL NULL). *)
type param = V.t

let p_str s = V.of_string s
let p_opt o = Option.fold ~some:V.of_string ~none:V.null o
let p_int64 i = V.of_int64 i
let p_int i = V.of_int i
let p_bool b = V.of_bool b
let p_text_list xs = V.of_list (List.map V.of_string xs)

let col (r : Pgx.row) i = List.nth r i

let text r i what =
  match col r i with
  | Some v -> V.to_string_exn (Some v)
  | None -> store_error "null in column %d (%s)" i what

let opt_text r i = V.to_string (col r i)
let int64 r i _what = V.to_int64_exn (col r i)
let opt_int64 r i = V.to_int64 (col r i)
let int r i _what = Int64.to_int (int64 r i _what)
let opt_int r i = V.to_int (col r i)
let bool r i _what = V.to_bool_exn (col r i)

let text_list r i what =
  match col r i with
  | None -> store_error "expected array in column %d (%s)" i what
  | Some v -> List.map V.to_string_exn (V.to_list_exn (Some v))

(* -- programs -------------------------------------------------------- *)

type program = {
  p_hash : string
; p_ternary : string
; p_ir : string option  (* yojson text *)
; p_created_by : string option
}

let program_of_row r what =
  { p_hash = text r 0 what
  ; p_ternary = text r 1 what
  ; p_ir = opt_text r 2
  ; p_created_by = opt_text r 3 }

let select_program =
  "SELECT hash, ternary, ir::text, created_by::text FROM programs WHERE hash = $1"

let fetch_program p hash =
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  (match rows with
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (program_of_row r hash))
  | _ -> store_error "multiple program rows for hash %s" hash )

(* get-or-create by hash: hash is content-addressed so conflicting
   content is impossible by construction; first insert wins for the
   ir/created_by metadata. *)
let upsert_program p ~hash ~ternary ~ir ~created_by =
  Db.q_unit
    ~params:[ p_str hash
            ; p_str ternary
            ; V.of_string (Option.value ir ~default:"null")
            ; p_opt created_by ]
    p
    "INSERT INTO programs (hash, ternary, ir, created_by) \
     VALUES ($1, $2, $3::jsonb, $4::uuid) ON CONFLICT (hash) DO NOTHING"
  >>= fun () ->
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  (match rows with
  | [ r ] -> Lwt.return (program_of_row r hash)
  | n -> store_error "program upsert: fetch after insert gave %d rows" (List.length n)
         )

(* -- runs ------------------------------------------------------------ *)

module Run_status = struct
  type t = Running | Normal | Fuel_exhausted | Size_exhausted | Error

  let to_string = function
    | Running -> "running"
    | Normal -> "normal"
    | Fuel_exhausted -> "fuel_exhausted"
    | Size_exhausted -> "size_exhausted"
    | Error -> "error"

  let of_string = function
    | "running" -> Running
    | "normal" -> Normal
    | "fuel_exhausted" -> Fuel_exhausted
    | "size_exhausted" -> Size_exhausted
    | "error" -> Error
    | s -> store_error "bad run status %S" s
end

type run = {
  r_id : string
; r_program_hash : string
; r_input_hashes : string list
; r_fuel : int
; r_size_cap : int
; r_result_hash : string option
; r_result_ternary : string option
; r_step_count : int option
; r_status : Run_status.t
; r_caller : string option
; r_parent_run_id : string option
; r_verify_status : string option
; r_created_at : string option
}

let select_run =
  "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
   result_ternary, step_count, status, caller::text, parent_run_id::text, \
   verify_status, created_at::text FROM runs WHERE id = $1::uuid"

let run_of_row r =
  { r_id = text r 0 "run.id"
  ; r_program_hash = text r 1 "run.program_hash"
  ; r_input_hashes = text_list r 2 "run.input_hashes"
  ; r_fuel = int r 3 "run.fuel"
  ; r_size_cap = int r 4 "run.size_cap"
  ; r_result_hash = opt_text r 5
  ; r_result_ternary = opt_text r 6
  ; r_step_count = opt_int r 7
  ; r_status = Run_status.of_string (text r 8 "run.status")
  ; r_caller = opt_text r 9
  ; r_parent_run_id = opt_text r 10
  ; r_verify_status = opt_text r 11
  ; r_created_at = opt_text r 12 }

let insert_run p ~program_hash ?(inputs = []) ?(caller = None) ?(parent_run_id = None)
    ~fuel ~size_cap () =
  Db.q
    ~params:[ p_str program_hash
            ; p_text_list inputs
            ; p_int64 (Int64.of_int fuel)
            ; p_int64 (Int64.of_int size_cap)
            ; p_opt caller
            ; p_opt parent_run_id ]
    p
    "INSERT INTO runs (program_hash, input_hashes, fuel, size_cap, caller, \
     parent_run_id, status) VALUES ($1, $2, $3, $4, $5::uuid, $6::uuid, \
     'running') RETURNING id::text"
  >>= fun rows ->
  (match rows with
  | [ r ] -> Lwt.return (text r 0 "run.id")
  | n -> store_error "insert_run: RETURNING gave %d rows" (List.length n) )

let update_run_result p ~id ~status ?result_ternary ?step_count () =
  let result_hash = Option.map Tuna.Hash.hex_of_string result_ternary in
  Db.q_unit
    ~params:[ p_str (Run_status.to_string status)
            ; p_opt result_hash
            ; p_opt result_ternary
            ; (match step_count with Some s -> p_int s | None -> None)
            ; p_str id ]
    p
    "UPDATE runs SET status = $1, result_hash = $2, result_ternary = $3, \
     step_count = $4 WHERE id = $5::uuid"

(* verify_status update (replay engine writes verified|failed; M7) *)
let update_verify_status p ~id ~verify_status () =
  Db.q_unit
    ~params:[ p_str verify_status; p_str id ]
    p
    "UPDATE runs SET verify_status = $1, verified_at = now() WHERE id = $2::uuid"

let fetch_run p id =
  Db.q ~params:[ p_str id ] p select_run
  >>= fun rows ->
  (match rows with
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (run_of_row r))
  | _ -> store_error "multiple run rows for id %s" id )

(* list newest-first; caller/program filters optional *)
let list_runs p ?(caller = None) ?(program = None) ?(limit = 50) () =
  Db.q
    ~params:[ p_opt caller; p_opt program; p_int limit ]
    p
    "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
     result_ternary, step_count, status, caller::text, parent_run_id::text, \
     verify_status, created_at::text FROM runs \
     WHERE ($1::uuid IS NULL OR caller = $1::uuid) \
       AND ($2::text IS NULL OR program_hash = $2) \
     ORDER BY created_at DESC LIMIT $3"
  >>= fun rows -> Lwt.return (List.map run_of_row rows)

(* -- journals -------------------------------------------------------- *)

type journal_event = {
  e_callsite_path : string
; e_prim : string
; e_prim_contract : string
; e_grant_id : string option
; e_args_ternary : string option
; e_result_ternary : string option
; e_error : string option
; e_wall_ms : int option
}

type journal = {
  j_run_id : string
; j_seq : int
; j_callsite_path : string
; j_prim : string
; j_prim_contract : string
; j_grant_id : string option
; j_args_ternary : string option
; j_args_hash : string option
; j_result_ternary : string option
; j_result_hash : string option
; j_error : string option
; j_wall_ms : int option
; j_host_build : string
; j_prev_hash : string
; j_row_hash : string
}

let genesis = String.make 64 '0'

(* canonical field encoding hashed into row_hash: RAW payload ternaries
   (NOT their column hashes) go in, so tampering with either payload or
   chain is caught by the walk.  Fields in a fixed order, \x1f-separated
   (safe: tree payloads are ternary digits).  run_id is included so
   identical rows in different runs don't share a row_hash. *)
let fingerprint run_id seq ev prev_hash =
  let field = function Some s -> s | None -> "" in
  String.concat "\x1f"
    [ run_id
    ; string_of_int seq
    ; ev.e_callsite_path
    ; ev.e_prim
    ; ev.e_prim_contract
    ; field ev.e_grant_id
    ; field ev.e_args_ternary
    ; field ev.e_result_ternary
    ; field ev.e_error
    ; (match ev.e_wall_ms with Some w -> string_of_int w | None -> "")
    ; prev_hash ]

let row_hash run_id seq ev prev_hash =
  Tuna.Hash.hex_of_string (fingerprint run_id seq ev prev_hash)

let select_journals =
  "SELECT run_id::text, seq, callsite_path, prim, prim_contract, \
   grant_id::text, args_ternary, args_hash, result_ternary, result_hash, \
   error, wall_ms, host_build, prev_hash, row_hash FROM journals \
   WHERE run_id = $1::uuid ORDER BY seq"

let journal_of_row r =
  { j_run_id = text r 0 "journal.run_id"
  ; j_seq = int r 1 "journal.seq"
  ; j_callsite_path = text r 2 "journal.callsite_path"
  ; j_prim = text r 3 "journal.prim"
  ; j_prim_contract = text r 4 "journal.prim_contract"
  ; j_grant_id = opt_text r 5
  ; j_args_ternary = opt_text r 6
  ; j_args_hash = opt_text r 7
  ; j_result_ternary = opt_text r 8
  ; j_result_hash = opt_text r 9
  ; j_error = opt_text r 10
  ; j_wall_ms = opt_int r 11
  ; j_host_build = text r 12 "journal.host_build"
  ; j_prev_hash = text r 13 "journal.prev_hash"
  ; j_row_hash = text r 14 "journal.row_hash" }

(* Append one boundary event, extending the run's hash chain.  Appends
   are sequenced by the run's single evaluator thread; a concurrent
   append collides on (run_id, seq) and raises. *)
let append_journal p ?(host_build = "tuna-dev") ~run_id ev =
  Db.q
    ~params:[ p_str run_id ]
    p
    "SELECT seq, row_hash FROM journals WHERE run_id = $1::uuid ORDER BY seq DESC LIMIT 1"
  >>= fun last ->
  let seq, prev_hash =
    match last with
    | [] -> (0, genesis)
    | [ r ] -> (int r 0 "last.seq" + 1, text r 1 "last.row_hash")
    | _ -> store_error "journal head: multiple rows" 
  in
  let args_hash = Option.map Tuna.Hash.hex_of_string ev.e_args_ternary in
  let result_hash = Option.map Tuna.Hash.hex_of_string ev.e_result_ternary in
  let h = row_hash run_id seq ev prev_hash in
  Db.q_unit
    ~params:[ p_str run_id
            ; p_int seq
            ; p_str ev.e_callsite_path
            ; p_str ev.e_prim
            ; p_str ev.e_prim_contract
            ; p_opt ev.e_grant_id
            ; p_opt ev.e_args_ternary
            ; p_opt args_hash
            ; p_opt ev.e_result_ternary
            ; p_opt result_hash
            ; p_opt ev.e_error
            ; (match ev.e_wall_ms with Some w -> p_int w | None -> None)
            ; p_str host_build
            ; p_str prev_hash
            ; p_str h ]
    p
    "INSERT INTO journals (run_id, seq, callsite_path, prim, prim_contract, \
     grant_id, args_ternary, args_hash, result_ternary, result_hash, error, \
     wall_ms, host_build, prev_hash, row_hash) \
     VALUES ($1::uuid, $2, $3, $4, $5, $6::uuid, $7, $8, $9, $10, $11, $12, \
     $13, $14, $15)"
  >>= fun () -> Lwt.return (seq, h)

let fetch_journals p run_id =
  Db.q ~params:[ p_str run_id ] p select_journals
  >>= fun rows -> Lwt.return (List.map journal_of_row rows)

(* counterfactual fork bookkeeping (journal.counterfactual-edits; the
   journal copy/chain-rebuild itself goes through append_journal) *)
let insert_derived_journal p ~run_id ~parent_run_id () =
  Db.q_unit
    ~params:[ p_str run_id; p_str parent_run_id ]
    p
    "INSERT INTO derived_journals (run_id, parent_run_id) \
     VALUES ($1::uuid, $2::uuid) ON CONFLICT (run_id) DO NOTHING"

(* chain walk over fetched rows: recompute each row_hash from its
   fields + expected prev hash, and check the linkage. *)
let verify_chain (js : journal list) : [ `Ok | `Bad of string ] =
  let rec go expected_prev = function
    | [] -> `Ok
    | j :: rest ->
      if j.j_prev_hash <> expected_prev then
        `Bad (Printf.sprintf "seq %d: prev_hash mismatch" j.j_seq)
      else
        let h =
          row_hash j.j_run_id j.j_seq
            { e_callsite_path = j.j_callsite_path
            ; e_prim = j.j_prim
            ; e_prim_contract = j.j_prim_contract
            ; e_grant_id = j.j_grant_id
            ; e_args_ternary = j.j_args_ternary
            ; e_result_ternary = j.j_result_ternary
            ; e_error = j.j_error
            ; e_wall_ms = j.j_wall_ms }
            j.j_prev_hash
        in
        if h <> j.j_row_hash then
          `Bad (Printf.sprintf "seq %d: row_hash mismatch" j.j_seq)
        else go h rest
  in
  go genesis js

(* -- REPL dictionary + session state (M9) ----------------------------- *)

type dict_entry = {
  d_identity : string
; d_name : string
; d_ternary : string
; d_updated_at : string option
}

let dict_entry_of_row r identity_id name =
  { d_identity = identity_id
  ; d_name = name
  ; d_ternary = text r 1 ("repl_dict." ^ name)
  ; d_updated_at = opt_text r 2 }

let select_dict_entry =
  "SELECT identity_id::text, ternary, updated_at::text FROM repl_dict \
   WHERE identity_id = $1::uuid AND name = $2"

let dict_get p ~identity_id ~name =
  Db.q ~params:[ p_str identity_id; p_str name ] p select_dict_entry
  >>= function
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (dict_entry_of_row r identity_id name))
  | _ -> store_error "repl_dict: multiple rows for %s/%s" identity_id name

let dict_set p ~identity_id ~name ~ternary =
  Db.q_unit
    ~params:[ p_str identity_id; p_str name; p_str ternary ]
    p
    "INSERT INTO repl_dict (identity_id, name, ternary) VALUES ($1::uuid, $2, $3) \
     ON CONFLICT (identity_id, name) \
     DO UPDATE SET ternary = EXCLUDED.ternary, updated_at = now()"

let dict_del p ~identity_id ~name =
  Db.q_unit
    ~params:[ p_str identity_id; p_str name ]
    p
    "DELETE FROM repl_dict WHERE identity_id = $1::uuid AND name = $2"

let dict_list p ~identity_id =
  Db.q ~params:[ p_str identity_id ] p
    "SELECT name, ternary, updated_at::text FROM repl_dict \
     WHERE identity_id = $1::uuid ORDER BY name"
  >>= fun rows ->
  Lwt.return
    (List.map
       (fun r -> dict_entry_of_row r identity_id (text r 0 "repl_dict.name"))
       rows)

(* session pointer: the identity's last journaled REPL round.  Each new
   run row links parent_run_id = this value, forming the per-session
   transcript chain (runs.parent_run_id). *)
let repl_state_get p ~identity_id =
  Db.q ~params:[ p_str identity_id ] p
    "SELECT last_run_id::text FROM repl_state WHERE identity_id = $1::uuid"
  >>= function
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (text r 0 "repl_state.last_run_id"))
  | _ -> store_error "repl_state: multiple rows for %s" identity_id

let repl_state_put p ~identity_id ~run_id =
  Db.q_unit
    ~params:[ p_str identity_id; p_str run_id ]
    p
    "INSERT INTO repl_state (identity_id, last_run_id) VALUES ($1::uuid, $2::uuid) \
     ON CONFLICT (identity_id) \
     DO UPDATE SET last_run_id = EXCLUDED.last_run_id, updated_at = now()"

(* -- prim kv (store/get + store/put; M7) ----------------------------- *)

(* Fetch by the key's content hash. Returns the (key, value) pair. *)
let prim_get p key_ternary =
  let kh = Tuna.Hash.hex_of_string key_ternary in
  Db.q ~params:[ p_str kh ] p "SELECT key_ternary, value_ternary FROM prim_kv WHERE key_hash = $1"
  >>= function
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (text r 0 "prim_kv.key", text r 1 "prim_kv.value"))
  | _ -> store_error "prim_kv: multiple rows for key %s" kh

let prim_put p ~key_ternary ~value_ternary =
  let kh = Tuna.Hash.hex_of_string key_ternary in
  Db.q_unit
    ~params:[ p_str kh; p_str key_ternary; p_str value_ternary ]
    p
    "INSERT INTO prim_kv (key_hash, key_ternary, value_ternary) \
     VALUES ($1, $2, $3) \
     ON CONFLICT (key_hash) DO UPDATE SET value_ternary = EXCLUDED.value_ternary, \
     updated_at = now()"

(* -- grants ---------------------------------------------------------- *)

type grant = {
  g_id : string
; g_prim : string
; g_args_attenuation : string  (* yojson text *)
; g_path_prefix : string option  (* M10: NULL matches everything *)
; g_caller : string
; g_minted_by : string option
; g_revoked_at : string option
}

let select_grant_by_id =
  "SELECT id::text, prim, args_attenuation::text, path_prefix, caller::text, \
   minted_by::text, revoked_at::text FROM grants WHERE id = $1::uuid"

let grant_of_row r =
  { g_id = text r 0 "grant.id"
  ; g_prim = text r 1 "grant.prim"
  ; g_args_attenuation = text r 2 "grant.args_attenuation"
  ; g_path_prefix = opt_text r 3
  ; g_caller = text r 4 "grant.caller"
  ; g_minted_by = opt_text r 5
  ; g_revoked_at = opt_text r 6 }

let mint_grant p ~prim ~args_attenuation ?(path_prefix = None) ~caller
    ?(minted_by = None) () =
  Db.q
    ~params:[ p_str prim
            ; p_str args_attenuation
            ; p_opt path_prefix
            ; p_str caller
            ; p_opt minted_by ]
    p
    "INSERT INTO grants (prim, args_attenuation, path_prefix, caller, minted_by) \
     VALUES ($1, $2::jsonb, $3, $4::uuid, $5::uuid) RETURNING id::text"
  >>= fun rows ->
  (match rows with
  | [ r ] ->
    let id = text r 0 "grant.id" in
    Db.q ~params:[ p_str id ] p select_grant_by_id
    >>= (function
         | [ r ] -> Lwt.return (grant_of_row r)
         | n -> store_error "mint_grant: fetch gave %d rows" (List.length n)
                )
  | n -> store_error "mint_grant: RETURNING gave %d rows" (List.length n) )

(* newest-first listing for the UI admin page (M8) *)
let list_grants p ?(limit = 100) () =
  Db.q ~params:[ p_int limit ] p
    ("SELECT id::text, prim, args_attenuation::text, path_prefix, caller::text, \
      minted_by::text, revoked_at::text FROM grants ORDER BY created_at DESC LIMIT $1")
  >>= fun rows -> Lwt.return (List.map grant_of_row rows)

let fetch_grant p id =
  Db.q ~params:[ p_str id ] p select_grant_by_id
  >>= fun rows ->
  (match rows with
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (grant_of_row r))
  | _ -> store_error "multiple grant rows for id %s" id )

(* immediate, forward-only: next live boundary check fails *)
let revoke_grant p id =
  Db.q_unit
    ~params:[ p_str id ]
    p
    "UPDATE grants SET revoked_at = now() WHERE id = $1::uuid AND revoked_at IS NULL"

(* deny-check at a prim boundary: grant must exist, be unrevoked, and
   belong to the claiming caller.  Recorded runs never re-check (the
   journal answers, not the grant table).

   M10 path scoping: when the call carries tree paths (the substrate
   prims + ns/fork pass every path it would read or write), the grant's
   path_prefix must cover ALL of them (simple string prefix; NULL
   matches everything).  A path-scoped grant cannot authorize a
   pathless call - the narrowing is the grant. *)
let prefix_match prefix path =
  String.length path >= String.length prefix
  && String.sub path 0 (String.length prefix) = prefix

let check_grant p ~id ~caller ?(paths = []) () :
  [ `Ok | `Revoked | `Wrong_caller | `Unknown | `Prefix_denied ] Lwt.t =
  fetch_grant p id
  >>= function
  | None -> Lwt.return `Unknown
  | Some g when g.g_revoked_at <> None -> Lwt.return `Revoked
  | Some g when g.g_caller <> caller -> Lwt.return `Wrong_caller
  | Some g -> (
      match (g.g_path_prefix, paths) with
      | None, _ -> Lwt.return `Ok
      | Some _, [] -> Lwt.return `Prefix_denied
      | Some prefix, ps ->
          if List.for_all (prefix_match prefix) ps then Lwt.return `Ok
          else Lwt.return `Prefix_denied)

(* -- identities ------------------------------------------------------ *)

type identity = {
  i_id : string
; i_name : string
; i_token_hash : string
; i_is_admin : bool
}

let identity_of_row r =
  { i_id = text r 0 "identity.id"
  ; i_name = text r 1 "identity.name"
  ; i_token_hash = text r 2 "identity.token_hash"
  ; i_is_admin = bool r 3 "identity.is_admin" }

let select_identity_by_id =
  "SELECT id::text, name, token_hash, is_admin FROM identities WHERE id = $1::uuid"

let fetch_identity p id =
  Db.q ~params:[ p_str id ] p select_identity_by_id
  >>= fun rows ->
  (match rows with
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (identity_of_row r))
  | _ -> store_error "multiple identity rows for id %s" id )

let fetch_identity_by_name p name =
  Db.q ~params:[ p_str name ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE name = $1"
  >>= fun rows ->
  (match rows with
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (identity_of_row r))
  | _ -> store_error "multiple identity rows for name %s" name )

(* bootstrap (server-boot): insert (name, sha256 token) if absent;
   idempotent — a re-boot with the same token is a no-op, and the
   identity row is returned either way. *)
let bootstrap_identity p ?(is_admin = true) ~name ~token () =
  Db.q_unit
    ~params:[ p_str name; p_str (Tuna.Hash.hex_of_string token); p_bool is_admin ]
    p
    "INSERT INTO identities (name, token_hash, is_admin) VALUES ($1, $2, $3) \
     ON CONFLICT (name) DO NOTHING"
  >>= fun () ->
  fetch_identity_by_name p name
  >>= (function
       | Some i -> Lwt.return i
       | None -> store_error "bootstrap_identity: insert succeeded but fetch failed"
                 )

(* token verify (sha256 lookup) — the only identity query by secret *)
let verify_token p token =
  Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE token_hash = $1"
  >>= fun rows ->
  (match rows with
   | [] -> Lwt.return None
   | [ r ] -> Lwt.return (Some (identity_of_row r))
   | _ -> Lwt.return None)

(* -- tree substrate: derived path index + chained op log (M10) --------

   migrations 0005/0006.  Law (tuna.borg watch note): content-addressed
   VALUES are the primitive (tree_values, dedup by sha256); tree_paths
   is a DERIVED mutable index (path -> value_hash + version) and never
   storage truth; tree_ops is the append-only effect log whose op_hash
   chain makes rewind a pure fold.

   Path range scans pin the C collation so the [prefix, prefix||chr(255))
   bound and the list ordering are byte-wise regardless of the cluster
   collation (validated paths are printable ASCII, so any extension of
   a prefix is bytewise below prefix||U+00FF). *)

let path_hash = Tuna.Hash.hex_of_string

type path_entry = {
  tp_path : string
; tp_value_hash : string
; tp_version : int64
; tp_owner : string
; tp_updated_at : string option
}

let path_entry_of_row r what =
  { tp_path = text r 0 what
  ; tp_value_hash = text r 1 what
  ; tp_version = int64 r 2 what
  ; tp_owner = text r 3 what
  ; tp_updated_at = opt_text r 4 }

let select_path_entry =
  "SELECT path, value_hash, version, owner, updated_at::text FROM tree_paths \
   WHERE path_hash = $1"

(* value hash + version + owner; the caller fetches value bytes by hash
   through value_fetch when it needs the tree itself *)
let path_get p ~path =
  Db.q ~params:[ p_str (path_hash path) ] p select_path_entry
  >>= function
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (path_entry_of_row r "tree_paths"))
  | _ -> store_error "tree_paths: multiple rows for path %s" path

(* -- the value side (content-addressed, migration 0006) --------------- *)

let value_put p ~hash ~ternary =
  Db.q_unit
    ~params:[ p_str hash; p_str ternary ]
    p
    "INSERT INTO tree_values (hash, ternary) VALUES ($1, $2) \
     ON CONFLICT (hash) DO NOTHING"

let value_fetch p hash =
  Db.q ~params:[ p_str hash ] p "SELECT ternary FROM tree_values WHERE hash = $1"
  >>= function
  | [] -> Lwt.return None
  | [ r ] -> Lwt.return (Some (text r 0 "tree_values.ternary"))
  | _ -> store_error "tree_values: multiple rows for hash %s" hash

(* unconditional write: INSERT at version 1, or version+1 on the
   existing row (tree/put prim).  Returns the new version. *)
let path_put p ~path ~value_hash ~owner =
  Db.q
    ~params:[ p_str (path_hash path); p_str path; p_str value_hash; p_str owner ]
    p
    "INSERT INTO tree_paths (path, path_hash, value_hash, version, owner) \
     VALUES ($2, $1, $3, 1, $4) \
     ON CONFLICT (path_hash) DO UPDATE \
       SET value_hash = EXCLUDED.value_hash, version = tree_paths.version + 1, \
           owner = EXCLUDED.owner, updated_at = now() \
     RETURNING version"
  >>= fun rows ->
  (match rows with
   | [ r ] -> Lwt.return (int64 r 0 "tree_paths.version")
   | n -> store_error "path_put: RETURNING gave %d rows" (List.length n))

(* versioned CAS write (tree/cas):
   - expected_version None (or 0) means CREATE: insert at version 1;
     `Conflict if the path already exists.
   - expected_version (Some n) updates only when the live row is at
     version n (and, when expected_hash is given, still carries that
     value hash); `Conflict on any mismatch, `Absent when the path has
     no row at all.  409-style, as an answer variant - never an
     exception at the boundary. *)
let path_put_cas p ~path ~value_hash ~owner ~expected_version ~expected_hash =
  let h = path_hash path in
  let classify () =
    path_get p ~path
    >>= function
    | None -> Lwt.return `Absent
    | Some _ -> Lwt.return `Conflict
  in
  match expected_version with
  | None | Some 0L ->
      Db.q
        ~params:[ p_str h; p_str path; p_str value_hash; p_str owner ]
        p
        "INSERT INTO tree_paths (path, path_hash, value_hash, version, owner) \
         VALUES ($2, $1, $3, 1, $4) ON CONFLICT (path_hash) DO NOTHING \
         RETURNING version"
      >>= (function
            | [ r ] -> Lwt.return (`Ok (int64 r 0 "tree_paths.version"))
            | [] -> classify ()
            | n -> store_error "path_put_cas: RETURNING gave %d rows" (List.length n))
  | Some n -> (
      let sql =
        match expected_hash with
        | Some _ ->
            "UPDATE tree_paths SET value_hash = $4, version = version + 1, \
             owner = $5, updated_at = now() \
             WHERE path_hash = $1 AND version = $2 AND value_hash = $3 \
             RETURNING version"
        | None ->
            "UPDATE tree_paths SET value_hash = $3, version = version + 1, \
             owner = $4, updated_at = now() \
             WHERE path_hash = $1 AND version = $2 \
             RETURNING version"
      in
      let params =
        match expected_hash with
        | Some eh -> [ p_str h; p_int64 n; p_str eh; p_str value_hash; p_str owner ]
        | None -> [ p_str h; p_int64 n; p_str value_hash; p_str owner ]
      in
      Db.q ~params p sql
      >>= (function
            | [ r ] -> Lwt.return (`Ok (int64 r 0 "tree_paths.version"))
            | [] -> classify ()
            | n -> store_error "path_put_cas: RETURNING gave %d rows" (List.length n)))

(* prefix range scan: every path starting with [prefix], byte-wise
   ordered.  [prefix] must be non-empty (the whole-namespace read is an
   API concern, not a store invariant). *)
let path_list p ?(limit = 1000) ~prefix () =
  Db.q
    ~params:[ p_str prefix; p_int limit ]
    p
    "SELECT path, value_hash, version, owner, updated_at::text FROM tree_paths \
     WHERE path >= ($1 COLLATE \"C\") \
       AND path < (($1 || chr(255)) COLLATE \"C\") \
     ORDER BY path COLLATE \"C\" LIMIT $2"
  >>= fun rows -> Lwt.return (List.map (fun r -> path_entry_of_row r "tree_paths") rows)

(* -- the op log (sha256-chained; rewind folds it) --------------------- *)

type tree_op = {
  o_seq : int64
; o_path : string
; o_op : string  (* get|put|cas|list|fork *)
; o_value_hash : string option  (* NULL for get/list and non-effects *)
; o_prev_version : int64 option
; o_version : int64 option
; o_actor : string
; o_ts_unix : int64
; o_op_hash : string
}

(* canonical op-row encoding hashed into op_hash (spec: seq:|path|op|
   value_hash|prev_version|version|actor|ts-unix, '|' separated, empty
   string for NULL fields, seq labeled with ':').  The chain is
   op_hash = sha256(prev_op_hash ^ row_concat), genesis prev = 64*'0'. *)
let op_concat ~seq ~path ~op ~value_hash ~prev_version ~version ~actor ~ts_unix
    =
  let s64 = Int64.to_string in
  let opt f = function Some v -> f v | None -> "" in
  Printf.sprintf "%Ld:%s|%s|%s|%s|%s|%s|%Ld" seq path op
    (opt Fun.id value_hash) (opt s64 prev_version) (opt s64 version) actor
    ts_unix

let tree_op_concat (o : tree_op) =
  op_concat ~seq:o.o_seq ~path:o.o_path ~op:o.o_op ~value_hash:o.o_value_hash
    ~prev_version:o.o_prev_version ~version:o.o_version ~actor:o.o_actor
    ~ts_unix:o.o_ts_unix

let select_tree_ops =
  "SELECT seq, path, op, value_hash, prev_version, version, actor, \
   EXTRACT(epoch FROM ts)::bigint, op_hash FROM tree_ops"

let tree_op_of_row r =
  { o_seq = int64 r 0 "tree_ops.seq"
  ; o_path = text r 1 "tree_ops.path"
  ; o_op = text r 2 "tree_ops.op"
  ; o_value_hash = opt_text r 3
  ; o_prev_version = opt_int64 r 4
  ; o_version = opt_int64 r 5
  ; o_actor = text r 6 "tree_ops.actor"
  ; o_ts_unix = int64 r 7 "tree_ops.ts"
  ; o_op_hash = text r 8 "tree_ops.op_hash" }

(* connection-level append (ns_fork journals inside its transaction);
   same append discipline as the run journal: the head is read then the
   row inserted, so a concurrent append collides on the PK and raises. *)
let op_append_conn c ~op ~path ~value_hash ~prev_version ~version ~actor =
  Db.q_conn c "SELECT seq, op_hash FROM tree_ops ORDER BY seq DESC LIMIT 1"
  >>= fun last ->
  let seq, prev_hash =
    match last with
    | [] -> (1L, genesis)
    | [ r ] -> (Int64.succ (int64 r 0 "last.seq"), text r 1 "last.op_hash")
    | _ -> store_error "tree_ops head: multiple rows"
  in
  let ts_unix = Int64.of_float (Unix.gettimeofday ()) in
  let concat =
    op_concat ~seq ~path ~op ~value_hash ~prev_version ~version ~actor ~ts_unix
  in
  let h = Tuna.Hash.hex_of_string (prev_hash ^ concat) in
  Db.q_conn_unit
    ~params:[ p_int64 seq
            ; p_str path
            ; p_str op
            ; p_opt value_hash
            ; (match prev_version with Some v -> p_int64 v | None -> None)
            ; (match version with Some v -> p_int64 v | None -> None)
            ; p_str actor
            ; p_int64 ts_unix
            ; p_str h ]
    c
    "INSERT INTO tree_ops (seq, path, op, value_hash, prev_version, version, \
     actor, ts, op_hash) VALUES ($1, $2, $3, $4, $5, $6, $7, \
     to_timestamp($8::double precision), $9)"
  >>= fun () -> Lwt.return (seq, h)

(* pool-level append: one op row on the global log.  Returns (seq, op_hash). *)
let op_append p ~op ~path ~value_hash ~prev_version ~version ~actor =
  Db.with_pool p (fun c ->
      op_append_conn c ~op ~path ~value_hash ~prev_version ~version ~actor)

(* ops_fold: the replay/rewind surface.  Reads rows for [prefix] (empty
   string = whole log) in [from_seq, to_seq], ascending. *)
let ops_fold p ?(prefix = "") ?(from_seq = 0L) ?(to_seq = Int64.max_int) () =
  let sql =
    select_tree_ops
    ^ " WHERE seq >= $1 AND seq <= $2 \
       AND ($3::text IS NULL OR (path >= ($3 COLLATE \"C\") \
            AND path < (($3 || chr(255)) COLLATE \"C\"))) \
       ORDER BY seq"
  in
  Db.q
    ~params:[ p_int64 from_seq
            ; p_int64 to_seq
            ; p_opt (if prefix = "" then None else Some prefix) ]
    p sql
  >>= fun rows -> Lwt.return (List.map tree_op_of_row rows)

(* chain walk over fetched rows: recompute each op_hash from its fields
   + the previous row's STORED op_hash (the log stores no prev column,
   so the linkage is proven by the recompute itself).  Expects rows in
   ascending seq order; a contiguous run from any start verifies, and
   any tampered field or hash breaks it. *)
let verify_ops_chain (ops : tree_op list) : [ `Ok | `Bad of string ] =
  match ops with
  | [] -> `Ok
  | first :: rest ->
      (* a window starting at seq 1 anchors from genesis (the append
         side chained prev_hash ^ concat); a window starting mid-log
         (prefix folds) can only prove INTERNAL linkage -- its first
         row's fields are trusted as the anchor *)
      let anchor =
        if first.o_seq = 1L then
          Tuna.Hash.hex_of_string (genesis ^ tree_op_concat first)
        else first.o_op_hash
      in
      if anchor <> first.o_op_hash then
        `Bad (Printf.sprintf "seq %Ld: op_hash mismatch" first.o_seq)
      else
        let rec go prev = function
          | [] -> `Ok
          | o :: rest ->
              let h = Tuna.Hash.hex_of_string (prev ^ tree_op_concat o) in
              if h <> o.o_op_hash then
                `Bad (Printf.sprintf "seq %Ld: op_hash mismatch" o.o_seq)
              else go h rest
        in
        go first.o_op_hash rest

(* namespace fork: copy the [src_prefix] index slice under [dst_prefix]
   in ONE transaction (versions reset to 1, owner = forking actor) and
   journal it: one 'fork' marker row, then one 'put' row per copied path
   (the put rows carry value_hash/version so a pure rewind fold
   reproduces the fork without consulting the live index). *)
let ns_fork p ~src_prefix ~dst_prefix ~actor =
  Db.with_tx p (fun c ->
      Db.q_conn
        ~params:[ p_str src_prefix ]
        c
        "SELECT path, value_hash FROM tree_paths \
         WHERE path >= ($1 COLLATE \"C\") \
           AND path < (($1 || chr(255)) COLLATE \"C\") \
         ORDER BY path COLLATE \"C\""
      >>= fun rows ->
      let copies =
        List.map
          (fun r ->
            let src_path = text r 0 "tree_paths.path" in
            let vh = text r 1 "tree_paths.value_hash" in
            let suffix =
              String.sub src_path (String.length src_prefix)
                (String.length src_path - String.length src_prefix)
            in
            (dst_prefix ^ suffix, vh))
          rows
      in
      let rec insert_all n = function
        | [] -> Lwt.return n
        | (np, vh) :: rest ->
            Db.q_conn_unit
              ~params:[ p_str (path_hash np); p_str np; p_str vh; p_str actor ]
              c
              "INSERT INTO tree_paths (path, path_hash, value_hash, version, owner) \
               VALUES ($2, $1, $3, 1, $4) ON CONFLICT (path_hash) DO NOTHING"
            >>= fun () -> insert_all (n + 1) rest
      in
      insert_all 0 copies
      >>= fun copied ->
      op_append_conn c ~op:"fork" ~path:dst_prefix ~value_hash:None
        ~prev_version:None ~version:None ~actor
      >>= fun _fk ->
      let rec log_copies = function
        | [] -> Lwt.return copied
        | (np, vh) :: rest ->
            op_append_conn c ~op:"put" ~path:np ~value_hash:(Some vh)
              ~prev_version:None ~version:(Some 1L) ~actor
            >>= fun _ -> log_copies rest
      in
      log_copies copies)
