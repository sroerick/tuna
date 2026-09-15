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
; g_caller : string
; g_minted_by : string option
; g_revoked_at : string option
}

let select_grant_by_id =
  "SELECT id::text, prim, args_attenuation::text, caller::text, minted_by::text, \
   revoked_at::text FROM grants WHERE id = $1::uuid"

let grant_of_row r =
  { g_id = text r 0 "grant.id"
  ; g_prim = text r 1 "grant.prim"
  ; g_args_attenuation = text r 2 "grant.args_attenuation"
  ; g_caller = text r 3 "grant.caller"
  ; g_minted_by = opt_text r 4
  ; g_revoked_at = opt_text r 5 }

let mint_grant p ~prim ~args_attenuation ~caller ?(minted_by = None) () =
  Db.q
    ~params:[ p_str prim; p_str args_attenuation; p_str caller; p_opt minted_by ]
    p
    "INSERT INTO grants (prim, args_attenuation, caller, minted_by) \
     VALUES ($1, $2::jsonb, $3::uuid, $4::uuid) RETURNING id::text"
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
    ("SELECT id::text, prim, args_attenuation::text, caller::text, minted_by::text, \
      revoked_at::text FROM grants ORDER BY created_at DESC LIMIT $1")
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
   journal answers, not the grant table). *)
let check_grant p ~id ~caller : [ `Ok | `Revoked | `Wrong_caller | `Unknown ] Lwt.t =
  fetch_grant p id
  >>= function
  | None -> Lwt.return `Unknown
  | Some g when g.g_revoked_at <> None -> Lwt.return `Revoked
  | Some g when g.g_caller <> caller -> Lwt.return `Wrong_caller
  | Some _ -> Lwt.return `Ok

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
