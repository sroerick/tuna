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
open Direct.Infix

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
; p_source : string option  (* 0015: what was compiled, when known *)
; p_created_by : string option
}

let program_of_row r what =
  { p_hash = text r 0 what
  ; p_ternary = text r 1 what
  ; p_ir = opt_text r 2
  ; p_source = opt_text r 3
  ; p_created_by = opt_text r 4 }

let select_program =
  "SELECT hash, ternary, ir::text, source, created_by::text FROM programs \
   WHERE hash = $1"

let fetch_program p hash =
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  (match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (program_of_row r hash))
  | _ -> store_error "multiple program rows for hash %s" hash )

(* get-or-create by hash: hash is content-addressed so conflicting
   content is impossible by construction; first insert wins for
   created_by, but the ir column REFRESHES when the recompile carries
   provenance: the hash pins the compiled tree, the ir is annotation
   (compiler span fixes must reach already-existing rows — caught by
   acceptance criterion 2, where a pre-fix row's spans were None
   forever).  A ternary-only re-upsert (ir=null) keeps the old ir. *)
let upsert_program p ~hash ~ternary ~source ~ir ~created_by =
  Db.q_unit
    ~params:[ p_str hash
            ; p_str ternary
            ; (match ir with Some s -> V.of_string s | None -> V.null)
            ; p_opt source
            ; p_opt created_by ]
    p
    "INSERT INTO programs (hash, ternary, ir, source, created_by) \
     VALUES ($1, $2, $3::jsonb, $4, $5::uuid) \
     ON CONFLICT (hash) DO UPDATE \
       SET ir = COALESCE(EXCLUDED.ir, programs.ir), \
           source = COALESCE(EXCLUDED.source, programs.source)"
  >>= fun () ->
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  (match rows with
  | [ r ] -> Direct.return (program_of_row r hash)
  | n -> store_error "program upsert: fetch after insert gave %d rows" (List.length n)
         )

(* recent programs newest-first (the /code public face; content-
   addressed, so created_by may be NULL for pre-identity rows) *)
let list_programs p ?(limit = 200) () =
  Db.q ~params:[ p_int limit ] p
    "SELECT hash, ternary, ir::text, source, created_by::text FROM programs \
     ORDER BY created_at DESC LIMIT $1"
  >>= fun rows ->
  Direct.return (List.map (fun r -> program_of_row r "programs") rows)

(* -- runs ------------------------------------------------------------ *)

module Run_status = struct
  type t =
    | Running
    | Normal
    | Loop  (* v1 distinct-work law: a firing re-entered in flight (borg/sharing.borg) *)
    | Fuel_exhausted
    | Size_exhausted
    | Deadline_exceeded  (* wall-clock abort (TUNA_RUN_MAX_SECONDS) *)
    | Error

  let to_string = function
    | Running -> "running"
    | Normal -> "normal"
    | Loop -> "loop"
    | Fuel_exhausted -> "fuel_exhausted"
    | Size_exhausted -> "size_exhausted"
    | Deadline_exceeded -> "deadline_exceeded"
    | Error -> "error"

  let of_string = function
    | "running" -> Running
    | "normal" -> Normal
    | "loop" -> Loop
    | "fuel_exhausted" -> Fuel_exhausted
    | "size_exhausted" -> Size_exhausted
    | "deadline_exceeded" -> Deadline_exceeded
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
; r_semantics : string  (* 'v0' canonical | 'v1' distinct-work (0009) *)
; r_demand_sharing : bool  (* 0011: did this run consult the demand-memo *)
; r_demand_hits : int  (* 0011: firings answered from the shared table *)
; r_denial_count : int  (* 0014: journaled grant denials (grants.invocation) *)
}

let select_run =
  "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
   result_ternary, step_count, status, caller::text, parent_run_id::text, \
   verify_status, created_at::text, semantics, demand_sharing, demand_hits, \
   denial_count \
   FROM runs WHERE id = $1::uuid"

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
  ; r_created_at = opt_text r 12
  ; r_semantics = text r 13 "run.semantics"
  ; r_demand_sharing = bool r 14 "run.demand_sharing"
  ; r_demand_hits = int r 15 "run.demand_hits"
  ; r_denial_count = int r 16 "run.denial_count" }

let insert_run p ~program_hash ?(inputs = []) ?(caller = None) ?(parent_run_id = None)
    ?(semantics = "v0") ~fuel ~size_cap () =
  Db.q
    ~params:[ p_str program_hash
            ; p_text_list inputs
            ; p_int64 (Int64.of_int fuel)
            ; p_int64 (Int64.of_int size_cap)
            ; p_opt caller
            ; p_opt parent_run_id
            ; p_str semantics ]
    p
    "INSERT INTO runs (program_hash, input_hashes, fuel, size_cap, caller, \
     parent_run_id, status, semantics) VALUES ($1, $2, $3, $4, $5::uuid, \
     $6::uuid, 'running', $7) RETURNING id::text"
  >>= fun rows ->
  (match rows with
  | [ r ] -> Direct.return (text r 0 "run.id")
  | n -> store_error "insert_run: RETURNING gave %d rows" (List.length n) )

let update_run_result p ~id ~status ?result_ternary ?step_count
    ?(denial_count = 0) () =
  let result_hash = Option.map Tuna.Hash.hex_of_string result_ternary in
  Db.q_unit
    ~params:[ p_str (Run_status.to_string status)
            ; p_opt result_hash
            ; p_opt result_ternary
            ; (match step_count with Some s -> p_int s | None -> None)
            ; p_int denial_count
            ; p_str id ]
    p
    "UPDATE runs SET status = $1, result_hash = $2, result_ternary = $3, \
     step_count = $4, denial_count = $5 WHERE id = $6::uuid"

(* verify_status update (replay engine writes verified|failed; M7) *)
let update_verify_status p ~id ~verify_status () =
  Db.q_unit
    ~params:[ p_str verify_status; p_str id ]
    p
    "UPDATE runs SET verify_status = $1, verified_at = now() WHERE id = $2::uuid"

(* run ids arrive as full uuids or hex prefixes (notes, links, and
   humans shorten them); resolve a prefix to the single matching run.
   None when malformed, absent, or ambiguous - callers answer 404,
   never a raw store error. *)
let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let resolve_run_id p id =
  let n = String.length id in
  if n = 36 && id.[8] = '-' && id.[13] = '-' && id.[18] = '-' && id.[23] = '-'
     && String.for_all (fun c -> c = '-' || is_hex c) id then
    Direct.return (Some id)
  else if n >= 8 && n <= 32 && String.for_all is_hex id then
    Db.q
      ~params:[ p_str (String.lowercase_ascii id) ]
      p
      "SELECT id::text FROM runs WHERE id::text LIKE $1 || '%' \
       ORDER BY created_at LIMIT 2"
    >>= fun rows ->
    (match rows with
    | [ r ] -> Direct.return (Some (text r 0 "runs.id"))
    | _ -> Direct.return None (* absent or ambiguous: same 404 answer *))
  else Direct.return None
let fetch_run p id =
  Db.q ~params:[ p_str id ] p select_run
  >>= fun rows ->
  (match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (run_of_row r))
  | _ -> store_error "multiple run rows for id %s" id )

(* fetch_run_resolved: fetch_run plus prefix resolution; returns the
   canonical full id beside the row so handlers rebind the id they
   thread to journals, traces, and replay *)
let fetch_run_resolved p id =
  resolve_run_id p id
  >>= function
  | None -> Direct.return None
  | Some full ->
      fetch_run p full
      >>= function
      | None -> Direct.return None (* raced a delete *)
      | Some r -> Direct.return (Some (full, r))

(* list newest-first; caller/program filters optional *)
let list_runs p ?(caller = None) ?(program = None) ?(limit = 50) () =
  Db.q
    ~params:[ p_opt caller; p_opt program; p_int limit ]
    p
     "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
     result_ternary, step_count, status, caller::text, parent_run_id::text, \
     verify_status, created_at::text, semantics, demand_sharing, demand_hits, \
     denial_count \
     FROM runs \
     WHERE ($1::uuid IS NULL OR caller = $1::uuid) \
       AND ($2::text IS NULL OR program_hash = $2) \
     ORDER BY created_at DESC LIMIT $3"
  >>= fun rows -> Direct.return (List.map run_of_row rows)

(* -- demand-memo (borg/sharing.borg §demand-memo, migration 0011) ----

   A CROSS-RUN cache of CLEAN firings only, scoped per caller (the
   own-garden trust option): key (caller, fun_hash, arg_hash) -> the
   answer.  Only firings that never touched a prim are written here,
   so the table is pure-cache by construction -- grant liveness and the
   journal audit are untouched.  Reads are the run's own garden only; a
   miss is a miss, never an error. *)

let demand_memo_get p ~caller ~fun_hash ~arg_hash =
  Db.q
    ~params:[ p_str caller; p_str fun_hash; p_str arg_hash ]
    p
    "SELECT result_ternary FROM demand_memo \
     WHERE caller = $1 AND fun_hash = $2 AND arg_hash = $3"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      (* touch last_seen_at for retention ranking; fire-and-forget *)
      Db.q_unit
        ~params:[ p_str caller; p_str fun_hash; p_str arg_hash ]
        p
        "UPDATE demand_memo SET last_seen_at = now() \
         WHERE caller = $1 AND fun_hash = $2 AND arg_hash = $3"
      >>= fun () -> Direct.return (Some (text r 0 "demand_memo.result_ternary"))
  | _ -> store_error "demand_memo: multiple rows for one key"

let demand_memo_put p ~caller ~fun_hash ~arg_hash ~result_hash
    ~result_ternary =
  Db.q_unit
    ~params:
      [ p_str caller; p_str fun_hash; p_str arg_hash; p_str result_hash
      ; p_str result_ternary ]
    p
    "INSERT INTO demand_memo \
       (caller, fun_hash, arg_hash, result_hash, result_ternary) \
     VALUES ($1, $2, $3, $4, $5) \
     ON CONFLICT (caller, fun_hash, arg_hash) DO UPDATE SET \
       result_hash = EXCLUDED.result_hash, \
       result_ternary = EXCLUDED.result_ternary, last_seen_at = now()"

let demand_memo_count p =
  Db.q p "SELECT count(*) FROM demand_memo"
  >>= function
  | [ r ] -> Direct.return (int r 0 "count")
  | _ -> store_error "demand_memo count: unexpected rows"

(* the caller's whole garden memo (own-garden scope): seed a demand run
   with these, most-recently-seen first, bounded by [limit].  The driver
   preloads them into the run's per-run memo, so the pure core never
   touches the store -- the shared table is a HOST fact. *)
let demand_memo_list p ?(limit = 100_000) ~caller () =
  Db.q
    ~params:[ p_str caller; p_int limit ]
    p
    "SELECT fun_hash, arg_hash, result_ternary FROM demand_memo \
     WHERE caller = $1 ORDER BY last_seen_at DESC LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         ( text r 0 "demand_memo.fun_hash"
         , text r 1 "demand_memo.arg_hash"
         , text r 2 "demand_memo.result_ternary" ))
       rows)

(* record the demand bookkeeping on the run row after execution *)
let update_run_demand p ~id ~demand_sharing ~demand_hits =
  Db.q_unit
    ~params:[ p_bool demand_sharing; p_int64 (Int64.of_int demand_hits); p_str id ]
    p
    "UPDATE runs SET demand_sharing = $1, demand_hits = $2 WHERE id = $3::uuid"

(* -- run traces (borg/trace.borg): opt-in per-firing observability ----

   A trace is a capped event log over a run's counted firings plus a
   summary row.  Recording never affects step counts, fuel, or
   evaluation order; the summary row's presence marks a traced run
   (events are bulk data, GC-able under the journals' retention policy
   when one exists). *)

type trace_summary = {
  t_run_id : string
; t_semantics : string
; t_raw_firings : int  (* firing-gate entries, both laws *)
; t_charged : int  (* counted steps (v0: every firing; v1: distinct) *)
; t_memo_hits : int  (* v1 only *)
; t_dirty_firings : int  (* v1 only *)
; t_loop : bool
; t_recorded : int  (* events actually stored *)
; t_truncated : bool
}

type trace_event = {
  v_seq : int
; v_kind : string  (* fire | charge | dirty | loop *)
; v_rule : string
; v_fun : string  (* 16-hex content-digest prefix *)
; v_arg : string  (* 16-hex content-digest prefix *)
; v_note : string
}

(* 7 bind params per row; 400 rows per statement stays far under the
   server's 65535-parameter ceiling. *)
let trace_rows_per_statement = 400

let insert_run_trace p ~run_id (s : trace_summary) (events : trace_event list)
    =
  let rec take k acc = function
    | [] -> (List.rev acc, [])
    | x :: rest when k = 1 -> (List.rev (x :: acc), rest)
    | x :: rest -> take (k - 1) (x :: acc) rest
  in
  let rec chunks = function
    | [] -> []
    | lst ->
        let head, tail = take trace_rows_per_statement [] lst in
        head :: chunks tail
  in
  let rec insert_chunks = function
    | [] -> Direct.return ()
    | evs :: rest ->
        let params =
          List.concat_map
            (fun e ->
              [ p_str run_id; p_int e.v_seq; p_str e.v_kind; p_str e.v_rule
              ; p_str e.v_fun; p_str e.v_arg; p_str e.v_note ])
            evs
        in
        let row_sql i =
          let base = 7 * i in
          Printf.sprintf "($%d,$%d,$%d,$%d,$%d,$%d,$%d)" (base + 1)
            (base + 2) (base + 3) (base + 4) (base + 5) (base + 6) (base + 7)
        in
        let values = String.concat "," (List.mapi (fun i _ -> row_sql i) evs) in
        Db.q_unit
          ~params p
          (Printf.sprintf
             "INSERT INTO trace_events (run_id, seq, kind, rule, fun_prefix, \
              arg_prefix, note) VALUES %s"
             values)
        >>= fun () -> insert_chunks rest
  in
  (* events first, summary row LAST: a partial insert never advertises
     a trace whose head events are missing *)
  insert_chunks (chunks events)
  >>= fun () ->
  Db.q_unit
    ~params:[
      p_str run_id
    ; p_str s.t_semantics
    ; p_int64 (Int64.of_int s.t_raw_firings)
    ; p_int64 (Int64.of_int s.t_charged)
    ; p_int64 (Int64.of_int s.t_memo_hits)
    ; p_int64 (Int64.of_int s.t_dirty_firings)
    ; p_bool s.t_loop
    ; p_int s.t_recorded
    ; p_bool s.t_truncated ]
    p
    "INSERT INTO run_traces (run_id, semantics, raw_firings, charged, \
     memo_hits, dirty_firings, loop_detected, recorded, truncated) \
     VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9) \
     ON CONFLICT (run_id) DO NOTHING"

let fetch_run_trace p run_id =
  Db.q
    ~params:[ p_str run_id ] p
    "SELECT run_id::text, semantics, raw_firings, charged, memo_hits, \
     dirty_firings, loop_detected, recorded, truncated FROM run_traces \
     WHERE run_id = $1::uuid"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      Direct.return
        (Some
           { t_run_id = text r 0 "run_traces.run_id"
           ; t_semantics = text r 1 "run_traces.semantics"
           ; t_raw_firings = int r 2 "run_traces.raw_firings"
           ; t_charged = int r 3 "run_traces.charged"
           ; t_memo_hits = int r 4 "run_traces.memo_hits"
           ; t_dirty_firings = int r 5 "run_traces.dirty_firings"
           ; t_loop = bool r 6 "run_traces.loop_detected"
           ; t_recorded = int r 7 "run_traces.recorded"
           ; t_truncated = bool r 8 "run_traces.truncated" })
  | _ -> store_error "multiple trace summary rows for run %s" run_id

let fetch_trace_events p run_id ~after_seq ~limit =
  Db.q
    ~params:[ p_str run_id; p_int after_seq; p_int limit ]
    p
    "SELECT seq, kind, rule, fun_prefix, arg_prefix, note FROM trace_events \
     WHERE run_id = $1::uuid AND seq > $2 ORDER BY seq LIMIT $3"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         { v_seq = int r 0 "trace_events.seq"
         ; v_kind = text r 1 "trace_events.kind"
         ; v_rule = text r 2 "trace_events.rule"
         ; v_fun = text r 3 "trace_events.fun_prefix"
         ; v_arg = text r 4 "trace_events.arg_prefix"
         ; v_note = text r 5 "trace_events.note" })
       rows)
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
  >>= fun () -> Direct.return (seq, h)

let fetch_journals p run_id =
  Db.q ~params:[ p_str run_id ] p select_journals
  >>= fun rows -> Direct.return (List.map journal_of_row rows)

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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (dict_entry_of_row r identity_id name))
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
  Direct.return
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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (text r 0 "repl_state.last_run_id"))
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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (text r 0 "prim_kv.key", text r 1 "prim_kv.value"))
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
; g_minted_by : string option  (* the MINTING IDENTITY (0003) *)
; g_parent_grant : string option  (* the grant this row attenuates (0012) *)
; g_revoked_at : string option
}

let select_grant_by_id =
  "SELECT id::text, prim, args_attenuation::text, path_prefix, caller::text, \
   minted_by::text, parent_grant::text, revoked_at::text FROM grants WHERE id = $1::uuid"

let grant_of_row r =
  { g_id = text r 0 "grant.id"
  ; g_prim = text r 1 "grant.prim"
  ; g_args_attenuation = text r 2 "grant.args_attenuation"
  ; g_path_prefix = opt_text r 3
  ; g_caller = text r 4 "grant.caller"
  ; g_minted_by = opt_text r 5
  ; g_parent_grant = opt_text r 6
  ; g_revoked_at = opt_text r 7 }

let mint_grant p ~prim ~args_attenuation ?(path_prefix = None) ~caller
    ?(minted_by = None) ?(parent_grant = None) () =
  Db.q
    ~params:[ p_str prim
            ; p_str args_attenuation
            ; p_opt path_prefix
            ; p_str caller
            ; p_opt minted_by
            ; p_opt parent_grant ]
    p
    "INSERT INTO grants (prim, args_attenuation, path_prefix, caller, minted_by, parent_grant) \
     VALUES ($1, $2::jsonb, $3, $4::uuid, $5::uuid, $6::uuid) RETURNING id::text"
  >>= fun rows ->
  (match rows with
  | [ r ] ->
    let id = text r 0 "grant.id" in
    Db.q ~params:[ p_str id ] p select_grant_by_id
    >>= (function
         | [ r ] -> Direct.return (grant_of_row r)
         | n -> store_error "mint_grant: fetch gave %d rows" (List.length n)
                )
  | n -> store_error "mint_grant: RETURNING gave %d rows" (List.length n) )

(* newest-first listing for the UI admin page (M8) and the admin
   JSON list (GET /api/grants).  ~author scopes to grants written by
   one identity: minted_by, with the caller as the fallback for
   pre-0003 rows that predate the author column. *)
let list_grants p ?(limit = 100) ~author () =
  let select =
    "SELECT id::text, prim, args_attenuation::text, path_prefix, caller::text, \
     minted_by::text, parent_grant::text, revoked_at::text FROM grants"
  in
  let sql, params =
    match author with
    | None ->
        (select ^ " ORDER BY created_at DESC LIMIT $1", [ p_int limit ])
    | Some a ->
        ( select
            ^ " WHERE (minted_by::text = $1 \
               OR (minted_by IS NULL AND caller::text = $1)) \
               ORDER BY created_at DESC LIMIT $2"
        , [ p_str a; p_int limit ] )
  in
  Db.q ~params p sql >>= fun rows -> Direct.return (List.map grant_of_row rows)

let fetch_grant p id =
  Db.q ~params:[ p_str id ] p select_grant_by_id
  >>= fun rows ->
  (match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (grant_of_row r))
  | _ -> store_error "multiple grant rows for id %s" id )

(* immediate, forward-only: next live boundary check fails *)
let revoke_grant p id =
  Db.q_unit
    ~params:[ p_str id ]
    p
    "UPDATE grants SET revoked_at = now() WHERE id = $1::uuid AND revoked_at IS NULL"

(* deny-check at a prim boundary: grant must exist, be unrevoked (as
   must every ANCESTOR in its parent_grant chain — attenuation must not
   escape revocation), and belong to the claiming caller.  Recorded
   runs never re-check (the journal answers, not the grant table).

   M10 path scoping: when the call carries tree paths (the substrate
   prims + ns/fork pass every path it would read or write), the grant's
   path_prefix must cover ALL of them (simple string prefix; NULL
   matches everything).  A path-scoped grant cannot authorize a
   pathless call - the narrowing is the grant. *)
let prefix_match prefix path =
  String.length path >= String.length prefix
  && String.sub path 0 (String.length prefix) = prefix

(* live-lineage walk: the grant and every parent_grant ancestor must be
   unrevoked.  Edges only ever point from a fresh grant to an existing
   one, so the chain is a DAG; the hop cap is belt-and-braces against
   pathological data, not a correctness device. *)
let lineage_live p ~max_hops =
  let rec go hops id =
    if hops > max_hops then Direct.return false
    else
      fetch_grant p id
      >>= function
      | None -> Direct.return false (* dangling lineage = dead *)
      | Some g when g.g_revoked_at <> None -> Direct.return false
      | Some g -> (
          match g.g_parent_grant with
          | None -> Direct.return true
          | Some parent -> go (hops + 1) parent)
  in
  fun id -> go 0 id

let check_grant p ~id ~caller ?(paths = []) () :
  [ `Ok | `Revoked | `Wrong_caller | `Unknown | `Prefix_denied ] =
  fetch_grant p id
  >>= function
  | None -> Direct.return `Unknown
  | Some g when g.g_revoked_at <> None -> Direct.return `Revoked
  | Some g when g.g_caller <> caller -> Direct.return `Wrong_caller
  | Some g -> (
      (* lineage death: an ancestor revoked kills the subtree's future *)
      (match g.g_parent_grant with
       | None -> Direct.return true
       | Some parent -> lineage_live p ~max_hops:64 parent)
      >>= function
      | false -> Direct.return `Revoked
      | true -> (
          match (g.g_path_prefix, paths) with
          | None, _ -> Direct.return `Ok
          | Some _, [] -> Direct.return `Prefix_denied
          | Some prefix, ps ->
              if List.for_all (prefix_match prefix) ps then Direct.return `Ok
              else Direct.return `Prefix_denied))

(* -- delegation-attenuation (grants.borg §delegation-attenuation) ---- *)

(* The narrowing relation, host-side (grants.grant-token: attenuation
   scoping correctness is host policy and stays a review matter).
   [attenuation_narrower ~prim ~args_attenuation ~path_prefix parent]
   answers whether a derived grant with those fields admits
   no-more-than [parent].  An unknown predicate shape or malformed
   JSON in the CHILD is never narrower (the narrowing cannot be proven,
   so refuse); a parent row with an unknown predicate shape is
   un-attenuable from (mint a fresh root instead). *)
let json_obj s =
  try
    match Yojson.Safe.from_string s with
    | `Assoc kvs -> Some kvs
    | `Null -> Some [] (* admit-all, the "null" jsonb spelling *)
    | _ -> None
  with Yojson.Json_error _ -> None (* malformed = not a provable narrowing *)

let admit_all kvs = kvs = []

(* the one interpreted predicate key (run.ml attenuation_ok): a cap on
   the encoded args length.  Anything else is an unknown shape. *)
let max_ternary_of kvs =
  match List.assoc_opt "max_ternary" kvs with
  | Some `Int n -> Some n
  | _ -> None

let attenuation_narrower ~prim ~args_attenuation ~path_prefix
    (parent : grant) : (unit, string) result =
  (* prim: "*" narrows to anything; a named prim only to itself *)
  if parent.g_prim <> "*" && parent.g_prim <> prim then
    Error (Printf.sprintf "prim %S is not a narrowing of %S" prim parent.g_prim)
  else
    (* args predicate *)
    match (json_obj parent.g_args_attenuation, json_obj args_attenuation) with
    | None, _ | _, None ->
        Error "args_attenuation must be JSON (object or null)"
    | Some pkvs, Some ckvs -> (
        let child_shape_ok = admit_all ckvs || max_ternary_of ckvs <> None in
        if admit_all pkvs then
          (* parent admits all: any well-formed known child shape narrows *)
          if child_shape_ok then Ok ()
          else Error "unknown args_attenuation predicate shape"
        else
          match max_ternary_of pkvs with
          | None ->
              Error
                "parent args_attenuation has an unknown predicate shape \
                 (un-attenuable)"
          | Some pmax -> (
              match max_ternary_of ckvs with
              | Some cmax when cmax <= pmax -> Ok ()
              | Some cmax ->
                  Error
                    (Printf.sprintf "max_ternary %d widens parent cap %d" cmax
                       pmax)
              | None ->
                  if admit_all ckvs then
                    Error "admit-all widens a capped parent"
                  else Error "unknown args_attenuation predicate shape"))
    |> fun r ->
    match r with
    | Error e -> Error e
    | Ok () -> (
        (* path: parent NULL covers anything; a prefixed parent only
           admits prefixes OF itself (a NULL child would widen) *)
        match (parent.g_path_prefix, path_prefix) with
        | None, _ -> Ok ()
        | Some _, None -> Error "dropping path_prefix widens the parent"
        | Some pp, Some cp ->
            if prefix_match pp cp then Ok ()
            else
              Error (Printf.sprintf "path prefix %S is not under %S" cp pp))

(* Host-side mint of a narrower grant (delegation-attenuation): only
   the HOLDER of the parent may attenuate it, the parent's lineage must
   be live, and the derived row must be a proven narrowing (relation
   above).  The derived row records BOTH lineage facts: minted_by =
   the minting identity, parent_grant = the parent grant.  No prim
   mints - this is a host API operation only (grants.borg: granting
   from inside the calculus is explicitly excluded in v1). *)
let attenuate_grant p ~parent_id ~prim ~args_attenuation ~path_prefix
    ~caller () :
  [ `Ok of grant
  | `Unknown
  | `Revoked
  | `Wrong_caller
  | `Not_narrower of string ] =
  fetch_grant p parent_id
  >>= function
  | None -> Direct.return `Unknown
  | Some parent when parent.g_revoked_at <> None -> Direct.return `Revoked
  | Some parent when parent.g_caller <> caller -> Direct.return `Wrong_caller
  | Some parent -> (
      (match parent.g_parent_grant with
       | None -> Direct.return true
       | Some ancestor -> lineage_live p ~max_hops:64 ancestor)
      >>= function
      | false -> Direct.return `Revoked (* dead lineage: nothing to narrow *)
      | true -> (
          match
            attenuation_narrower ~prim ~args_attenuation ~path_prefix parent
          with
          | Error reason -> Direct.return (`Not_narrower reason)
          | Ok () ->
              mint_grant p ~prim ~args_attenuation ~path_prefix ~caller
                ~minted_by:(Some caller) ~parent_grant:(Some parent.g_id)
                ()
              >>= fun g -> Direct.return (`Ok g)))

(* lineage surfaces (audit): the ancestor chain (nearest first) and the
   descendant subtree (breadth via recursive CTE, depth 1 = direct
   children).  Bounded the same way as the live walk. *)
let grant_lineage p ?(max_hops = 64) id =
  let rec go hops acc id =
    if hops > max_hops then Direct.return (List.rev acc)
    else
      fetch_grant p id
      >>= function
      | None | Some { g_parent_grant = None; _ } ->
          Direct.return (List.rev acc)
      | Some { g_parent_grant = Some parent; _ } -> (
          fetch_grant p parent
          >>= function
          | None -> Direct.return (List.rev acc)
          | Some g -> go (hops + 1) (g :: acc) parent)
  in
  fetch_grant p id >>= function
  | None -> Direct.return []
  | Some _ -> go 0 [] id

let grant_descendants p ?(max_depth = 64) id =
  Db.q
    ~params:[ p_str id; p_int max_depth ]
    p
    "WITH RECURSIVE tree AS ( \
     SELECT id::text AS id, 1 AS depth FROM grants WHERE parent_grant = $1::uuid \
     UNION ALL \
     SELECT g.id::text, t.depth + 1 FROM grants g JOIN tree t ON g.parent_grant::text = t.id \
     WHERE t.depth < $2 ) \
     SELECT id FROM tree ORDER BY depth, id"
  >>= fun rows ->
  let rec fetch_all acc = function
    | [] -> Direct.return (List.rev acc)
    | id :: rest -> (
        fetch_grant p id
        >>= function
        | None -> fetch_all acc rest
        | Some g -> fetch_all (g :: acc) rest)
  in
  fetch_all [] (List.map (fun r -> text r 0 "grant.id") rows)

(* -- retention (journal.retention-gc, M10) --------------------------- *)

(* GC one run's journal, leaving the CITED TOMBSTONE the book demands:
   journal rows are deleted and the run row carries journal_gced_at +
   journal_gced_policy + verify_status 'gced' — a verifier that looks
   at this run later reports GONE, never VERIFIED (the journal is what
   made verification possible; without it the run is simply
   unverifiable-by-retention, not confirmed).  Runs as one transaction
   so the delete and the tombstone cannot split apart. *)
let gc_journal p ~run_id ~policy () =
  (* simple_query only: a parameterized extended-protocol query cannot
     span BEGIN/COMMIT.  Policy/run-id are short admin-supplied text;
     escape single quotes by doubling. *)
  let esc s = String.concat "''" (String.split_on_char '\'' s) in
  let sql =
    Printf.sprintf
      "BEGIN; \
       DELETE FROM journals WHERE run_id = '%s'::uuid; \
       UPDATE runs SET journal_gced_at = now(), journal_gced_policy = '%s', \
         verify_status = 'gced', verified_at = now() WHERE id = '%s'::uuid; \
       COMMIT;"
      (esc run_id) (esc policy) (esc run_id)
  in
  Db.with_pool p (fun c -> Db.Pg.simple_query c sql)
  >>= fun _ -> fetch_run p run_id

(* Was this run's journal GC'd?  Some policy = GONE (tombstone cited). *)
let fetch_gc_tombstone p run_id =
  Db.q
    ~params:[ p_str run_id ]
    p
    "SELECT journal_gced_policy FROM runs \
     WHERE id = $1::uuid AND journal_gced_at IS NOT NULL"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (opt_text r 0))
  | _ -> store_error "runs: multiple rows for one id"

(* PII redaction is an EXPLICIT chain break (journal.retention-gc): the
   row's payload is replaced by a policy-citing redacted tombstone.
   row_hash is deliberately left stale — the walk fails at that seq,
   which is the visible break.  The run's verify_status is cleared so
   the next fetch re-verifies and surfaces the break (a stale VERIFIED
   would be the silent-delete the book forbids). *)
let redact_journal_row p ~run_id ~seq ~policy () =
  Db.q_unit
    ~params:[ p_str policy; p_str run_id; p_int seq ]
    p
    "UPDATE journals SET result_ternary = NULL, result_hash = NULL, \
       error = 'redacted (chain break): ' || $1 \
     WHERE run_id = $2::uuid AND seq = $3"
  >>= fun () ->
  Db.q_unit
    ~params:[ p_str run_id ]
    p
    "UPDATE runs SET verify_status = NULL, verified_at = NULL \
     WHERE id = $1::uuid"

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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | _ -> store_error "multiple identity rows for id %s" id )

let fetch_identity_by_name p name =
  Db.q ~params:[ p_str name ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE name = $1"
  >>= fun rows ->
  (match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
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
       | Some i -> Direct.return i
       | None -> store_error "bootstrap_identity: insert succeeded but fetch failed"
                 )

(* admin mint (pp-slice T2): a NEW non-admin (default) identity with
   its own bearer token.  The raw token is returned to the caller ONCE
   and never stored (only sha256); the identity row is do-nothing on a
   name collision (ON CONFLICT DO NOTHING) so a duplicate mint cannot
   silently overwrite an existing identity's token — the caller sees
   the existing row's id but the returned token will not verify. *)
let mint_identity p ?(is_admin = false) ~name ~token () =
  Db.q_unit
    ~params:[ p_str name; p_str (Tuna.Hash.hex_of_string token); p_bool is_admin ]
    p
    "INSERT INTO identities (name, token_hash, is_admin) VALUES ($1, $2, $3) \
     ON CONFLICT (name) DO NOTHING"
  >>= fun () ->
  fetch_identity_by_name p name
  >>= (function
       | Some i -> Direct.return i
       | None -> store_error "mint_identity: insert succeeded but fetch failed")

(* the identities roster (admin page/API): never carries token hashes *)
let list_identities p () =
  Db.q p
    "SELECT id::text, name, token_hash, is_admin FROM identities ORDER BY name"
  >>= fun rows ->
  Direct.return (List.map identity_of_row rows)

(* token verify (sha256 lookup) — the only identity query by secret *)
let verify_token p token =
  Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE token_hash = $1"
  >>= fun rows ->
  (match rows with
   | [] -> Direct.return None
   | [ r ] -> Direct.return (Some (identity_of_row r))
   | _ -> Direct.return None)

(* token rotation (design 2026-10-09: rotating your OWN token is
   self-serve for non-admins; another identity's requires the admin
   gate — enforced in the handler, not here).  Swap the sha256 token
   hash in place: the raw token never lands in the store, the caller
   gets it back once, and the previous token stops verifying
   immediately (verify_token is a plain hash lookup).  None on an
   unknown id. *)
let rotate_token p ~identity_id ~token () =
  Db.q ~params:[ p_str identity_id; p_str (Tuna.Hash.hex_of_string token) ] p
    "UPDATE identities SET token_hash = $2 WHERE id = $1::uuid \
     RETURNING id::text, name, token_hash, is_admin"
  >>= fun rows ->
  (match rows with
   | [] -> Direct.return None
   | [ r ] -> Direct.return (Some (identity_of_row r))
   | _ -> store_error "rotate_token: RETURNING gave %d rows" (List.length rows))

(* -- accounts: passwords + browser sessions (0013, pp-slice) -------

   The browser tier over the SAME identities: kind-tagged credentials
   (v1: kind='password', INTERIM sha256$salt$digest — the PP legacy
   format, argon2id named as planned in the honest-limitations tone of
   both READMEs) and opaque session tokens (auth_sessions: only the
   sha256 is stored, expiry + revoke-on-logout, exactly PP's
   mint/lookup/revoke shape on flat 0001-style rows).  Bearer auth
   above is untouched — the agent tier.  auth_log rows run alongside
   so throttling/forensics have data, not guesses. *)

(* Log an auth attempt.  Never fails a login: log rows are telemetry,
   pp-slice has no throttle yet (chapter honest-limitations). *)
let log_auth p ?(identity_id : string option) ~kind ~success () =
  Direct.catch
    (fun () ->
      Db.q_unit
        ~params:[ p_opt identity_id; p_str kind; p_bool success ]
        p
        "INSERT INTO auth_log (identity_id, kind, success) \
         VALUES ($1::uuid, $2, $3)"
      >>= fun () -> Direct.return ())
    (fun _ -> Direct.return ())

(* Set (or rotate) an identity's password.  Upsert: one live password row
   per identity (UNIQUE(identity_id, kind)); each call rehashes with a
   fresh salt — a repeated set is a rotation, and boot-time ensure
   (TUNA_BOOTSTRAP_PASSWORD) rewrites the row identically when the
   operator leaves it alone (the salt churns but the effective secret
   does not). *)
let set_password p ~identity_id ~password =
  let hash = Credentials.hash_password password in
  Db.q_unit
    ~params:[ p_str identity_id; p_str "password"; p_str hash ]
    p
    "INSERT INTO credentials (identity_id, kind, secret_hash) \
     VALUES ($1::uuid, $2, $3) \
     ON CONFLICT (identity_id, kind) \
     DO UPDATE SET secret_hash = EXCLUDED.secret_hash"
  >>= fun () -> Direct.return ()

(* Username+password verify -> identity.  Unknown usernames still pay
   one dummy verify so known/unknown cost the same (timing flatten);
   the row AND the hash check both have to pass, and neither failure
   path names which half failed. *)
let verify_password p ~username ~password =
  fetch_identity_by_name p (String.trim username)
  >>= (function
       | None ->
           let _ =
             Credentials.verify_password password
               ~stored:Credentials.dummy_hash
           in
           log_auth p ~kind:"password" ~success:false ()
           >>= fun () -> Direct.return None
       | Some i ->
           Db.q ~params:[ p_str i.i_id ] p
             "SELECT secret_hash FROM credentials \
              WHERE identity_id = $1::uuid AND kind = 'password'"
           >>= (function
               | [ r ] when
                   Credentials.verify_password password
                     ~stored:(text r 0 "credentials.secret") ->
                   log_auth p ~identity_id:i.i_id ~kind:"password" ~success:true ()
                   >>= fun () ->
                   (* last_used_at is telemetry: a failed touch never
                      fails the login (PP's same keep division) *)
                   Direct.catch
                     (fun () ->
                       Db.q_unit ~params:[ p_str i.i_id ] p
                         "UPDATE credentials SET last_used_at = now() \
                          WHERE identity_id = $1::uuid AND kind = 'password'"
                       >>= fun () -> Direct.return ())
                     (fun _ -> Direct.return ())
                   >>= fun () -> Direct.return (Some i)
               | _ ->
                   (* wrong password, no row, or more rows - one
                      uniform deny, same log shape *)
                   log_auth p ~identity_id:i.i_id ~kind:"password" ~success:false ()
                   >>= fun () -> Direct.return None))

(* Mint an opaque session token for a login.  The raw token leaves the
   process exactly once (cookie value); auth_sessions stores only its
   sha256.  Expiry is the caller's TTL seconds (default at the login
   handler). *)
let mint_session p ~identity_id ~ttl_seconds =
  let token = Credentials.mint_session_token () in
  Db.q_unit
    ~params:
      [ p_str identity_id
      ; p_str (Tuna.Hash.hex_of_string token)
      ; p_int64 (Int64.of_int ttl_seconds) ]
    p
    "INSERT INTO auth_sessions (identity_id, token_hash, expires_at) \
     VALUES ($1::uuid, $2, now() + ($3::text || ' seconds')::interval)"
  >>= fun () ->
  log_auth p ~identity_id ~kind:"session" ~success:true ()
  >>= fun () -> Direct.return token

(* Verify a session cookie value -> identity.  Live = sha256 hit AND
   not revoked AND not expired; everything else is anonymous.  Misses
   are NOT logged: session lookups are per-page-request traffic, log
   rows would dilute (mints and logins carry the accounts story). *)
let verify_session p token =
  if String.length token = 0 then Direct.return None
  else
    Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
      "SELECT i.id::text, i.name, i.token_hash, i.is_admin \
       FROM auth_sessions s JOIN identities i ON i.id = s.identity_id \
       WHERE s.token_hash = $1 AND s.revoked_at IS NULL \
       AND s.expires_at > now() LIMIT 1"
  >>= fun rows ->
    (match rows with
     | [] -> Direct.return None
     | [ r ] -> Direct.return (Some (identity_of_row r))
     | _ -> Direct.return None)

(* Revoke-by-token (logout).  Idempotent: an already-revoked or unknown
   token is still a successful logout from the caller's view. *)
let revoke_session p token =
  Direct.catch
    (fun () ->
      Db.q_unit
        ~params:[ p_str (Tuna.Hash.hex_of_string token) ]
        p
        "UPDATE auth_sessions SET revoked_at = now() \
         WHERE token_hash = $1 AND revoked_at IS NULL"
      >>= fun () -> Direct.return ())
    (fun _ -> Direct.return ())

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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (path_entry_of_row r "tree_paths"))
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
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (text r 0 "tree_values.ternary"))
  | _ -> store_error "tree_values: multiple rows for hash %s" hash

(* -- byte values (M11, migration 0008) --------------------------------

   The BYTE kind of the content-addressed value store
   (borg/byte-values.borg): raw bytes (HTML, JSON, templates, media)
   addressed by sha256 over the exact stored bytes - the same address
   law as tree_values, a different payload kind.  ONE NAMESPACE, TWO
   KINDS: readers probe both stores by hash; kinds do not coalesce, so
   the same content can live in both tables under one hash. *)

let byte_hash = Tuna.Hash.hex_of_string

(* payload cap (byte-values.borg law 3): a journaled denial answer at
   the boundary, never a raised error; default 1 MiB, env
   TUNA_VALUE_MAX_BYTES. *)
let value_max_bytes () =
  match Sys.getenv_opt "TUNA_VALUE_MAX_BYTES" with
  | Some s -> (
      match int_of_string_opt s with
      | Some v when v >= 0 -> v
      | _ -> 1_048_576)
  | None -> 1_048_576

let byte_value_put p ~hash ~bytes =
  Db.q_unit
    ~params:[ p_str hash; V.of_binary bytes ]
    p
    "INSERT INTO byte_values (hash, bytes) VALUES ($1, $2) \
     ON CONFLICT (hash) DO NOTHING"

let byte_value_fetch p hash =
  Db.q ~params:[ p_str hash ] p "SELECT bytes FROM byte_values WHERE hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (V.to_binary_exn (col r 0)))
  | _ -> store_error "byte_values: multiple rows for hash %s" hash

let byte_value_len p hash =
  Db.q ~params:[ p_str hash ] p
    "SELECT octet_length(bytes) FROM byte_values WHERE hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (int64 r 0 "byte_values.len"))
  | _ -> store_error "byte_values: multiple rows for hash %s" hash

(* probe-both-stores resolution (byte-values.borg law 1): the kind is
   whichever store answers; tree_values wins the probe where both
   exist (only possible when the ternary text and the bytes share a
   sha256 - callers that care state the kind). *)
type probe = Tree of string | Bytes of string

let probe_value p hash =
  value_fetch p hash
  >>= function
  | Some ternary -> Direct.return (Some (Tree ternary))
  | None -> (
      byte_value_fetch p hash >>= function
      | Some bytes -> Direct.return (Some (Bytes bytes))
      | None -> Direct.return None)

(* octet length of whichever store holds [hash] (the value/len cheap
   probe: lengths without materializing payloads) *)
let probe_len p hash =
  Db.q ~params:[ p_str hash ] p
    "SELECT octet_length(ternary) FROM tree_values WHERE hash = $1"
  >>= function
  | [ r ] -> Direct.return (Some (int64 r 0 "tree_values.len"))
  | [] -> (
      Db.q ~params:[ p_str hash ] p
        "SELECT octet_length(bytes) FROM byte_values WHERE hash = $1"
      >>= function
      | [ r ] -> Direct.return (Some (int64 r 0 "byte_values.len"))
      | [] -> Direct.return None
      | _ -> store_error "byte_values: multiple rows for hash %s" hash)
  | _ -> store_error "tree_values: multiple rows for hash %s" hash

(* -- law 2 live checks (byte-values.borg: HASH-GATED READS,
   CAPABILITY-GATED WRITES) ------------------------------------------- *)

(* a bare non-uuid actor (tree_ops actors are free text) simply holds
   nothing; cast failures read as false, never raise *)
let is_admin p id =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str id ] p
        "SELECT is_admin FROM identities WHERE id = $1::uuid"
      >>= function
      | [ r ] -> Direct.return (bool r 0 "identity.is_admin")
      | _ -> Direct.return false)
    (fun _ -> Direct.return false)

(* value/put capability: the caller holds at least one unrevoked grant *)
let has_live_grant p caller =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str caller ] p
        "SELECT 1 FROM grants WHERE caller = $1::uuid AND revoked_at IS NULL LIMIT 1"
      >>= (function
            | [] -> Direct.return false
            | _ -> Direct.return true))
    (fun _ -> Direct.return false)

(* M11 route publish/delete capability: an unrevoked grant of [caller]
   whose prefix covers [path] (NULL prefix covers everything) *)
let has_covering_grant p caller path =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str caller ] p
        "SELECT path_prefix FROM grants WHERE caller = $1::uuid AND revoked_at IS NULL"
      >>= fun rows ->
      Direct.return
        (List.exists
           (fun r ->
             match opt_text r 0 with
             | None -> true
             | Some pfx -> prefix_match pfx path)
           rows))
    (fun _ -> Direct.return false)

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
   | [ r ] -> Direct.return (int64 r 0 "tree_paths.version")
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
    | None -> Direct.return `Absent
    | Some _ -> Direct.return `Conflict
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
            | [ r ] -> Direct.return (`Ok (int64 r 0 "tree_paths.version"))
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
            | [ r ] -> Direct.return (`Ok (int64 r 0 "tree_paths.version"))
            | [] -> classify ()
            | n -> store_error "path_put_cas: RETURNING gave %d rows" (List.length n)))

(* route delete (M11 routes.borg): remove a path row; [expected_version]
   pins the optimistic version like path_put_cas (None = unconditional).
   `Ok v carries the version of the row as deleted; `Absent / `Conflict
   are answers, never exceptions. *)
let path_delete p ~path ~expected_version =
  let h = path_hash path in
  match expected_version with
  | None ->
      Db.q ~params:[ p_str h ] p
        "DELETE FROM tree_paths WHERE path_hash = $1 RETURNING version"
      >>= (function
            | [ r ] -> Direct.return (`Ok (int64 r 0 "tree_paths.version"))
            | [] -> Direct.return `Absent
            | n -> store_error "path_delete: RETURNING gave %d rows" (List.length n))
  | Some n ->
      Db.q ~params:[ p_str h; p_int64 n ] p
        "DELETE FROM tree_paths WHERE path_hash = $1 AND version = $2 \
         RETURNING version"
      >>= (function
            | [ r ] -> Direct.return (`Ok (int64 r 0 "tree_paths.version"))
            | [] -> (
                path_get p ~path >>= function
                | None -> Direct.return `Absent
                | Some _ -> Direct.return `Conflict)
            | n -> store_error "path_delete: RETURNING gave %d rows" (List.length n))

(* prefix range scan: every path starting with [prefix], byte-wise
   ordered.  [prefix] must be non-empty (the whole-namespace read is an
   API concern, not a store invariant).  [after] is an optional
   EXCLUSIVE lower bound for windowed reads (board.borg L6): only rows
   with path > after are returned; its default ("" sorts below every
   stored path) makes the predicate always-true, so unwindowed callers
   see the same rows as before the parameter existed. *)
let path_list p ?(limit = 1000) ?(after = "") ~prefix () =
  Db.q
    ~params:[ p_str prefix; p_int limit; p_str after ]
    p
    "SELECT path, value_hash, version, owner, updated_at::text FROM tree_paths \
     WHERE path >= ($1 COLLATE \"C\") \
       AND path < (($1 || chr(255)) COLLATE \"C\") \
       AND path > ($3 COLLATE \"C\") \
     ORDER BY path COLLATE \"C\" LIMIT $2"
  >>= fun rows -> Direct.return (List.map (fun r -> path_entry_of_row r "tree_paths") rows)

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
  >>= fun () -> Direct.return (seq, h)

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
  >>= fun rows -> Direct.return (List.map tree_op_of_row rows)

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
        | [] -> Direct.return n
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
        | [] -> Direct.return copied
        | (np, vh) :: rest ->
            op_append_conn c ~op:"put" ~path:np ~value_hash:(Some vh)
              ~prev_version:None ~version:(Some 1L) ~actor
            >>= fun _ -> log_copies rest
      in
      log_copies copies)

(* -- federation FED2: ops-chain sync (borg/federation.borg) ------------- *)

(* head seq of the whole log (0L for an empty log).  Used by the pull
   surface and, inside a transaction, to assert "this apply's window
   lands exactly on top of the head I fetched" (no intervening append). *)
let ops_head_seq p =
  Db.q p "SELECT COALESCE(MAX(seq), 0) FROM tree_ops"
  >>= function
  | [ r ] -> Direct.return (int64 r 0 "tree_ops.head")
  | _ -> store_error "tree_ops head: unexpected row count"

(* A pulled op plus its TRUE global predecessor hash (the op_hash of
   seq-1; genesis for seq 1).  Filtering a window by prefix makes the
   subsequence non-contiguous, so internal recompute-linkage cannot be
   used; carrying prev_hash per row makes each row independently
   verifiable (recompute sha256(prev_hash ^ concat) == op_hash). *)
type ops_window_row = { w_op : tree_op; w_prev_hash : string }

(* per-row verification over a prefix-filtered (non-contiguous) window:
   each row carries its TRUE global predecessor hash, so recompute each
   row's op_hash independently of the others' presence.  This is the
   check that keeps a prefix-filtered pull tamper-evident (pre-registered
   failure 2: rejects are journaled, never absorbed). *)
let verify_window_rows (rows : ops_window_row list) : [ `Ok | `Bad of string ] =
  let rec go = function
    | [] -> `Ok
    | ({ w_op = o; w_prev_hash } : ops_window_row) :: rest ->
        let h = Tuna.Hash.hex_of_string (w_prev_hash ^ tree_op_concat o) in
        if h <> o.o_op_hash then
          `Bad (Printf.sprintf "seq %Ld: op_hash mismatch" o.o_seq)
        else go rest
  in
  go rows

(* contiguous window [from_seq ..] with each row's global predecessor.
   LAG is computed over the WHOLE log before the range filter so the
   first row still gets its true predecessor (not NULL). *)
let ops_window p ~from_seq ~limit =
  Db.q
    ~params:[ p_int64 from_seq; p_int limit ]
    p
    "SELECT seq, path, op, value_hash, prev_version, version, actor, \
     EXTRACT(epoch FROM ts)::bigint, op_hash, prev_hash FROM ( \
       SELECT seq, path, op, value_hash, prev_version, version, actor, ts, \
              op_hash, COALESCE(LAG(op_hash) OVER (ORDER BY seq), \
                                repeat('0', 64)) AS prev_hash \
       FROM tree_ops) t \
     WHERE seq >= $1 ORDER BY seq LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         { w_op = tree_op_of_row r; w_prev_hash = text r 9 "window.prev_hash" })
       rows)

(* one applied effect row: value_hash + a FRESH version (1 if the
   destination path is first seen, else existing+1).  Returns the new
   version. *)
let fed_apply_effect_conn c ~dst_prefix ~suffix ~value_hash ~owner =
  let path = dst_prefix ^ suffix in
  Db.q_conn
    ~params:[ p_str (path_hash path); p_str path; p_str value_hash; p_str owner ]
    c
    "INSERT INTO tree_paths (path, path_hash, value_hash, version, owner) \
     VALUES ($2, $1, $3, 1, $4) \
     ON CONFLICT (path_hash) DO UPDATE SET value_hash = EXCLUDED.value_hash, \
       version = tree_paths.version + 1, owner = EXCLUDED.owner, \
       updated_at = now() \
     RETURNING version"
  >>= fun rows ->
  (match rows with
   | [ r ] -> Direct.return (int64 r 0 "tree_paths.version")
   | _ -> store_error "fed_apply: RETURNING gave %d rows" (List.length rows))

(* does any of the three value homes hold [hash]?  apply refuses a
   window whose effect rows cite values this peer has not pulled (fetch
   values first, then ops); a dangling index entry is a broken
   substrate.  The probe mirrors FED1's address law: tree_values,
   byte_values, or a compiled program (a program hash resolves as its
   own ternary). *)
let value_present p hash =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str hash ] p
        "SELECT 1 FROM tree_values WHERE hash = $1 \
         UNION ALL SELECT 1 FROM byte_values WHERE hash = $1 \
         UNION ALL SELECT 1 FROM programs WHERE hash = $1 LIMIT 1"
      >>= (function [] -> Direct.return false | _ -> Direct.return true))
    (fun _ -> Direct.return false)

(* Apply a window of (op, true-predecessor-hash) pairs into [dst_prefix].
   Each row is first re-verified against its carried predecessor (so a
   prefix-filtered, non-contiguous window is still tamper-evident), then
   each source op's suffix is its path relative to [src_prefix]; every
   applied row lands in the log (put for effects, cas proof rows for
   reads/denials/fork markers), so the destination log grows by one row
   per applied source op.  Effect rows write tree_paths with a FRESH
   version (first-seen -> 1, re-apply -> version+1): versions are local,
   the value hash is the shared fact.  Refuses a chain-broken row, a
   window that leaves [src_prefix], has non-increasing source seqs, or
   cites a value this peer does not hold (rejects journaled, never
   absorbed).  Atomic: one transaction.  Returns the effect
   (path, source_seq, applied_seq, new_version) rows in source-seq
   order; new_version <> 1 means the source path SHADOWED an existing
   destination path (two names, one path - visible, never merged).  The
   source watermark ([after_seq] on the PULL) is deliberately NOT a
   parameter here: the peer's sequence numbers name source rows, not
   destination rows, and reads journal into the destination log like
   any other op. *)
let fed_apply p ~actor ~dst_prefix ~src_prefix
    (ops : (tree_op * string) list) =
  Db.with_tx p (fun c ->
      if ops = [] then Direct.return (`Applied [])
      else
        let rows = List.map (fun (o, h) -> { w_op = o; w_prev_hash = h }) ops in
        (match verify_window_rows rows with
         | `Bad msg -> Direct.return (`Refused ("window chain broken: " ^ msg))
         | `Ok ->
        let rec check prev = function
          | [] -> Direct.return `Ok
          | ((o : tree_op), _) :: rest ->
              if not (prefix_match src_prefix o.o_path) then
                Direct.return
                  (`Bad
                    (Printf.sprintf "seq %Ld: path %S outside src prefix %S"
                       o.o_seq o.o_path src_prefix))
              else if o.o_seq <= prev then
                Direct.return
                  (`Bad
                    (Printf.sprintf
                       "seq %Ld: window is not strictly increasing" o.o_seq))
              else
                match (o.o_op, o.o_value_hash) with
                | ("put" | "cas"), Some vh ->
                    value_present p vh
                    >>= (function
                          | true -> check o.o_seq rest
                          | false ->
                              Direct.return
                                (`Bad
                                  (Printf.sprintf
                                     "seq %Ld: value %s not held locally (pull \
                                      values before ops)"
                                     o.o_seq vh)))
                | _ -> check o.o_seq rest
        in
        check Int64.min_int ops
        >>= (function
              | `Bad msg -> Direct.return (`Refused msg)
              | `Ok ->
                  let rec apply acc = function
                    | [] -> Direct.return (`Applied (List.rev acc))
                    | ((o : tree_op), _) :: rest ->
                        let suffix =
                          String.sub o.o_path (String.length src_prefix)
                            (String.length o.o_path - String.length src_prefix)
                        in
                        let is_effect =
                          match (o.o_op, o.o_value_hash) with
                          | ("put" | "cas"), Some _ -> true
                          | _ -> false
                        in
                        (if is_effect then
                           fed_apply_effect_conn c ~dst_prefix ~suffix
                             ~value_hash:(Option.get o.o_value_hash) ~owner:actor
                         else Direct.return 1L)
                        >>= fun newv ->
                        op_append_conn c
                          ~op:(if is_effect then "put" else "cas")
                          ~path:(dst_prefix ^ suffix) ~value_hash:o.o_value_hash
                          ~prev_version:None
                          ~version:(if is_effect then Some newv else None)
                          ~actor
                        >>= fun (applied_seq, _op_hash) ->
                        let acc =
                          if is_effect then
                            (dst_prefix ^ suffix, o.o_seq, applied_seq, newv)
                            :: acc
                          else acc
                        in
                        apply acc rest
                  in
                  apply [] ops)))
