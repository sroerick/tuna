(* Tuna_store.Store: scoped hybrid store over lean kernel SQL
   (0001-kernel.sql) + Midden_store for the entire state/product plane.

   Kernel tables (flat SQL, zero Midden lock contention):
     identities, grants (+ structural scope_schema/namespace/projection),
     programs (Base-243 bytes), runs (Base-243 result_bytes), journals,
     derived_journals, demand_memo (Base-243 result_bytes), run_traces,
     trace_events, credentials, auth_sessions, auth_log, runner_tokens,
     oidc_logins, oidc_identities, prim_kv.

   State/product plane (100% Midden — retired legacy tables gone):
     midden_values (unified CAS for Base-243 trees + raw bytes),
     def, def_visibility, repl_session, message (+ midden_cursors),
     retention_policy, cron_run, cron_job, cron_heartbeat, todo
     (+ todo_counts/todo_open/todo_by_created), claim (+ claim_by_kind),
     tree_compat (+ tree_compat_by_path), op_audit (+ op_audit_by_seq). *)

open Direct.Infix

module V = Pgx.Value
module MS = Midden_store.Store
module TS = Tuna_schemas

exception Store_error of string

let store_error fmt = Printf.ksprintf (fun s -> raise (Store_error s)) fmt

(* -- row plumbing ---------------------------------------------------- *)

type param = V.t

let p_str s = V.of_string s
let p_opt o = Option.fold ~some:V.of_string ~none:V.null o
let p_int64 i = V.of_int64 i
let p_int i = V.of_int i
let p_bool b = V.of_bool b
let p_bin b = V.of_binary b
let p_opt_bin o = Option.fold ~some:V.of_binary ~none:V.null o
let p_text_list xs = V.of_list (List.map V.of_string xs)

let col (r : Pgx.row) i = List.nth r i

let text r i what =
  match col r i with
  | Some v -> V.to_string_exn (Some v)
  | None -> store_error "null in column %d (%s)" i what

let opt_text r i = V.to_string (col r i)
let binary r i what =
  match col r i with
  | Some v -> V.to_binary_exn (Some v)
  | None -> store_error "null binary in column %d (%s)" i what
let opt_binary r i = V.to_binary (col r i)
let int64 r i _what = V.to_int64_exn (col r i)
let opt_int64 r i = V.to_int64 (col r i)
let int r i _what = Int64.to_int (int64 r i _what)
let opt_int r i = V.to_int (col r i)
let bool r i _what = V.to_bool_exn (col r i)

let text_list r i what =
  match col r i with
  | None -> store_error "expected array in column %d (%s)" i what
  | Some v -> List.map V.to_string_exn (V.to_list_exn (Some v))

(* -- Base-243 Tpack helpers ------------------------------------------ *)

let tpack_of_ternary ternary =
  match Tuna.Canon.of_string ternary with
  | Ok tree -> Tuna.Tpack.encode tree
  | Error (off, msg) ->
      store_error "invalid ternary tree at offset %d: %s" off msg

let ternary_of_tpack bytes =
  match Tuna.Tpack.decode bytes with
  | Ok tree -> Tuna.Canon.encode tree
  | Error (off, msg) -> store_error "corrupt Base-243 tree bytes at %d: %s" off msg

let ensure_midden p = TS.ensure_all p

let midden_exn what = function
  | Ok v -> v
  | Error e -> store_error "%s: %s" what (MS.string_of_error e)

let fresh_uuid () =
  let ic = open_in_bin "/dev/urandom" in
  let b = Bytes.create 16 in
  really_input ic b 0 16;
  close_in ic;
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let s = Bytes.unsafe_to_string b in
  match Uuidm.of_binary_string s with
  | Some u -> Uuidm.to_string u
  | None -> store_error "fresh_uuid: impossible"

(* Upsert a Midden entity by deterministic key using read-head -> put. *)
let midden_upsert p ~entity ~schema ~author ?reason doc =
  ensure_midden p;
  let m = Db.midden p in
  let rec go retries =
    let prev =
      match midden_exn "midden head" (MS.head m ~entity) with
      | None -> None
      | Some h -> h.head
    in
    match MS.put m ~entity ~prev ~schema ~author ?reason doc with
    | Ok r -> r
    | Error (MS.Conflict _) when retries > 0 -> go (retries - 1)
    | Error e -> store_error "midden_upsert: %s" (MS.string_of_error e)
  in
  go 4

let midden_delete_if_live p ~entity ~author ?reason () =
  ensure_midden p;
  let m = Db.midden p in
  let rec go retries =
    match midden_exn "midden head" (MS.head m ~entity) with
    | None | Some { head = None; _ } -> None
    | Some { head = Some _ as prev; _ } -> (
        match MS.delete m ~entity ~prev ~author ?reason () with
        | Ok r -> Some r
        | Error (MS.Conflict _) when retries > 0 -> go (retries - 1)
        | Error e -> store_error "midden_delete: %s" (MS.string_of_error e))
  in
  go 4

(* -- programs (kernel SQL, Base-243 bytes + Midden CAS mirror) ------- *)

type program = {
  p_hash : string;
  p_ternary : string;
  p_ir : string option;
  p_source : string option;
  p_created_by : string option;
}

let program_of_row r what =
  {
    p_hash = text r 0 what;
    p_ternary = ternary_of_tpack (binary r 1 what);
    p_ir = opt_text r 2;
    p_source = opt_text r 3;
    p_created_by = opt_text r 4;
  }

let select_program =
  "SELECT hash, bytes, ir::text, source, created_by::text FROM programs \
   WHERE hash = $1"

let fetch_program p hash =
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (program_of_row r hash))
  | _ -> store_error "multiple program rows for hash %s" hash

let put_tree_value_blob p ~hash ~bytes =
  Db.q_unit
    ~params:[ p_str hash; p_bin bytes; p_str "program" ]
    p
    "INSERT INTO midden_values (hash, bytes, kind) VALUES ($1, $2, $3) \
     ON CONFLICT (hash) DO NOTHING"

let upsert_program p ~hash ~ternary ~source ~ir ~created_by =
  let bytes = tpack_of_ternary ternary in
  put_tree_value_blob p ~hash ~bytes;
  Db.q_unit
    ~params:
      [ p_str hash;
        p_bin bytes;
        (match ir with Some s -> V.of_string s | None -> V.null);
        p_opt source;
        p_opt created_by ]
    p
    "INSERT INTO programs (hash, bytes, ir, source, created_by) \
     VALUES ($1, $2, $3::jsonb, $4, $5::uuid) \
     ON CONFLICT (hash) DO UPDATE \
       SET ir = COALESCE(EXCLUDED.ir, programs.ir), \
           source = COALESCE(EXCLUDED.source, programs.source)"
  >>= fun () ->
  Db.q ~params:[ p_str hash ] p select_program
  >>= fun rows ->
  match rows with
  | [ r ] -> Direct.return (program_of_row r hash)
  | n -> store_error "program upsert: fetch after insert gave %d rows" (List.length n)

let defs_registry p =
  ensure_midden p;
  Db.q ~params:[] p
    "SELECT name, \"identity-name\" FROM def_view ORDER BY name, \"identity-name\""
  >>= fun rows ->
  Direct.return
    (List.filter_map
       (fun r ->
         match (col r 0, col r 1) with
         | Some n, Some o -> Some (V.to_string_exn (Some n), V.to_string_exn (Some o))
         | _ -> None)
       rows)

let list_programs p ?(limit = 200) () =
  Db.q ~params:[ p_int limit ] p
    "SELECT hash, bytes, ir::text, source, created_by::text FROM programs \
     ORDER BY created_at DESC LIMIT $1"
  >>= fun rows ->
  Direct.return (List.map (fun r -> program_of_row r "programs") rows)

(* -- runs (kernel SQL, Base-243 result_bytes) ------------------------ *)

module Run_status = struct
  type t =
    | Running
    | Normal
    | Loop
    | Fuel_exhausted
    | Size_exhausted
    | Deadline_exceeded
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
  r_id : string;
  r_program_hash : string;
  r_input_hashes : string list;
  r_fuel : int;
  r_size_cap : int;
  r_result_hash : string option;
  r_result_ternary : string option;
  r_step_count : int option;
  r_status : Run_status.t;
  r_caller : string option;
  r_parent_run_id : string option;
  r_verify_status : string option;
  r_created_at : string option;
  r_semantics : string;
  r_demand_sharing : bool;
  r_demand_hits : int;
  r_denial_count : int;
}

let select_run =
  "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
   result_bytes, step_count, status, caller::text, parent_run_id::text, \
   verify_status, created_at::text, semantics, demand_sharing, demand_hits, \
   denial_count \
   FROM runs WHERE id = $1::uuid"

let run_of_row r =
  {
    r_id = text r 0 "run.id";
    r_program_hash = text r 1 "run.program_hash";
    r_input_hashes = text_list r 2 "run.input_hashes";
    r_fuel = int r 3 "run.fuel";
    r_size_cap = int r 4 "run.size_cap";
    r_result_hash = opt_text r 5;
    r_result_ternary = Option.map ternary_of_tpack (opt_binary r 6);
    r_step_count = opt_int r 7;
    r_status = Run_status.of_string (text r 8 "run.status");
    r_caller = opt_text r 9;
    r_parent_run_id = opt_text r 10;
    r_verify_status = opt_text r 11;
    r_created_at = opt_text r 12;
    r_semantics = text r 13 "run.semantics";
    r_demand_sharing = bool r 14 "run.demand_sharing";
    r_demand_hits = int r 15 "run.demand_hits";
    r_denial_count = int r 16 "run.denial_count";
  }

let insert_run p ~program_hash ?(inputs = []) ?(caller = None) ?(parent_run_id = None)
    ?(semantics = "v0") ~fuel ~size_cap () =
  Db.q
    ~params:
      [ p_str program_hash;
        p_text_list inputs;
        p_int64 (Int64.of_int fuel);
        p_int64 (Int64.of_int size_cap);
        p_opt caller;
        p_opt parent_run_id;
        p_str semantics ]
    p
    "INSERT INTO runs (program_hash, input_hashes, fuel, size_cap, caller, \
     parent_run_id, status, semantics) VALUES ($1, $2, $3, $4, $5::uuid, \
     $6::uuid, 'running', $7) RETURNING id::text"
  >>= fun rows ->
  match rows with
  | [ r ] -> Direct.return (text r 0 "run.id")
  | n -> store_error "insert_run: RETURNING gave %d rows" (List.length n)

let update_run_result p ~id ~status ?result_ternary ?step_count
    ?(denial_count = 0) () =
  let result_bytes = Option.map tpack_of_ternary result_ternary in
  let result_hash = Option.map Tuna.Hash.hex_of_tpack result_bytes in
  Db.q_unit
    ~params:
      [ p_str (Run_status.to_string status);
        p_opt result_hash;
        p_opt_bin result_bytes;
        (match step_count with Some s -> p_int s | None -> None);
        p_int denial_count;
        p_str id ]
    p
    "UPDATE runs SET status = $1, result_hash = $2, result_bytes = $3, \
     step_count = $4, denial_count = $5 WHERE id = $6::uuid"

let update_verify_status p ~id ~verify_status () =
  Db.q_unit
    ~params:[ p_str verify_status; p_str id ]
    p
    "UPDATE runs SET verify_status = $1, verified_at = now() WHERE id = $2::uuid"

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
    match rows with
    | [ r ] -> Direct.return (Some (text r 0 "runs.id"))
    | _ -> Direct.return None
  else Direct.return None

let fetch_run p id =
  Db.q ~params:[ p_str id ] p select_run
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (run_of_row r))
  | _ -> store_error "multiple run rows for id %s" id

let fetch_run_resolved p id =
  resolve_run_id p id
  >>= function
  | None -> Direct.return None
  | Some full -> (
      fetch_run p full >>= function
      | None -> Direct.return None
      | Some r -> Direct.return (Some (full, r)))

let list_runs p ?(caller = None) ?(program = None) ?(limit = 50) () =
  Db.q
    ~params:[ p_opt caller; p_opt program; p_int limit ]
    p
    "SELECT id::text, program_hash, input_hashes, fuel, size_cap, result_hash, \
     result_bytes, step_count, status, caller::text, parent_run_id::text, \
     verify_status, created_at::text, semantics, demand_sharing, demand_hits, \
     denial_count \
     FROM runs \
     WHERE ($1::uuid IS NULL OR caller = $1::uuid) \
       AND ($2::text IS NULL OR program_hash = $2) \
     ORDER BY created_at DESC LIMIT $3"
  >>= fun rows -> Direct.return (List.map run_of_row rows)

(* -- demand-memo (kernel SQL, Base-243 result_bytes) ----------------- *)

let demand_memo_get p ~caller ~fun_hash ~arg_hash =
  Db.q
    ~params:[ p_str caller; p_str fun_hash; p_str arg_hash ]
    p
    "SELECT result_bytes FROM demand_memo \
     WHERE caller = $1 AND fun_hash = $2 AND arg_hash = $3"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      Db.q_unit
        ~params:[ p_str caller; p_str fun_hash; p_str arg_hash ]
        p
        "UPDATE demand_memo SET last_seen_at = now() \
         WHERE caller = $1 AND fun_hash = $2 AND arg_hash = $3"
      >>= fun () ->
      Direct.return (Some (ternary_of_tpack (binary r 0 "demand_memo.result_bytes")))
  | _ -> store_error "demand_memo: multiple rows for one key"

let demand_memo_put p ~caller ~fun_hash ~arg_hash ~result_hash ~result_ternary =
  let bytes = tpack_of_ternary result_ternary in
  Db.q_unit
    ~params:
      [ p_str caller; p_str fun_hash; p_str arg_hash; p_str result_hash;
        p_bin bytes ]
    p
    "INSERT INTO demand_memo \
       (caller, fun_hash, arg_hash, result_hash, result_bytes) \
     VALUES ($1, $2, $3, $4, $5) \
     ON CONFLICT (caller, fun_hash, arg_hash) DO UPDATE SET \
       result_hash = EXCLUDED.result_hash, \
       result_bytes = EXCLUDED.result_bytes, last_seen_at = now()"

let demand_memo_count p =
  Db.q p "SELECT count(*) FROM demand_memo"
  >>= function
  | [ r ] -> Direct.return (int r 0 "count")
  | _ -> store_error "demand_memo count: unexpected rows"

let demand_memo_list p ?(limit = 100_000) ~caller () =
  Db.q
    ~params:[ p_str caller; p_int limit ]
    p
    "SELECT fun_hash, arg_hash, result_bytes FROM demand_memo \
     WHERE caller = $1 ORDER BY last_seen_at DESC LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         ( text r 0 "demand_memo.fun_hash",
           text r 1 "demand_memo.arg_hash",
           ternary_of_tpack (binary r 2 "demand_memo.result_bytes") ))
       rows)

let update_run_demand p ~id ~demand_sharing ~demand_hits =
  Db.q_unit
    ~params:[ p_bool demand_sharing; p_int64 (Int64.of_int demand_hits); p_str id ]
    p
    "UPDATE runs SET demand_sharing = $1, demand_hits = $2 WHERE id = $3::uuid"

(* -- run traces (kernel SQL) ----------------------------------------- *)

type trace_summary = {
  t_run_id : string;
  t_semantics : string;
  t_raw_firings : int;
  t_charged : int;
  t_memo_hits : int;
  t_dirty_firings : int;
  t_loop : bool;
  t_recorded : int;
  t_truncated : bool;
}

type trace_event = {
  v_seq : int;
  v_kind : string;
  v_rule : string;
  v_fun : string;
  v_arg : string;
  v_note : string;
}

let trace_rows_per_statement = 400

let insert_run_trace p ~run_id (s : trace_summary) (events : trace_event list) =
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
              [ p_str run_id; p_int e.v_seq; p_str e.v_kind; p_str e.v_rule;
                p_str e.v_fun; p_str e.v_arg; p_str e.v_note ])
            evs
        in
        let row_sql i =
          let base = 7 * i in
          Printf.sprintf "($%d,$%d,$%d,$%d,$%d,$%d,$%d)" (base + 1)
            (base + 2) (base + 3) (base + 4) (base + 5) (base + 6) (base + 7)
        in
        let values = String.concat "," (List.mapi (fun i _ -> row_sql i) evs) in
        Db.q_unit ~params p
          (Printf.sprintf
             "INSERT INTO trace_events (run_id, seq, kind, rule, fun_prefix, \
              arg_prefix, note) VALUES %s"
             values)
        >>= fun () -> insert_chunks rest
  in
  insert_chunks (chunks events)
  >>= fun () ->
  Db.q_unit
    ~params:
      [ p_str run_id;
        p_str s.t_semantics;
        p_int64 (Int64.of_int s.t_raw_firings);
        p_int64 (Int64.of_int s.t_charged);
        p_int64 (Int64.of_int s.t_memo_hits);
        p_int64 (Int64.of_int s.t_dirty_firings);
        p_bool s.t_loop;
        p_int s.t_recorded;
        p_bool s.t_truncated ]
    p
    "INSERT INTO run_traces (run_id, semantics, raw_firings, charged, \
     memo_hits, dirty_firings, loop_detected, recorded, truncated) \
     VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9) \
     ON CONFLICT (run_id) DO NOTHING"

let fetch_run_trace p run_id =
  Db.q ~params:[ p_str run_id ] p
    "SELECT run_id::text, semantics, raw_firings, charged, memo_hits, \
     dirty_firings, loop_detected, recorded, truncated FROM run_traces \
     WHERE run_id = $1::uuid"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      Direct.return
        (Some
           {
             t_run_id = text r 0 "run_traces.run_id";
             t_semantics = text r 1 "run_traces.semantics";
             t_raw_firings = int r 2 "run_traces.raw_firings";
             t_charged = int r 3 "run_traces.charged";
             t_memo_hits = int r 4 "run_traces.memo_hits";
             t_dirty_firings = int r 5 "run_traces.dirty_firings";
             t_loop = bool r 6 "run_traces.loop_detected";
             t_recorded = int r 7 "run_traces.recorded";
             t_truncated = bool r 8 "run_traces.truncated";
           })
  | _ -> store_error "multiple trace summary rows for run %s" run_id

let fetch_trace_events p run_id ~after_seq ~limit =
  Db.q ~params:[ p_str run_id; p_int after_seq; p_int limit ] p
    "SELECT seq, kind, rule, fun_prefix, arg_prefix, note FROM trace_events \
     WHERE run_id = $1::uuid AND seq > $2 ORDER BY seq LIMIT $3"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         {
           v_seq = int r 0 "trace_events.seq";
           v_kind = text r 1 "trace_events.kind";
           v_rule = text r 2 "trace_events.rule";
           v_fun = text r 3 "trace_events.fun_prefix";
           v_arg = text r 4 "trace_events.arg_prefix";
           v_note = text r 5 "trace_events.note";
         })
       rows)

(* -- journals (kernel SQL) ------------------------------------------- *)

type journal_event = {
  e_callsite_path : string;
  e_prim : string;
  e_prim_contract : string;
  e_grant_id : string option;
  e_args_ternary : string option;
  e_result_ternary : string option;
  e_error : string option;
  e_wall_ms : int option;
}

type journal = {
  j_run_id : string;
  j_seq : int;
  j_callsite_path : string;
  j_prim : string;
  j_prim_contract : string;
  j_grant_id : string option;
  j_args_ternary : string option;
  j_args_hash : string option;
  j_result_ternary : string option;
  j_result_hash : string option;
  j_error : string option;
  j_wall_ms : int option;
  j_host_build : string;
  j_prev_hash : string;
  j_row_hash : string;
}

let genesis = String.make 64 '0'

let fingerprint run_id seq ev prev_hash =
  let field = function Some s -> s | None -> "" in
  String.concat "\x1f"
    [ run_id;
      string_of_int seq;
      ev.e_callsite_path;
      ev.e_prim;
      ev.e_prim_contract;
      field ev.e_grant_id;
      field ev.e_args_ternary;
      field ev.e_result_ternary;
      field ev.e_error;
      (match ev.e_wall_ms with Some w -> string_of_int w | None -> "");
      prev_hash ]

let row_hash run_id seq ev prev_hash =
  Tuna.Hash.hex_of_string (fingerprint run_id seq ev prev_hash)

let select_journals =
  "SELECT run_id::text, seq, callsite_path, prim, prim_contract, \
   grant_id::text, args_ternary, args_hash, result_ternary, result_hash, \
   error, wall_ms, host_build, prev_hash, row_hash FROM journals \
   WHERE run_id = $1::uuid ORDER BY seq"

let journal_of_row r =
  {
    j_run_id = text r 0 "journal.run_id";
    j_seq = int r 1 "journal.seq";
    j_callsite_path = text r 2 "journal.callsite_path";
    j_prim = text r 3 "journal.prim";
    j_prim_contract = text r 4 "journal.prim_contract";
    j_grant_id = opt_text r 5;
    j_args_ternary = opt_text r 6;
    j_args_hash = opt_text r 7;
    j_result_ternary = opt_text r 8;
    j_result_hash = opt_text r 9;
    j_error = opt_text r 10;
    j_wall_ms = opt_int r 11;
    j_host_build = text r 12 "journal.host_build";
    j_prev_hash = text r 13 "journal.prev_hash";
    j_row_hash = text r 14 "journal.row_hash";
  }

let append_journal p ?(host_build = "tuna-dev") ~run_id ev =
  Db.q ~params:[ p_str run_id ] p
    "SELECT seq, row_hash FROM journals WHERE run_id = $1::uuid ORDER BY seq DESC LIMIT 1"
  >>= fun last ->
  let seq, prev_hash =
    match last with
    | [] -> (0, genesis)
    | [ r ] -> (int r 0 "last.seq" + 1, text r 1 "last.row_hash")
    | _ -> store_error "journal head: multiple rows"
  in
  let args_hash = Option.map Tuna.Hash.hex_of_trits ev.e_args_ternary in
  let result_hash = Option.map Tuna.Hash.hex_of_trits ev.e_result_ternary in
  let h = row_hash run_id seq ev prev_hash in
  Db.q_unit
    ~params:
      [ p_str run_id;
        p_int seq;
        p_str ev.e_callsite_path;
        p_str ev.e_prim;
        p_str ev.e_prim_contract;
        p_opt ev.e_grant_id;
        p_opt ev.e_args_ternary;
        p_opt args_hash;
        p_opt ev.e_result_ternary;
        p_opt result_hash;
        p_opt ev.e_error;
        (match ev.e_wall_ms with Some w -> p_int w | None -> None);
        p_str host_build;
        p_str prev_hash;
        p_str h ]
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

let insert_derived_journal p ~run_id ~parent_run_id () =
  Db.q_unit ~params:[ p_str run_id; p_str parent_run_id ] p
    "INSERT INTO derived_journals (run_id, parent_run_id) \
     VALUES ($1::uuid, $2::uuid) ON CONFLICT (run_id) DO NOTHING"

let verify_chain (js : journal list) : [ `Ok | `Bad of string ] =
  let rec go expected_prev = function
    | [] -> `Ok
    | j :: rest ->
        if j.j_prev_hash <> expected_prev then
          `Bad (Printf.sprintf "seq %d: prev_hash mismatch" j.j_seq)
        else
          let h =
            row_hash j.j_run_id j.j_seq
              {
                e_callsite_path = j.j_callsite_path;
                e_prim = j.j_prim;
                e_prim_contract = j.j_prim_contract;
                e_grant_id = j.j_grant_id;
                e_args_ternary = j.j_args_ternary;
                e_result_ternary = j.j_result_ternary;
                e_error = j.j_error;
                e_wall_ms = j.j_wall_ms;
              }
              j.j_prev_hash
          in
          if h <> j.j_row_hash then
            `Bad (Printf.sprintf "seq %d: row_hash mismatch" j.j_seq)
          else go h rest
  in
  go genesis js

(* -- identities (kernel SQL; forward decls used by Midden state) ----- *)

type identity = {
  i_id : string;
  i_name : string;
  i_token_hash : string;
  i_is_admin : bool;
}

let identity_of_row r =
  {
    i_id = text r 0 "identity.id";
    i_name = text r 1 "identity.name";
    i_token_hash = text r 2 "identity.token_hash";
    i_is_admin = bool r 3 "identity.is_admin";
  }

let select_identity_by_id =
  "SELECT id::text, name, token_hash, is_admin FROM identities WHERE id = $1::uuid"

let fetch_identity p id =
  Db.q ~params:[ p_str id ] p select_identity_by_id
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | _ -> store_error "multiple identity rows for id %s" id

let fetch_identity_by_name p name =
  Db.q ~params:[ p_str name ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE name = $1"
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | _ -> store_error "multiple identity rows for name %s" name

let bootstrap_identity p ?(is_admin = true) ~name ~token () =
  Db.q_unit
    ~params:[ p_str name; p_str (Tuna.Hash.hex_of_string token); p_bool is_admin ]
    p
    "INSERT INTO identities (name, token_hash, is_admin) VALUES ($1, $2, $3) \
     ON CONFLICT (name) DO NOTHING"
  >>= fun () ->
  fetch_identity_by_name p name
  >>= function
  | Some i -> Direct.return i
  | None -> store_error "bootstrap_identity: insert succeeded but fetch failed"

(* -- unified value store over midden_values -------------------------- *)

let value_put p ~hash ~ternary =
  let bytes = tpack_of_ternary ternary in
  put_tree_value_blob p ~hash ~bytes

let value_fetch p hash =
  Db.q ~params:[ p_str hash ] p
    "SELECT bytes, kind FROM midden_values WHERE hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      let kind = text r 1 "midden_values.kind" in
      if kind = "program" then
        Direct.return (Some (ternary_of_tpack (binary r 0 "midden_values.bytes")))
      else Direct.return None
  | _ -> store_error "midden_values: multiple rows for hash %s" hash

let byte_hash = Tuna.Hash.hex_of_string

let value_max_bytes () =
  match Sys.getenv_opt "TUNA_VALUE_MAX_BYTES" with
  | Some s -> (
      match int_of_string_opt s with Some v when v >= 0 -> v | _ -> 1_048_576)
  | None -> 1_048_576

let byte_value_put p ~hash ~bytes =
  let h = midden_exn "put_blob" (MS.put_blob (Db.midden p) bytes) in
  if not (String.equal h hash) then
    store_error "byte_value_put: hash mismatch (%s vs %s)" hash h

let byte_value_fetch p hash =
  Direct.return (midden_exn "get_blob" (MS.get_blob (Db.midden p) hash))

let byte_value_len p hash =
  Direct.return
    (Option.map Int64.of_int
       (midden_exn "blob_len" (MS.blob_len (Db.midden p) hash)))

type probe = Tree of string | Bytes of string

let probe_value p hash =
  Db.q ~params:[ p_str hash ] p
    "SELECT bytes, kind FROM midden_values WHERE hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      let bytes = binary r 0 "midden_values.bytes" in
      let kind = text r 1 "midden_values.kind" in
      if kind = "program" then Direct.return (Some (Tree (ternary_of_tpack bytes)))
      else Direct.return (Some (Bytes bytes))
  | _ -> store_error "midden_values: multiple rows for hash %s" hash

let probe_len p hash =
  Db.q ~params:[ p_str hash ] p
    "SELECT bytes, kind FROM midden_values WHERE hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      let bytes = binary r 0 "midden_values.bytes" in
      let kind = text r 1 "midden_values.kind" in
      if kind = "program" then
        Direct.return (Some (Int64.of_int (String.length (ternary_of_tpack bytes))))
      else Direct.return (Some (Int64.of_int (String.length bytes)))
  | _ -> store_error "midden_values: multiple rows for hash %s" hash

(* -- REPL dictionary & session state (Midden def & repl_session) ----- *)

type dict_entry = {
  d_identity : string;
  d_name : string;
  d_ternary : string;
  d_updated_at : string option;
}

let def_entity_id ~identity_id ~name =
  Midden.Entity_id.of_key ~namespace:"def" (identity_id ^ "/" ^ name)

let dict_get p ~identity_id ~name =
  ensure_midden p;
  Db.q ~params:[ p_str identity_id; p_str name ] p
    "SELECT d.\"identity-id\", v.bytes, d.at::text \
     FROM def_view d JOIN midden_values v ON v.hash = d.\"value-hash\" \
     WHERE d.\"identity-id\" = $1 AND d.name = $2"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      Direct.return
        (Some
           {
             d_identity = identity_id;
             d_name = name;
             d_ternary = ternary_of_tpack (binary r 1 ("def." ^ name));
             d_updated_at = opt_text r 2;
           })
  | _ -> store_error "def_view: multiple rows for %s/%s" identity_id name

let dict_set p ~identity_id ~name ~ternary =
  ensure_midden p;
  let bytes = tpack_of_ternary ternary in
  let value_hash = Tuna.Hash.hex_of_tpack bytes in
  put_tree_value_blob p ~hash:value_hash ~bytes;
  let identity_name =
    match fetch_identity p identity_id with
    | Some i -> i.i_name
    | None -> identity_id
  in
  let entity = def_entity_id ~identity_id ~name in
  let doc =
    TS.record
      [ TS.req_str "identity-id" identity_id;
        TS.req_str "identity-name" identity_name;
        TS.req_str "name" name;
        TS.req_str "value-hash" value_hash ]
  in
  ignore
    (midden_upsert p ~entity ~schema:TS.def_hash ~author:identity_id
       ~reason:("def:" ^ name) doc)

let dict_del p ~identity_id ~name =
  let entity = def_entity_id ~identity_id ~name in
  ignore
    (midden_delete_if_live p ~entity ~author:identity_id
       ~reason:("undef:" ^ name) ())

let dict_list p ~identity_id =
  ensure_midden p;
  Db.q ~params:[ p_str identity_id ] p
    "SELECT d.name, v.bytes, d.at::text \
     FROM def_view d JOIN midden_values v ON v.hash = d.\"value-hash\" \
     WHERE d.\"identity-id\" = $1 ORDER BY d.name"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         let name = text r 0 "def_view.name" in
         {
           d_identity = identity_id;
           d_name = name;
           d_ternary = ternary_of_tpack (binary r 1 ("def_view." ^ name));
           d_updated_at = opt_text r 2;
         })
       rows)

let repl_session_entity_id ~identity_id =
  Midden.Entity_id.of_key ~namespace:"repl_session" identity_id

let repl_state_get p ~identity_id =
  ensure_midden p;
  Db.q ~params:[ p_str identity_id ] p
    "SELECT \"last-run-id\" FROM repl_session WHERE \"identity-id\" = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (text r 0 "repl_session.last-run-id"))
  | _ -> store_error "repl_session: multiple rows for %s" identity_id

let repl_state_put p ~identity_id ~run_id =
  let entity = repl_session_entity_id ~identity_id in
  let doc =
    TS.record
      [ TS.req_str "identity-id" identity_id;
        TS.req_str "last-run-id" run_id ]
  in
  ignore
    (midden_upsert p ~entity ~schema:TS.repl_session_hash ~author:identity_id
       ~reason:"repl_state" doc)

(* -- prim kv (kernel SQL) -------------------------------------------- *)

let prim_get p key_ternary =
  let kh = Tuna.Hash.hex_of_trits key_ternary in
  Db.q ~params:[ p_str kh ] p
    "SELECT key_ternary, value_ternary FROM prim_kv WHERE key_hash = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] ->
      Direct.return (Some (text r 0 "prim_kv.key", text r 1 "prim_kv.value"))
  | _ -> store_error "prim_kv: multiple rows for key %s" kh

let prim_put p ~key_ternary ~value_ternary =
  let kh = Tuna.Hash.hex_of_trits key_ternary in
  Db.q_unit ~params:[ p_str kh; p_str key_ternary; p_str value_ternary ] p
    "INSERT INTO prim_kv (key_hash, key_ternary, value_ternary) \
     VALUES ($1, $2, $3) \
     ON CONFLICT (key_hash) DO UPDATE SET value_ternary = EXCLUDED.value_ternary, \
     updated_at = now()"

(* -- grants (kernel SQL + structural Midden scope columns) ----------- *)

type grant = {
  g_id : string;
  g_prim : string;
  g_args_attenuation : string;
  g_path_prefix : string option;
  g_scope_schema : string option;
  g_scope_namespace : string option;
  g_scope_projection : string option;
  g_caller : string;
  g_minted_by : string option;
  g_parent_grant : string option;
  g_revoked_at : string option;
  g_created_at : string;
}

let select_grant_by_id =
  "SELECT id::text, prim, args_attenuation::text, path_prefix, scope_schema, \
   scope_namespace, scope_projection, caller::text, minted_by::text, \
   parent_grant::text, revoked_at::text, created_at::text \
   FROM grants WHERE id = $1::uuid"

let grant_of_row r =
  {
    g_id = text r 0 "grant.id";
    g_prim = text r 1 "grant.prim";
    g_args_attenuation = text r 2 "grant.args_attenuation";
    g_path_prefix = opt_text r 3;
    g_scope_schema = opt_text r 4;
    g_scope_namespace = opt_text r 5;
    g_scope_projection = opt_text r 6;
    g_caller = text r 7 "grant.caller";
    g_minted_by = opt_text r 8;
    g_parent_grant = opt_text r 9;
    g_revoked_at = opt_text r 10;
    g_created_at = text r 11 "grant.created_at";
  }

let mint_grant p ~prim ~args_attenuation ?(path_prefix = None)
    ?(scope_schema = None) ?(scope_namespace = None) ?(scope_projection = None)
    ~caller ?(minted_by = None) ?(parent_grant = None) () =
  Db.q
    ~params:
      [ p_str prim;
        p_str args_attenuation;
        p_opt path_prefix;
        p_opt scope_schema;
        p_opt scope_namespace;
        p_opt scope_projection;
        p_str caller;
        p_opt minted_by;
        p_opt parent_grant ]
    p
    "INSERT INTO grants (prim, args_attenuation, path_prefix, scope_schema, \
     scope_namespace, scope_projection, caller, minted_by, parent_grant) \
     VALUES ($1, $2::jsonb, $3, $4, $5, $6, $7::uuid, $8::uuid, $9::uuid) \
     RETURNING id::text"
  >>= fun rows ->
  match rows with
  | [ r ] -> (
      let id = text r 0 "grant.id" in
      Db.q ~params:[ p_str id ] p select_grant_by_id
      >>= function
      | [ r ] -> Direct.return (grant_of_row r)
      | n -> store_error "mint_grant: fetch gave %d rows" (List.length n))
  | n -> store_error "mint_grant: RETURNING gave %d rows" (List.length n)

let list_grants p ?(limit = 100) ~author () =
  let select =
    "SELECT id::text, prim, args_attenuation::text, path_prefix, scope_schema, \
     scope_namespace, scope_projection, caller::text, minted_by::text, \
     parent_grant::text, revoked_at::text, created_at::text FROM grants"
  in
  let sql, params =
    match author with
    | None -> (select ^ " ORDER BY created_at DESC LIMIT $1", [ p_int limit ])
    | Some a ->
        ( select
          ^ " WHERE (minted_by::text = $1 \
             OR (minted_by IS NULL AND caller::text = $1)) \
             ORDER BY created_at DESC LIMIT $2",
          [ p_str a; p_int limit ] )
  in
  Db.q ~params p sql >>= fun rows -> Direct.return (List.map grant_of_row rows)

let fetch_grant p id =
  Db.q ~params:[ p_str id ] p select_grant_by_id
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (grant_of_row r))
  | _ -> store_error "multiple grant rows for id %s" id

let revoke_grant p id =
  Db.q_unit ~params:[ p_str id ] p
    "UPDATE grants SET revoked_at = now() WHERE id = $1::uuid AND revoked_at IS NULL"

let prefix_match prefix path =
  String.length path >= String.length prefix
  && String.sub path 0 (String.length prefix) = prefix

let lineage_live p ~max_hops =
  let rec go hops id =
    if hops > max_hops then Direct.return false
    else
      fetch_grant p id
      >>= function
      | None -> Direct.return false
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
      (match g.g_parent_grant with
      | None -> Direct.return true
      | Some parent -> lineage_live p ~max_hops:64 parent)
      >>= function
      | false -> Direct.return `Revoked
      | true -> (
          match (g.g_path_prefix, paths) with
          | None, [] -> Direct.return `Ok
          | None, _ -> Direct.return `Prefix_denied
          | Some _, [] -> Direct.return `Prefix_denied
          | Some prefix, ps ->
              if List.for_all (prefix_match prefix) ps then Direct.return `Ok
              else Direct.return `Prefix_denied))

let check_midden_grant p ~id ~caller ?schema ?(namespace = "local") ?projection () :
    [ `Ok | `Revoked | `Wrong_caller | `Unknown | `Scope_denied ] =
  fetch_grant p id
  >>= function
  | None -> Direct.return `Unknown
  | Some g when g.g_revoked_at <> None -> Direct.return `Revoked
  | Some g when g.g_caller <> caller -> Direct.return `Wrong_caller
  | Some g -> (
      (match g.g_parent_grant with
      | None -> Direct.return true
      | Some parent -> lineage_live p ~max_hops:64 parent)
      >>= function
      | false -> Direct.return `Revoked
      | true ->
          let ns_ok =
            match g.g_scope_namespace with
            | None -> true
            | Some ns -> String.equal ns namespace
          in
          let target_ok =
            match (schema, projection) with
            | _, Some proj_name -> (
                match g.g_scope_projection with
                | Some p when String.equal p proj_name -> true
                | _ -> (
                    match (g.g_scope_schema, schema) with
                    | Some s, Some target_s -> String.equal s target_s
                    | _ -> false))
            | Some target_s, None -> (
                match g.g_scope_schema with
                | Some s -> String.equal s target_s
                | None -> false)
            | None, None -> true
          in
          if ns_ok && target_ok then Direct.return `Ok
          else Direct.return `Scope_denied)

(* -- delegation-attenuation ------------------------------------------ *)

let json_obj s =
  try
    match Yojson.Safe.from_string s with
    | `Assoc kvs -> Some kvs
    | `Null -> Some []
    | _ -> None
  with Yojson.Json_error _ -> None

let admit_all kvs = kvs = []

let max_ternary_of kvs =
  match List.assoc_opt "max_ternary" kvs with
  | Some (`Int n) -> Some n
  | _ -> None

let scope_narrower ~label parent child =
  match (parent, child) with
  | None, None -> Ok ()
  | None, Some _ -> Ok ()
  | Some p, Some c when String.equal p c -> Ok ()
  | Some p, Some c ->
      Error (Printf.sprintf "%s %S does not match parent %S" label c p)
  | Some _, None ->
      Error (Printf.sprintf "dropping %s widens the parent" label)

let attenuation_narrower ~prim ~args_attenuation ~path_prefix
    ?(scope_schema = None) ?(scope_namespace = None) ?(scope_projection = None)
    (parent : grant) : (unit, string) result =
  if parent.g_prim <> "*" && parent.g_prim <> prim then
    Error (Printf.sprintf "prim %S is not a narrowing of %S" prim parent.g_prim)
  else
    match (json_obj parent.g_args_attenuation, json_obj args_attenuation) with
    | None, _ | _, None -> Error "args_attenuation must be JSON (object or null)"
    | Some pkvs, Some ckvs -> (
        let child_shape_ok = admit_all ckvs || max_ternary_of ckvs <> None in
        let args_res =
          if admit_all pkvs then
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
                    else Error "unknown args_attenuation predicate shape")
        in
        match args_res with
        | Error e -> Error e
        | Ok () -> (
            let path_res =
              match (parent.g_path_prefix, path_prefix) with
              | None, None -> Ok ()
              | None, Some _ ->
                  Error "a NULL-prefix parent authorizes no path scope"
              | Some _, None -> Error "dropping path_prefix widens the parent"
              | Some pp, Some cp ->
                  if prefix_match pp cp then Ok ()
                  else
                    Error (Printf.sprintf "path prefix %S is not under %S" cp pp)
            in
            match path_res with
            | Error e -> Error e
            | Ok () -> (
                match scope_narrower ~label:"scope_schema" parent.g_scope_schema scope_schema with
                | Error e -> Error e
                | Ok () -> (
                    match scope_narrower ~label:"scope_namespace" parent.g_scope_namespace scope_namespace with
                    | Error e -> Error e
                    | Ok () ->
                        scope_narrower ~label:"scope_projection" parent.g_scope_projection scope_projection))))

let attenuate_grant p ~parent_id ~prim ~args_attenuation ~path_prefix
    ?(scope_schema = None) ?(scope_namespace = None) ?(scope_projection = None)
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
      | false -> Direct.return `Revoked
      | true -> (
          match
            attenuation_narrower ~prim ~args_attenuation ~path_prefix
              ~scope_schema ~scope_namespace ~scope_projection parent
          with
          | Error reason -> Direct.return (`Not_narrower reason)
          | Ok () ->
              mint_grant p ~prim ~args_attenuation ~path_prefix ~scope_schema
                ~scope_namespace ~scope_projection ~caller
                ~minted_by:(Some caller) ~parent_grant:(Some parent.g_id) ()
              >>= fun g -> Direct.return (`Ok g)))

let grant_lineage p ?(max_hops = 64) id =
  let rec go hops acc id =
    if hops > max_hops then Direct.return (List.rev acc)
    else
      fetch_grant p id
      >>= function
      | None | Some { g_parent_grant = None; _ } -> Direct.return (List.rev acc)
      | Some { g_parent_grant = Some parent; _ } -> (
          fetch_grant p parent >>= function
          | None -> Direct.return (List.rev acc)
          | Some g -> go (hops + 1) (g :: acc) parent)
  in
  fetch_grant p id >>= function
  | None -> Direct.return []
  | Some _ -> go 0 [] id

let grant_descendants p ?(max_depth = 64) id =
  Db.q ~params:[ p_str id; p_int max_depth ] p
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
        fetch_grant p id >>= function
        | None -> fetch_all acc rest
        | Some g -> fetch_all (g :: acc) rest)
  in
  fetch_all [] (List.map (fun r -> text r 0 "grant.id") rows)

(* -- retention (kernel GC + Midden retention_policy schema) ---------- *)

let gc_journal p ~run_id ~policy () =
  Db.with_tx p (fun c ->
      Db.q_conn_unit c ~params:[ p_str run_id ]
        "DELETE FROM journals WHERE run_id = $1::uuid"
      >>= fun () ->
      Db.q_conn_unit c ~params:[ p_str policy; p_str run_id ]
        "UPDATE runs SET journal_gced_at = now(), journal_gced_policy = $1, \
         verify_status = 'gced', verified_at = now() WHERE id = $2::uuid")
  >>= fun _ -> fetch_run p run_id

let fetch_gc_tombstone p run_id =
  Db.q ~params:[ p_str run_id ] p
    "SELECT journal_gced_policy FROM runs \
     WHERE id = $1::uuid AND journal_gced_at IS NOT NULL"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (opt_text r 0))
  | _ -> store_error "runs: multiple rows for one id"

let redact_journal_row p ~run_id ~seq ~policy () =
  Db.with_tx p (fun c ->
      Db.q_conn_unit c ~params:[ p_str policy; p_str run_id; p_int seq ]
        "UPDATE journals SET result_ternary = NULL, result_hash = NULL, \
         error = 'redacted (chain break): ' || $1 \
         WHERE run_id = $2::uuid AND seq = $3"
      >>= fun () ->
      Db.q_conn_unit c ~params:[ p_str run_id ]
        "UPDATE runs SET verify_status = NULL, verified_at = NULL \
         WHERE id = $1::uuid")

type retention_policy = {
  rp_identity : string;
  rp_name : string;
  rp_keep_days : int;
  rp_keep_failures : bool;
  rp_enabled : bool;
  rp_updated_at : string;
}

let select_retention_policy =
  "rp.\"identity-id\", i.name, rp.\"keep-days\", rp.\"keep-failures\", \
   rp.enabled, rp.at::text FROM retention_policy rp \
   JOIN identities i ON i.id::text = rp.\"identity-id\""

let retention_policy_of_row r =
  {
    rp_identity = text r 0 "retention_policy.identity-id";
    rp_name = text r 1 "identities.name";
    rp_keep_days = int r 2 "retention_policy.keep-days";
    rp_keep_failures = bool r 3 "retention_policy.keep-failures";
    rp_enabled = bool r 4 "retention_policy.enabled";
    rp_updated_at = text r 5 "retention_policy.at";
  }

let fetch_retention_policy p ~identity =
  ensure_midden p;
  Db.q ~params:[ p_str identity ] p
    ("SELECT " ^ select_retention_policy ^ " WHERE rp.\"identity-id\" = $1")
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (retention_policy_of_row r))
  | _ -> store_error "retention_policy: multiple rows for one identity"

let list_retention_policies p () =
  ensure_midden p;
  Db.q p ("SELECT " ^ select_retention_policy ^ " ORDER BY i.name")
  >>= fun rows -> Direct.return (List.map retention_policy_of_row rows)

let put_retention_policy p ~identity ~keep_days ~keep_failures ~enabled
    ~updated_by () =
  let entity = Midden.Entity_id.of_key ~namespace:"retention_policy" identity in
  let doc =
    TS.record
      [ TS.req_bool "enabled" enabled;
        TS.req_str "identity-id" identity;
        TS.req_int "keep-days" (Int64.of_int keep_days);
        TS.req_bool "keep-failures" keep_failures ]
  in
  ignore
    (midden_upsert p ~entity ~schema:TS.retention_policy_hash
       ~author:updated_by ~reason:"retention_policy" doc);
  fetch_retention_policy p ~identity
  >>= function
  | Some pol -> Direct.return pol
  | None -> store_error "put_retention_policy: row vanished after upsert"

let retention_where =
  "caller = $1::uuid AND journal_gced_at IS NULL \
   AND created_at < now() - ($2::int * interval '1 day') \
   AND (NOT $3::bool OR (status = 'normal' AND verify_status = 'verified'))"

let retention_params pol =
  [ p_str pol.rp_identity; p_int pol.rp_keep_days; p_bool pol.rp_keep_failures ]

let retention_eligible_ids p pol =
  Db.q ~params:(retention_params pol) p
    ("SELECT id::text FROM runs WHERE " ^ retention_where
    ^ " ORDER BY created_at, id")
  >>= fun rows -> Direct.return (List.map (fun r -> text r 0 "runs.id") rows)

let retention_citation pol =
  Printf.sprintf "retention:%s:keep-%dd%s" pol.rp_name pol.rp_keep_days
    (if pol.rp_keep_failures then ":keep-failures" else "")

let rec retention_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: rest -> x :: retention_take (n - 1) rest

type retention_sweep = {
  rs_identity : string;
  rs_name : string;
  rs_policy : string;
  rs_eligible : int;
  rs_collected : int;
  rs_run_ids : string list;
}

let sweep_retention p ~dry_run () =
  list_retention_policies p ()
  >>= fun policies ->
  let enabled = List.filter (fun pol -> pol.rp_enabled) policies in
  let collect pol ids =
    let citation = retention_citation pol in
    let rec go n = function
      | [] -> Direct.return n
      | id :: rest -> gc_journal p ~run_id:id ~policy:citation () >>= fun _ -> go (n + 1) rest
    in
    go 0 ids
  in
  let rec one acc = function
    | [] -> Direct.return (List.rev acc)
    | pol :: rest ->
        retention_eligible_ids p pol
        >>= fun ids ->
        (if dry_run then Direct.return 0 else collect pol ids)
        >>= fun collected ->
        one
          ({
             rs_identity = pol.rp_identity;
             rs_name = pol.rp_name;
             rs_policy = retention_citation pol;
             rs_eligible = List.length ids;
             rs_collected = collected;
             rs_run_ids = retention_take 10 ids;
           }
          :: acc)
          rest
  in
  one [] enabled

(* -- identity name registry, signup, oidc, sessions, runner tokens --- *)

type name_error =
  [ `Invalid_name of string | `Name_conflict of string | `Exists ]

let name_max_len = 32
let normalize_name name = String.lowercase_ascii (String.trim name)

let check_name_charset name : (unit, string) result =
  let n = String.length name in
  if n = 0 then Error "name must be non-empty"
  else if n > name_max_len then
    Error (Printf.sprintf "name must be at most %d characters" name_max_len)
  else if name.[0] = '-' then Error "name must not start with '-'"
  else if name.[n - 1] = '-' then Error "name must not end with '-'"
  else if
    String.for_all
      (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-')
      name
  then Ok ()
  else Error "name may contain only a-z, 0-9, and '-'"

let garden_prefix_of_name name = "u." ^ name ^ "."

let name_prefixes_alias a b =
  let la = String.length a and lb = String.length b in
  if la <= lb then String.sub b 0 la = a else String.sub a 0 lb = b

let check_name_claim ~candidate ~existing : (string, name_error) result =
  let name = normalize_name candidate in
  match check_name_charset name with
  | Error e -> Error (`Invalid_name e)
  | Ok () -> (
      let root = "u." ^ name in
      match
        List.find_opt
          (fun e -> name_prefixes_alias root ("u." ^ normalize_name e))
          existing
      with
      | Some e ->
          Error
            (`Name_conflict
              (Printf.sprintf
                 "name %S collides with the existing identity %S: garden \
                  prefixes may not overlap (u.%s. would alias u.%s.)"
                 name e name (normalize_name e)))
      | None -> Ok name)

let claim_name_tx c ~candidate : (string, name_error) result =
  Db.q_conn c "SELECT name FROM identities"
  >>= fun rows ->
  let existing = List.map (fun r -> text r 0 "identity.name") rows in
  Direct.return (check_name_claim ~candidate ~existing)

let insert_identity_tx c ~name ~token ~is_admin =
  Db.q_conn
    ~params:[ p_str name; p_str (Tuna.Hash.hex_of_string token); p_bool is_admin ]
    c
    "INSERT INTO identities (name, token_hash, is_admin) VALUES ($1, $2, $3) \
     ON CONFLICT (name) DO NOTHING \
     RETURNING id::text, name, token_hash, is_admin"
  >>= function
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | [] -> Direct.return None
  | _ -> store_error "identity insert: unexpected row count"

let mint_identity p ?(is_admin = false) ~name ~token () :
    (identity, name_error) result =
  Db.with_tx p (fun c ->
      claim_name_tx c ~candidate:name
      >>= function
      | Error e -> Direct.return (Error e)
      | Ok name -> (
          insert_identity_tx c ~name ~token ~is_admin
          >>= function
          | Some i -> Direct.return (Ok i)
          | None -> Direct.return (Error `Exists)))

let signup_identity p ~name ~token () : (identity * grant, name_error) result =
  Db.with_tx p (fun c ->
      claim_name_tx c ~candidate:name
      >>= function
      | Error e -> Direct.return (Error e)
      | Ok name -> (
          insert_identity_tx c ~name ~token ~is_admin:false
          >>= function
          | None -> Direct.return (Error `Exists)
          | Some i -> (
              Db.q_conn
                ~params:[ p_str i.i_id; p_str (garden_prefix_of_name name) ]
                c
                "INSERT INTO grants (prim, args_attenuation, path_prefix, \
                 caller, minted_by, parent_grant) \
                 VALUES ('tree/put', '{}'::jsonb, $2, $1::uuid, NULL, NULL) \
                 RETURNING id::text"
              >>= function
              | [ g ] -> (
                  Db.q_conn ~params:[ p_str (text g 0 "grant.id") ] c
                    select_grant_by_id
                  >>= function
                  | [ gr ] -> Direct.return (Ok (i, grant_of_row gr))
                  | rows ->
                      store_error "signup_identity: grant fetch gave %d rows"
                        (List.length rows))
              | rows ->
                  store_error "signup_identity: grant insert gave %d rows"
                    (List.length rows))))

let put_oidc_login p ~state ~nonce ~code_verifier ~ttl_seconds =
  Db.q_unit
    ~params:
      [ p_str (Tuna.Hash.hex_of_string state);
        p_str nonce;
        p_str code_verifier;
        p_int64 (Int64.of_int ttl_seconds) ]
    p
    "INSERT INTO oidc_logins (state_hash, nonce, code_verifier, expires_at) \
     VALUES ($1, $2, $3, now() + ($4::text || ' seconds')::interval)"
  >>= fun () -> Direct.return ()

let take_oidc_login p ~state =
  Db.q ~params:[ p_str (Tuna.Hash.hex_of_string state) ] p
    "UPDATE oidc_logins SET used_at = now() \
     WHERE state_hash = $1 AND used_at IS NULL AND expires_at > now() \
     RETURNING nonce, code_verifier"
  >>= function
  | [ r ] ->
      Direct.return (Some (text r 0 "oidc.nonce", text r 1 "oidc.verifier"))
  | [] -> Direct.return None
  | _ -> store_error "take_oidc_login: unexpected row count"

let find_oidc_identity p ~provider ~subject =
  Db.q ~params:[ p_str provider; p_str subject ] p
    "SELECT identity_id::text FROM oidc_identities \
     WHERE provider = $1 AND subject = $2"
  >>= function
  | [ r ] -> Direct.return (Some (text r 0 "oidc.identity"))
  | [] -> Direct.return None
  | _ -> store_error "find_oidc_identity: unexpected row count"

let signup_oidc_identity p ~provider ~subject ~name ~token () :
    (identity, name_error) result =
  Db.with_tx p (fun c ->
      claim_name_tx c ~candidate:name
      >>= function
      | Error e -> Direct.return (Error e)
      | Ok name -> (
          insert_identity_tx c ~name ~token ~is_admin:false
          >>= function
          | None -> Direct.return (Error `Exists)
          | Some i ->
              Db.q_conn_unit
                ~params:[ p_str i.i_id; p_str (garden_prefix_of_name name) ]
                c
                "INSERT INTO grants (prim, args_attenuation, path_prefix, \
                 caller, minted_by, parent_grant) \
                 VALUES ('tree/put', '{}'::jsonb, $2, $1::uuid, NULL, NULL)"
              >>= fun () ->
              Db.q_conn_unit
                ~params:[ p_str provider; p_str subject; p_str i.i_id ]
                c
                "INSERT INTO oidc_identities (provider, subject, identity_id) \
                 VALUES ($1, $2, $3::uuid) \
                 ON CONFLICT (provider, subject) DO NOTHING"
              >>= fun () -> Direct.return (Ok i)))

let list_identities p () =
  Db.q p "SELECT id::text, name, token_hash, is_admin FROM identities ORDER BY name"
  >>= fun rows -> Direct.return (List.map identity_of_row rows)

let verify_token p token =
  Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
    "SELECT id::text, name, token_hash, is_admin FROM identities WHERE token_hash = $1"
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | _ -> Direct.return None

let rotate_token p ~identity_id ~token () =
  Db.q ~params:[ p_str identity_id; p_str (Tuna.Hash.hex_of_string token) ] p
    "UPDATE identities SET token_hash = $2 WHERE id = $1::uuid \
     RETURNING id::text, name, token_hash, is_admin"
  >>= fun rows ->
  match rows with
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (identity_of_row r))
  | _ -> store_error "rotate_token: RETURNING gave %d rows" (List.length rows)

let log_auth p ?(identity_id : string option) ~kind ~success () =
  Direct.catch
    (fun () ->
      Db.q_unit ~params:[ p_opt identity_id; p_str kind; p_bool success ] p
        "INSERT INTO auth_log (identity_id, kind, success) \
         VALUES ($1::uuid, $2, $3)"
      >>= fun () -> Direct.return ())
    (fun _ -> Direct.return ())

let set_password p ~identity_id ~password =
  let hash = Credentials.hash_password password in
  Db.q_unit ~params:[ p_str identity_id; p_str "password"; p_str hash ] p
    "INSERT INTO credentials (identity_id, kind, secret_hash) \
     VALUES ($1::uuid, $2, $3) \
     ON CONFLICT (identity_id, kind) \
     DO UPDATE SET secret_hash = EXCLUDED.secret_hash"
  >>= fun () -> Direct.return ()

let verify_password p ~username ~password =
  fetch_identity_by_name p (String.trim username)
  >>= function
  | None ->
      let _ = Credentials.verify_password password ~stored:Credentials.dummy_hash in
      log_auth p ~kind:"password" ~success:false () >>= fun () -> Direct.return None
  | Some i -> (
      Db.q ~params:[ p_str i.i_id ] p
        "SELECT secret_hash FROM credentials \
         WHERE identity_id = $1::uuid AND kind = 'password'"
      >>= function
      | [ r ]
        when Credentials.verify_password password
               ~stored:(text r 0 "credentials.secret") ->
          log_auth p ~identity_id:i.i_id ~kind:"password" ~success:true ()
          >>= fun () ->
          Direct.catch
            (fun () ->
              Db.q_unit ~params:[ p_str i.i_id ] p
                "UPDATE credentials SET last_used_at = now() \
                 WHERE identity_id = $1::uuid AND kind = 'password'"
              >>= fun () -> Direct.return ())
            (fun _ -> Direct.return ())
          >>= fun () -> Direct.return (Some i)
      | rows ->
          (match rows with
          | [ _ ] -> ()
          | _ ->
              let _ =
                Credentials.verify_password password ~stored:Credentials.dummy_hash
              in
              ());
          log_auth p ~identity_id:i.i_id ~kind:"password" ~success:false ()
          >>= fun () -> Direct.return None)

let mint_session p ~identity_id ~ttl_seconds =
  let token = Credentials.mint_session_token () in
  Db.q_unit
    ~params:
      [ p_str identity_id;
        p_str (Tuna.Hash.hex_of_string token);
        p_int64 (Int64.of_int ttl_seconds) ]
    p
    "INSERT INTO auth_sessions (identity_id, token_hash, expires_at) \
     VALUES ($1::uuid, $2, now() + ($3::text || ' seconds')::interval)"
  >>= fun () ->
  log_auth p ~identity_id ~kind:"session" ~success:true ()
  >>= fun () -> Direct.return token

let verify_session p token =
  if String.length token = 0 then Direct.return None
  else
    Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
      "SELECT i.id::text, i.name, i.token_hash, i.is_admin \
       FROM auth_sessions s JOIN identities i ON i.id = s.identity_id \
       WHERE s.token_hash = $1 AND s.revoked_at IS NULL \
       AND s.expires_at > now() LIMIT 1"
    >>= fun rows ->
    match rows with
    | [] -> Direct.return None
    | [ r ] -> Direct.return (Some (identity_of_row r))
    | _ -> Direct.return None

let revoke_session p token =
  Direct.catch
    (fun () ->
      Db.q_unit ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
        "UPDATE auth_sessions SET revoked_at = now() \
         WHERE token_hash = $1 AND revoked_at IS NULL"
      >>= fun () -> Direct.return ())
    (fun e ->
      Printf.eprintf "[tuna] revoke_session failed: %s\n%!"
        (Printexc.to_string e);
      Direct.return ())

let mint_runner_token p ~identity_id ~name =
  let token = Credentials.mint_session_token () in
  Db.q
    ~params:
      [ p_str identity_id; p_str name; p_str (Tuna.Hash.hex_of_string token) ]
    p
    "INSERT INTO runner_tokens (identity_id, name, token_hash) \
     VALUES ($1::uuid, $2, $3) RETURNING id::text"
  >>= fun rows ->
  match rows with
  | [ r ] -> Direct.return (text r 0 "runner_tokens.id", token)
  | n -> store_error "mint_runner_token: RETURNING gave %d rows" (List.length n)

type runner_token = {
  rt_id : string;
  rt_name : string;
  rt_created_at : string;
  rt_last_used_at : string option;
  rt_revoked_at : string option;
}

let list_runner_tokens p ~identity_id =
  Db.q ~params:[ p_str identity_id ] p
    "SELECT id::text, name, created_at::text, last_used_at::text, \
     revoked_at::text FROM runner_tokens WHERE identity_id = $1::uuid \
     ORDER BY created_at"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         {
           rt_id = text r 0 "runner_tokens.id";
           rt_name = text r 1 "runner_tokens.name";
           rt_created_at = text r 2 "runner_tokens.created_at";
           rt_last_used_at = opt_text r 3;
           rt_revoked_at = opt_text r 4;
         })
       rows)

let revoke_runner_token p ~identity_id ~token_id =
  Db.q ~params:[ p_str token_id; p_str identity_id ] p
    "UPDATE runner_tokens SET revoked_at = now() \
     WHERE id = $1::uuid AND identity_id = $2::uuid AND revoked_at IS NULL \
     RETURNING id::text"
  >>= fun rows -> Direct.return (match rows with [ _ ] -> true | _ -> false)

let verify_runner_token p token =
  if String.length token = 0 then Direct.return None
  else
    Db.q ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
      "SELECT i.id::text, i.name, i.token_hash, i.is_admin \
       FROM runner_tokens t JOIN identities i ON i.id = t.identity_id \
       WHERE t.token_hash = $1 AND t.revoked_at IS NULL LIMIT 1"
    >>= fun rows ->
    match rows with
    | [] -> Direct.return None
    | [ r ] ->
        let i = identity_of_row r in
        Direct.catch
          (fun () ->
            Db.q_unit ~params:[ p_str (Tuna.Hash.hex_of_string token) ] p
              "UPDATE runner_tokens SET last_used_at = now() \
               WHERE token_hash = $1 AND revoked_at IS NULL"
            >>= fun () -> Direct.return (Some i))
          (fun _ -> Direct.return (Some i))
    | _ -> Direct.return None

(* -- capability helpers ---------------------------------------------- *)

let is_admin p id =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str id ] p
        "SELECT is_admin FROM identities WHERE id = $1::uuid"
      >>= function
      | [ r ] -> Direct.return (bool r 0 "identity.is_admin")
      | _ -> Direct.return false)
    (fun e ->
      Printf.eprintf "[tuna] probe is_admin failed: %s\n%!"
        (Printexc.to_string e);
      Direct.return false)

let has_live_grant p caller =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str caller ] p
        "SELECT 1 FROM grants WHERE caller = $1::uuid AND revoked_at IS NULL LIMIT 1"
      >>= function [] -> Direct.return false | _ -> Direct.return true)
    (fun e ->
      Printf.eprintf "[tuna] probe has_live_grant failed: %s\n%!"
        (Printexc.to_string e);
      Direct.return false)

let has_covering_grant p caller path =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str caller ] p
        "SELECT path_prefix, parent_grant::text FROM grants \
         WHERE caller = $1::uuid AND revoked_at IS NULL"
      >>= fun rows ->
      let rec seek = function
        | [] -> Direct.return false
        | r :: rest -> (
            match (opt_text r 0, opt_text r 1) with
            | Some pfx, parent when prefix_match pfx path -> (
                match parent with
                | None -> Direct.return true
                | Some pid -> lineage_live p ~max_hops:64 pid)
            | _ -> seek rest)
      in
      seek rows)
    (fun e ->
      Printf.eprintf "[tuna] probe has_covering_grant failed: %s\n%!"
        (Printexc.to_string e);
      Direct.return false)

let find_live_grant_id p ~prim ~path_prefix ~caller =
  Db.q ~params:[ p_str prim; p_str path_prefix; p_str caller ] p
    "SELECT id::text FROM grants WHERE prim = $1 AND path_prefix = $2 \
     AND caller = $3::uuid AND revoked_at IS NULL \
     ORDER BY created_at DESC LIMIT 1"
  >>= function
  | [ r ] -> Direct.return (Some (text r 0 "grants.id"))
  | _ -> Direct.return None

(* -- grant audit surface --------------------------------------------- *)

type grant_exercise = {
  ge_run_id : string;
  ge_prim : string list;
  ge_calls : int64;
  ge_first_callsite : string;
  ge_last_callsite : string;
  ge_run_status : string;
  ge_run_steps : int option;
  ge_run_caller : string option;
  ge_run_created : string option;
}

let list_grant_exercises p ?(limit = 200) id =
  Db.q ~params:[ p_str id; p_int limit ] p
    "SELECT j.run_id::text, \
            string_agg(DISTINCT j.prim, ',' ORDER BY j.prim) AS prims, \
            count(*)::bigint, \
            min(j.callsite_path) AS first_cs, \
            max(j.callsite_path) AS last_cs, \
            r.status, r.step_count, r.caller::text, r.created_at::text \
       FROM journals j JOIN runs r ON r.id = j.run_id \
      WHERE j.grant_id = $1::uuid \
      GROUP BY j.run_id, r.status, r.step_count, r.caller, r.created_at \
      ORDER BY r.created_at DESC NULLS LAST LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         {
           ge_run_id = text r 0 "journals.run_id";
           ge_prim =
             (match opt_text r 1 with
             | None -> []
             | Some s -> String.split_on_char ',' s);
           ge_calls = int64 r 2 "journal.calls";
           ge_first_callsite = text r 3 "journal.first_callsite";
           ge_last_callsite = text r 4 "journal.last_callsite";
           ge_run_status = text r 5 "runs.status";
           ge_run_steps = opt_int r 6;
           ge_run_caller = opt_text r 7;
           ge_run_created = opt_text r 8;
         })
       rows)

let list_grant_calls p ~grant_id ~run_id =
  Db.q ~params:[ p_str grant_id; p_str run_id ] p
    "SELECT seq, callsite_path, prim, prim_contract, error, result_hash \
       FROM journals WHERE grant_id = $1::uuid AND run_id = $2::uuid \
      ORDER BY seq"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         ( int r 0 "journal.seq",
           text r 1 "journal.callsite_path",
           text r 2 "journal.prim",
           text r 3 "journal.prim_contract",
           opt_text r 4,
           opt_text r 5 ))
       rows)

(* -- tree_compat schema (replaces tree_paths) ------------------------ *)

let path_hash = Tuna.Hash.hex_of_string

let tree_compat_entity path =
  Midden.Entity_id.of_key ~namespace:"tree_compat" path

type path_entry = {
  tp_path : string;
  tp_value_hash : string;
  tp_version : int64;
  tp_owner : string;
  tp_updated_at : string option;
}

let path_entry_of_row r what =
  {
    tp_path = text r 0 what;
    tp_value_hash = text r 1 what;
    tp_version = int64 r 2 what;
    tp_owner = text r 3 what;
    tp_updated_at = opt_text r 4;
  }

let path_get p ~path =
  ensure_midden p;
  Db.q ~params:[ p_str path ] p
    "SELECT path, \"value-hash\", version, owner, at::text \
     FROM tree_compat_by_path WHERE path = $1"
  >>= function
  | [] -> Direct.return None
  | [ r ] -> Direct.return (Some (path_entry_of_row r "tree_compat"))
  | _ -> store_error "tree_compat: multiple rows for path %s" path

let path_put p ~path ~value_hash ~owner =
  ensure_midden p;
  let m = Db.midden p in
  let entity = tree_compat_entity path in
  let rec go retries =
    let prev, next_ver =
      match midden_exn "path_put head" (MS.head m ~entity) with
      | None | Some { head = None; _ } -> (None, 1L)
      | Some { head = Some _ as prev; _ } -> (
          match path_get p ~path with
          | Some e -> (prev, Int64.succ e.tp_version)
          | None -> (prev, 1L))
    in
    let doc =
      TS.record
        [ TS.req_str "owner" owner;
          TS.req_str "path" path;
          TS.req_str "value-hash" value_hash;
          TS.req_int "version" next_ver ]
    in
    match
      MS.put m ~entity ~prev ~schema:TS.tree_compat_hash ~author:owner
        ~reason:("tree_compat:" ^ path) doc
    with
    | Ok _ -> Direct.return next_ver
    | Error (MS.Conflict _) when retries > 0 -> go (retries - 1)
    | Error e -> store_error "path_put: %s" (MS.string_of_error e)
  in
  go 4

let path_put_cas p ~path ~value_hash ~owner ~expected_version ~expected_hash =
  ensure_midden p;
  let m = Db.midden p in
  let entity = tree_compat_entity path in
  let head_opt = midden_exn "path_put_cas head" (MS.head m ~entity) in
  let existing = path_get p ~path in
  match expected_version with
  | None | Some 0L -> (
      match existing with
      | Some _ -> Direct.return `Conflict
      | None -> (
          let prev =
            match head_opt with None -> None | Some h -> h.head
          in
          let doc =
            TS.record
              [ TS.req_str "owner" owner;
                TS.req_str "path" path;
                TS.req_str "value-hash" value_hash;
                TS.req_int "version" 1L ]
          in
          match
            MS.put m ~entity ~prev ~schema:TS.tree_compat_hash ~author:owner
              ~reason:("tree_compat:cas:" ^ path) doc
          with
          | Ok _ -> Direct.return (`Ok 1L)
          | Error (MS.Conflict _) -> Direct.return `Conflict
          | Error e -> store_error "path_put_cas: %s" (MS.string_of_error e)))
  | Some n -> (
      match existing with
      | None -> Direct.return `Absent
      | Some e ->
          let hash_ok =
            match expected_hash with
            | None -> true
            | Some eh -> String.equal eh e.tp_value_hash
          in
          if e.tp_version <> n || not hash_ok then Direct.return `Conflict
          else
            let prev =
              match head_opt with None -> None | Some h -> h.head
            in
            let next_ver = Int64.succ e.tp_version in
            let doc =
              TS.record
                [ TS.req_str "owner" owner;
                  TS.req_str "path" path;
                  TS.req_str "value-hash" value_hash;
                  TS.req_int "version" next_ver ]
            in
            match
              MS.put m ~entity ~prev ~schema:TS.tree_compat_hash ~author:owner
                ~reason:("tree_compat:cas:" ^ path) doc
            with
            | Ok _ -> Direct.return (`Ok next_ver)
            | Error (MS.Conflict _) -> Direct.return `Conflict
            | Error err -> store_error "path_put_cas: %s" (MS.string_of_error err))

let path_delete p ~path ~expected_version =
  ensure_midden p;
  let m = Db.midden p in
  let entity = tree_compat_entity path in
  match path_get p ~path with
  | None -> Direct.return `Absent
  | Some e -> (
      match expected_version with
      | Some n when e.tp_version <> n -> Direct.return `Conflict
      | _ -> (
          let prev =
            match midden_exn "path_delete head" (MS.head m ~entity) with
            | None -> None
            | Some h -> h.head
          in
          match
            MS.delete m ~entity ~prev ~author:e.tp_owner
              ~reason:("tree_compat:del:" ^ path) ()
          with
          | Ok _ -> Direct.return (`Ok e.tp_version)
          | Error (MS.Conflict _) -> Direct.return `Conflict
          | Error err -> store_error "path_delete: %s" (MS.string_of_error err)))

let path_list p ?(limit = 1000) ?(after = "") ~prefix () =
  ensure_midden p;
  Db.q ~params:[ p_str prefix; p_int limit; p_str after ] p
    "SELECT path, \"value-hash\", version, owner, at::text \
     FROM tree_compat_by_path \
     WHERE path >= ($1 COLLATE \"C\") \
       AND path < (($1 || chr(255)) COLLATE \"C\") \
       AND path > ($3 COLLATE \"C\") \
     ORDER BY path COLLATE \"C\" LIMIT $2"
  >>= fun rows ->
  Direct.return (List.map (fun r -> path_entry_of_row r "tree_compat") rows)

let census_limit = 100000

let list_citation_paths p ?(limit = census_limit) () =
  ensure_midden p;
  Db.q ~params:[ p_int limit ] p
    "SELECT path, value_hash, version, owner, updated_at FROM ( \
       SELECT path, \"body-hash\" AS value_hash, 1::bigint AS version, \
              author AS owner, at::text AS updated_at \
       FROM claim_by_kind \
       UNION ALL \
       SELECT path, \"value-hash\" AS value_hash, version, owner, \
              at::text AS updated_at \
       FROM tree_compat_by_path \
       WHERE (path LIKE '%citation-%' OR path LIKE '%.audit-%' \
          OR path LIKE '%.verdict-%' OR path LIKE '%.claim.%') \
         AND NOT EXISTS (SELECT 1 FROM claim_by_kind c WHERE c.path = tree_compat_by_path.path) \
     ) combined ORDER BY path COLLATE \"C\" LIMIT $1"
  >>= fun rows ->
  Direct.return (List.map (fun r -> path_entry_of_row r "claims") rows)

(* -- op_audit schema (replaces tree_ops) ----------------------------- *)

type tree_op = {
  o_seq : int64;
  o_path : string;
  o_op : string;
  o_value_hash : string option;
  o_prev_version : int64 option;
  o_version : int64 option;
  o_actor : string;
  o_ts_unix : int64;
  o_op_hash : string;
}

let op_concat ~seq ~path ~op ~value_hash ~prev_version ~version ~actor ~ts_unix =
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
  "SELECT \"op-seq\", path, op, \"value-hash\", \"prev-version\", version, \
   actor, \"ts-unix\", \"op-hash\" FROM op_audit_by_seq"

let tree_op_of_row r =
  {
    o_seq = int64 r 0 "op_audit.op-seq";
    o_path = text r 1 "op_audit.path";
    o_op = text r 2 "op_audit.op";
    o_value_hash = opt_text r 3;
    o_prev_version = opt_int64 r 4;
    o_version = opt_int64 r 5;
    o_actor = text r 6 "op_audit.actor";
    o_ts_unix = int64 r 7 "op_audit.ts-unix";
    o_op_hash = text r 8 "op_audit.op-hash";
  }

let op_append_mu = Eio.Mutex.create ()

let op_append p ~op ~path ~value_hash ~prev_version ~version ~actor =
  ensure_midden p;
  Eio.Mutex.lock op_append_mu;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock op_append_mu)
    (fun () ->
      let seq, prev_hash =
        match
          Db.q p
            "SELECT \"op-seq\", \"op-hash\" FROM op_audit_by_seq \
             ORDER BY \"op-seq\" DESC LIMIT 1"
        with
        | [] -> (1L, genesis)
        | [ r ] -> (Int64.succ (int64 r 0 "last.seq"), text r 1 "last.op_hash")
        | _ -> store_error "op_audit head: multiple rows"
      in
      let ts_unix = Int64.of_float (Unix.gettimeofday ()) in
      let concat =
        op_concat ~seq ~path ~op ~value_hash ~prev_version ~version ~actor
          ~ts_unix
      in
      let h = Tuna.Hash.hex_of_string (prev_hash ^ concat) in
      let entity = fresh_uuid () in
      let author = if String.equal actor "" then "system" else actor in
      let doc =
        TS.record
          [ TS.req_str "actor" actor;
            TS.req_str "op" op;
            TS.req_str "op-hash" h;
            TS.req_int "op-seq" seq;
            TS.req_str "path" path;
            TS.opt_int "prev-version" prev_version;
            TS.req_int "ts-unix" ts_unix;
            TS.opt_str "value-hash" value_hash;
            TS.opt_int "version" version ]
      in
      ignore
        (midden_exn "op_append"
           (MS.put (Db.midden p) ~entity ~prev:None ~schema:TS.op_audit_hash
              ~author ~reason:("op:" ^ op) doc));
      Direct.return (seq, h))

let ops_fold p ?(prefix = "") ?(from_seq = 0L) ?(to_seq = Int64.max_int) () =
  ensure_midden p;
  let sql =
    select_tree_ops
    ^ " WHERE \"op-seq\" >= $1 AND \"op-seq\" <= $2 \
       AND ($3::text IS NULL OR (path >= ($3 COLLATE \"C\") \
            AND path < (($3 || chr(255)) COLLATE \"C\"))) \
       ORDER BY \"op-seq\""
  in
  Db.q
    ~params:
      [ p_int64 from_seq;
        p_int64 to_seq;
        p_opt (if prefix = "" then None else Some prefix) ]
    p sql
  >>= fun rows -> Direct.return (List.map tree_op_of_row rows)

let verify_ops_chain (ops : tree_op list) : [ `Ok | `Bad of string ] =
  match ops with
  | [] -> `Ok
  | first :: rest ->
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

let ns_fork p ~src_prefix ~dst_prefix ~actor =
  ensure_midden p;
  let src_entries = path_list p ~prefix:src_prefix ~limit:5000 () in
  let copies =
    List.map
      (fun (e : path_entry) ->
        let suffix =
          String.sub e.tp_path (String.length src_prefix)
            (String.length e.tp_path - String.length src_prefix)
        in
        (dst_prefix ^ suffix, e.tp_value_hash))
      src_entries
  in
  let copied =
    List.fold_left
      (fun n (np, vh) ->
        match path_get p ~path:np with
        | Some _ -> n + 1
        | None ->
            ignore (path_put p ~path:np ~value_hash:vh ~owner:actor);
            n + 1)
      0 copies
  in
  ignore
    (op_append p ~op:"fork" ~path:dst_prefix ~value_hash:None ~prev_version:None
       ~version:None ~actor);
  List.iter
    (fun (np, vh) ->
      ignore
        (op_append p ~op:"put" ~path:np ~value_hash:(Some vh) ~prev_version:None
           ~version:(Some 1L) ~actor))
    copies;
  Direct.return copied

(* -- federation FED2 over op_audit + Midden -------------------------- *)

let ops_head_seq p =
  ensure_midden p;
  Db.q p "SELECT COALESCE(MAX(\"op-seq\"), 0) FROM op_audit_by_seq"
  >>= function
  | [ r ] -> Direct.return (int64 r 0 "op_audit.head")
  | _ -> store_error "op_audit head: unexpected row count"

type ops_window_row = { w_op : tree_op; w_prev_hash : string }

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

let ops_window p ~from_seq ~limit =
  ensure_midden p;
  Db.q ~params:[ p_int64 from_seq; p_int limit ] p
    "SELECT \"op-seq\", path, op, \"value-hash\", \"prev-version\", version, \
     actor, \"ts-unix\", \"op-hash\", prev_hash FROM ( \
       SELECT \"op-seq\", path, op, \"value-hash\", \"prev-version\", version, \
              actor, \"ts-unix\", \"op-hash\", \
              COALESCE(LAG(\"op-hash\") OVER (ORDER BY \"op-seq\"), \
                       repeat('0', 64)) AS prev_hash \
       FROM op_audit_by_seq) t \
     WHERE \"op-seq\" >= $1 ORDER BY \"op-seq\" LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         { w_op = tree_op_of_row r; w_prev_hash = text r 9 "window.prev_hash" })
       rows)

(* -- definitions as ops + visibility (Midden def & def_visibility) --- *)

let undef_marker =
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

let dict_path_of ~identity ~name = "dict/" ^ identity ^ "/" ^ name

let dict_op_put p ~actor ~identity_id ~identity ~name ~value_hash ~ternary =
  let path = dict_path_of ~identity ~name in
  ops_fold p ~prefix:path ()
  >>= fun ops ->
  let seen =
    List.length (List.filter (fun (o : tree_op) -> o.o_path = path) ops)
  in
  op_append p ~op:"put" ~path ~value_hash:(Some value_hash)
    ~prev_version:(if seen = 0 then None else Some (Int64.of_int seen))
    ~version:(Some (Int64.of_int (seen + 1))) ~actor
  >>= fun _ ->
  match ternary with
  | Some t -> dict_set p ~identity_id ~name ~ternary:t
  | None -> dict_del p ~identity_id ~name

let dict_op_undef p ~actor ~identity_id ~identity ~name =
  byte_value_put p ~hash:undef_marker ~bytes:""
  >>= fun () ->
  dict_op_put p ~actor ~identity_id ~identity ~name ~value_hash:undef_marker
    ~ternary:None

let dict_fold p ?(to_seq = Int64.max_int) ~identity () =
  let pre = "dict/" ^ identity ^ "/" in
  let n = String.length pre in
  ops_fold p ~prefix:pre ~to_seq ()
  >>= fun ops ->
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (o : tree_op) ->
      if String.length o.o_path > n && String.sub o.o_path 0 n = pre then
        match o.o_value_hash with
        | None -> ()
        | Some h ->
            let name = String.sub o.o_path n (String.length o.o_path - n) in
            Hashtbl.replace tbl name h)
    ops;
  Direct.return
    (List.sort compare
       (Hashtbl.fold
          (fun name h acc -> if h = undef_marker then acc else (name, h) :: acc)
          tbl []))

(* With Midden def schema, defs ARE already commits from day 1; backfill
   also ensures any def in def_view has an op_audit row if needed. *)
let dict_backfill p =
  ensure_midden p;
  Db.q p
    "SELECT \"identity-id\", \"identity-name\", name, \"value-hash\" \
     FROM def_view ORDER BY \"identity-id\", name"
  >>= fun rows ->
  List.iter
    (fun r ->
      let identity_id = text r 0 "def_view.identity-id" in
      let identity_name = text r 1 "def_view.identity-name" in
      let name = text r 2 "def_view.name" in
      let hash = text r 3 "def_view.value-hash" in
      let held = dict_fold p ~identity:identity_name () in
      if not (List.mem_assoc name held) then
        ignore
          (op_append p ~op:"put"
             ~path:(dict_path_of ~identity:identity_name ~name)
             ~value_hash:(Some hash) ~prev_version:None ~version:(Some 1L)
             ~actor:identity_id))
    rows;
  Direct.return ()

let dict_path_identity (path : string) : string option =
  let at_dict off =
    let pre = "dict/" in
    let plen = String.length pre in
    if String.length path > off + plen && String.sub path off plen = pre then
      match String.index_from_opt path (off + plen) '/' with
      | Some i -> Some (String.sub path (off + plen) (i - off - plen))
      | None -> None
    else None
  in
  match at_dict 0 with
  | Some id -> Some id
  | None ->
      let pre = "ns/" in
      if
        String.length path > String.length pre
        && String.sub path 0 (String.length pre) = pre
      then
        match String.index_from_opt path (String.length pre) '/' with
        | Some i -> at_dict (i + 1)
        | None -> None
      else None

let dict_visibility_of_path (path : string) : (string * bool) option =
  let pre = "vis/" in
  if
    String.length path > String.length pre
    && String.sub path 0 (String.length pre) = pre
  then
    match String.rindex_opt path '/' with
    | Some i when i > String.length pre -> (
        let id = String.sub path (String.length pre) (i - String.length pre) in
        match String.sub path (i + 1) (String.length path - i - 1) with
        | "public" -> Some (id, true)
        | "private" -> Some (id, false)
        | _ -> None)
    | _ -> None
  else None

let dict_visibility_path ~identity ~public =
  "vis/" ^ identity ^ if public then "/public" else "/private"

let dict_visibility_set p ~actor ~target ~public =
  let entity = Midden.Entity_id.of_key ~namespace:"def_visibility" target in
  let doc =
    TS.record [ TS.req_str "identity" target; TS.req_bool "public" public ]
  in
  ignore
    (midden_upsert p ~entity ~schema:TS.def_visibility_hash ~author:actor
       ~reason:"dict-visibility" doc);
  op_append p ~op:"dict-visibility"
    ~path:(dict_visibility_path ~identity:target ~public)
    ~value_hash:None ~prev_version:None ~version:None ~actor

let dict_visibility_public p =
  ensure_midden p;
  Db.q p "SELECT identity FROM def_visibility WHERE public = true ORDER BY identity"
  >>= fun rows ->
  Direct.return (List.map (fun r -> text r 0 "def_visibility.identity") rows)

let value_present p hash =
  Direct.catch
    (fun () ->
      Db.q ~params:[ p_str hash ] p
        "SELECT 1 FROM midden_values WHERE hash = $1 \
         UNION ALL SELECT 1 FROM programs WHERE hash = $1 LIMIT 1"
      >>= function [] -> Direct.return false | _ -> Direct.return true)
    (fun e ->
      Printf.eprintf "[tuna] probe value_present failed: %s\n%!"
        (Printexc.to_string e);
      Direct.return false)

let fed_apply p ~actor ~dst_prefix ~src_prefix (ops : (tree_op * string) list) =
  ensure_midden p;
  if ops = [] then Direct.return (`Applied [])
  else
    let rows = List.map (fun (o, h) -> { w_op = o; w_prev_hash = h }) ops in
    match verify_window_rows rows with
    | `Bad msg -> Direct.return (`Refused ("window chain broken: " ^ msg))
    | `Ok -> (
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
                    (Printf.sprintf "seq %Ld: window is not strictly increasing"
                       o.o_seq))
              else
                match (o.o_op, o.o_value_hash) with
                | ("put" | "cas"), Some vh -> (
                    value_present p vh >>= function
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
        >>= function
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
                  let newv =
                    if is_effect then
                      path_put p ~path:(dst_prefix ^ suffix)
                        ~value_hash:(Option.get o.o_value_hash) ~owner:actor
                    else 1L
                  in
                  let applied_seq, _ =
                    op_append p
                      ~op:(if is_effect then "put" else "cas")
                      ~path:(dst_prefix ^ suffix) ~value_hash:o.o_value_hash
                      ~prev_version:None
                      ~version:(if is_effect then Some newv else None)
                      ~actor
                  in
                  let acc =
                    if is_effect then
                      (dst_prefix ^ suffix, o.o_seq, applied_seq, newv) :: acc
                    else acc
                  in
                  apply acc rest
            in
            apply [] ops)

(* -- messaging over Midden message schema + midden_cursors ----------- *)

type message = {
  msg_seq : int64;
  msg_sender : string;
  msg_recipient : string;
  msg_kind : string;
  msg_re : string option;
  msg_body : string;
  msg_hash : string;
  msg_created_at : string;
}

let select_message_cols =
  "seq, sender, recipient, kind, re, body, \"body-hash\", at::text"

let message_of_row r =
  {
    msg_seq = int64 r 0 "message.seq";
    msg_sender = text r 1 "message.sender";
    msg_recipient = text r 2 "message.recipient";
    msg_kind = text r 3 "message.kind";
    msg_re = opt_text r 4;
    msg_body = text r 5 "message.body";
    msg_hash = text r 6 "message.body-hash";
    msg_created_at = text r 7 "message.at";
  }

let send_message p ~sender ~recipient ?(kind = "note") ?re ~body () =
  ensure_midden p;
  let body_hash = Tuna.Hash.hex_of_string body in
  let entity = fresh_uuid () in
  let doc =
    TS.record
      [ TS.req_str "body" body;
        TS.req_str "body-hash" body_hash;
        TS.req_str "kind" kind;
        TS.opt_str "re" re;
        TS.req_str "recipient" recipient;
        TS.req_str "sender" sender ]
  in
  let r =
    midden_exn "send_message"
      (MS.put (Db.midden p) ~entity ~prev:None ~schema:TS.message_hash
         ~author:sender ~reason:("msg:" ^ recipient) doc)
  in
  Direct.return
    {
      msg_seq = Int64.of_int r.MS.seq;
      msg_sender = sender;
      msg_recipient = recipient;
      msg_kind = kind;
      msg_re = re;
      msg_body = body;
      msg_hash = body_hash;
      msg_created_at = r.MS.at;
    }

let list_messages p ?recipient ?(after = 0L) ?(limit = 100) () =
  ensure_midden p;
  Db.q ~params:[ p_opt recipient; p_int64 after; p_int limit ] p
    (Printf.sprintf
       "SELECT %s FROM message_inbox \
        WHERE ($1::text IS NULL OR recipient = $1) AND seq > $2 \
        ORDER BY seq ASC LIMIT $3"
       select_message_cols)
  >>= fun rows -> Direct.return (List.map message_of_row rows)

let newest_message p ?recipient () =
  ensure_midden p;
  Db.q ~params:[ p_opt recipient ] p
    (Printf.sprintf
       "SELECT %s FROM message_inbox WHERE ($1::text IS NULL OR recipient = $1) \
        ORDER BY seq DESC LIMIT 1"
       select_message_cols)
  >>= fun rows ->
  Direct.return (match rows with [] -> None | r :: _ -> Some (message_of_row r))

let inbox_cursor_name inbox = "inbox:" ^ inbox

let inbox_poll_mu = Eio.Mutex.create ()

let inbox_poll p ~inbox ?(limit = 100) () =
  ensure_midden p;
  let m = Db.midden p in
  let cname = inbox_cursor_name inbox in
  Eio.Mutex.lock inbox_poll_mu;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock inbox_poll_mu)
    (fun () ->
      let cur =
        midden_exn "inbox subscribe"
          (MS.subscribe m ~name:cname ~after:0 ~schemas:[ "message" ])
      in
      let cursor = Int64.of_int cur.Midden.Subscriptions.after in
      let rows =
        Db.q ~params:[ p_str inbox; p_int64 cursor; p_int limit ] p
          (Printf.sprintf
             "SELECT %s FROM message_inbox WHERE recipient = $1 AND seq > $2 \
              ORDER BY seq ASC LIMIT $3"
             select_message_cols)
      in
      let msgs = List.map message_of_row rows in
      let next =
        List.fold_left
          (fun acc (m : message) -> if m.msg_seq > acc then m.msg_seq else acc)
          cursor msgs
      in
      ignore (midden_exn "inbox ack" (MS.ack m ~name:cname ~seq:(Int64.to_int next)));
      Direct.return (msgs, next))

let inbox_status p ~inbox =
  ensure_midden p;
  let cname = inbox_cursor_name inbox in
  Db.q ~params:[ p_str cname; p_str inbox ] p
    "WITH c AS (SELECT after_seq, updated_at FROM midden_cursors \
                WHERE name = $1) \
     SELECT COALESCE((SELECT after_seq FROM c), 0)::bigint, \
            (SELECT updated_at::text FROM c), \
            (SELECT count(*)::bigint FROM message_inbox m WHERE m.recipient = $2 \
               AND m.seq > COALESCE((SELECT after_seq FROM c), 0)), \
            (SELECT COALESCE(max(seq), 0)::bigint FROM message_inbox \
               WHERE recipient = $2)"
  >>= function
  | [ r ] ->
      Direct.return
        ( int64 r 0 "midden_cursors.after_seq",
          opt_text r 1,
          int64 r 2 "pending",
          int64 r 3 "head" )
  | n -> store_error "inbox_status: %d rows" (List.length n)

(* -- cron telemetry over Midden cron_run / cron_job / cron_heartbeat - *)

type cron_run = {
  cr_box : string;
  cr_run_id : string;
  cr_job : string option;
  cr_verdict : string option;
  cr_started_at : string option;
  cr_ended_at : string option;
  cr_detail : string;
  cr_received_at : string;
}

type cron_run_in = {
  ci_run_id : string;
  ci_job : string option;
  ci_verdict : string option;
  ci_started_at : string option;
  ci_ended_at : string option;
  ci_detail : string;
}

let select_cron_run_cols =
  "box, \"run-id\", job, verdict, \"started-at\", \"ended-at\", detail, at::text"

let cron_run_of_row r =
  {
    cr_box = text r 0 "cron_run.box";
    cr_run_id = text r 1 "cron_run.run-id";
    cr_job = opt_text r 2;
    cr_verdict = opt_text r 3;
    cr_started_at = opt_text r 4;
    cr_ended_at = opt_text r 5;
    cr_detail = Option.value (opt_text r 6) ~default:"{}";
    cr_received_at = text r 7 "cron_run.at";
  }

let ingest_cron_runs p ~box ~(runs : cron_run_in list) =
  ensure_midden p;
  List.iter
    (fun (r : cron_run_in) ->
      let entity =
        Midden.Entity_id.of_key ~namespace:"cron_run" (box ^ "/" ^ r.ci_run_id)
      in
      let doc =
        TS.record
          [ TS.req_str "box" box;
            TS.req_str "detail" r.ci_detail;
            TS.opt_str "ended-at" r.ci_ended_at;
            TS.opt_str "job" r.ci_job;
            TS.req_str "run-id" r.ci_run_id;
            TS.opt_str "started-at" r.ci_started_at;
            TS.opt_str "verdict" r.ci_verdict ]
      in
      ignore
        (midden_upsert p ~entity ~schema:TS.cron_run_hash ~author:box
           ~reason:("cron_run:" ^ r.ci_run_id) doc))
    runs;
  match runs with
  | [] -> Direct.return []
  | _ ->
      Db.q
        ~params:[ p_str box; p_text_list (List.map (fun r -> r.ci_run_id) runs) ]
        p
        (Printf.sprintf
           "SELECT %s FROM cron_run_idx WHERE box = $1 AND \
            \"run-id\" = ANY($2::text[]) ORDER BY \"run-id\""
           select_cron_run_cols)
      >>= fun rows -> Direct.return (List.map cron_run_of_row rows)

let cron_runs_filter ?box ?job ?since ?verdict () =
  ( " WHERE ($1::text IS NULL OR box = $1) \
     AND ($2::text IS NULL OR job = $2) \
     AND ($3::timestamptz IS NULL OR at >= $3::timestamptz) \
     AND ($4::text IS NULL OR verdict = $4)",
    [ p_opt box; p_opt job; p_opt since; p_opt verdict ] )

let list_cron_runs p ?box ?job ?since ?verdict ?(limit = 100) ?(offset = 0) () =
  ensure_midden p;
  let where, params = cron_runs_filter ?box ?job ?since ?verdict () in
  Db.q ~params:(params @ [ p_int limit; p_int offset ]) p
    (Printf.sprintf
       "SELECT %s FROM cron_run_idx%s ORDER BY at DESC, \"run-id\" \
        LIMIT $5 OFFSET $6"
       select_cron_run_cols where)
  >>= fun rows -> Direct.return (List.map cron_run_of_row rows)

let count_cron_runs p ?box ?job ?since ?verdict () =
  ensure_midden p;
  let where, params = cron_runs_filter ?box ?job ?since ?verdict () in
  Db.q ~params p (Printf.sprintf "SELECT count(*)::bigint FROM cron_run_idx%s" where)
  >>= function
  | [ r ] -> Direct.return (int64 r 0 "count")
  | n -> store_error "count_cron_runs: %d rows" (List.length n)

type cron_job = {
  cj_box : string;
  cj_job : string;
  cj_schedule : string option;
  cj_enabled : bool;
  cj_detail : string;
  cj_updated_at : string;
}

let select_cron_job_cols = "box, job, schedule, enabled, detail, at::text"

let cron_job_of_row r =
  {
    cj_box = text r 0 "cron_job.box";
    cj_job = text r 1 "cron_job.job";
    cj_schedule = opt_text r 2;
    cj_enabled = bool r 3 "cron_job.enabled";
    cj_detail = Option.value (opt_text r 4) ~default:"{}";
    cj_updated_at = text r 5 "cron_job.at";
  }

let replace_cron_jobs p ~box ~(jobs : (string * string option * bool * string) list) =
  ensure_midden p;
  let keep = List.map (fun (job, _, _, _) -> job) jobs in
  let existing =
    Db.q ~params:[ p_str box ] p "SELECT job FROM cron_job WHERE box = $1"
    |> List.map (fun r -> text r 0 "cron_job.job")
  in
  List.iter
    (fun old_job ->
      if not (List.mem old_job keep) then
        let entity =
          Midden.Entity_id.of_key ~namespace:"cron_job" (box ^ "/" ^ old_job)
        in
        ignore
          (midden_delete_if_live p ~entity ~author:box
             ~reason:("cron_job:del:" ^ old_job) ()))
    existing;
  List.iter
    (fun (job, schedule, enabled, detail) ->
      let entity =
        Midden.Entity_id.of_key ~namespace:"cron_job" (box ^ "/" ^ job)
      in
      let doc =
        TS.record
          [ TS.req_str "box" box;
            TS.req_str "detail" detail;
            TS.req_bool "enabled" enabled;
            TS.req_str "job" job;
            TS.opt_str "schedule" schedule ]
      in
      ignore
        (midden_upsert p ~entity ~schema:TS.cron_job_hash ~author:box
           ~reason:("cron_job:" ^ job) doc))
    jobs;
  Db.q ~params:[ p_str box ] p
    (Printf.sprintf "SELECT %s FROM cron_job WHERE box = $1 ORDER BY job"
       select_cron_job_cols)
  >>= fun rows -> Direct.return (List.map cron_job_of_row rows)

let list_cron_jobs p ?box () =
  ensure_midden p;
  Db.q ~params:[ p_opt box ] p
    (Printf.sprintf
       "SELECT %s FROM cron_job WHERE ($1::text IS NULL OR box = $1) \
        ORDER BY box, job"
       select_cron_job_cols)
  >>= fun rows -> Direct.return (List.map cron_job_of_row rows)

type cron_heartbeat = {
  ch_box : string;
  ch_name : string;
  ch_last_seen : string;
  ch_detail : string;
}

let select_cron_heartbeat_cols = "box, name, at::text, detail"

let cron_heartbeat_of_row r =
  {
    ch_box = text r 0 "cron_heartbeat.box";
    ch_name = text r 1 "cron_heartbeat.name";
    ch_last_seen = text r 2 "cron_heartbeat.at";
    ch_detail = Option.value (opt_text r 3) ~default:"{}";
  }

let put_cron_heartbeat p ~box ?(name = "") ~detail () =
  ensure_midden p;
  let entity =
    Midden.Entity_id.of_key ~namespace:"cron_heartbeat" (box ^ "/" ^ name)
  in
  let doc =
    TS.record
      [ TS.req_str "box" box;
        TS.req_str "detail" detail;
        TS.req_str "name" name ]
  in
  ignore
    (midden_upsert p ~entity ~schema:TS.cron_heartbeat_hash ~author:box
       ~reason:("cron_heartbeat:" ^ name) doc);
  Db.q ~params:[ p_str box; p_str name ] p
    (Printf.sprintf
       "SELECT %s FROM cron_heartbeat WHERE box = $1 AND name = $2"
       select_cron_heartbeat_cols)
  >>= function
  | [ r ] -> Direct.return (cron_heartbeat_of_row r)
  | n -> store_error "put_cron_heartbeat: %d rows" (List.length n)

let list_cron_heartbeats p ?box () =
  ensure_midden p;
  Db.q ~params:[ p_opt box ] p
    (Printf.sprintf
       "SELECT %s FROM cron_heartbeat WHERE ($1::text IS NULL OR box = $1) \
        ORDER BY box, name"
       select_cron_heartbeat_cols)
  >>= fun rows -> Direct.return (List.map cron_heartbeat_of_row rows)

(* -- native Midden claim helper (Midden M2 + Tuna claims) ------------ *)

type claim_row = {
  cl_entity : string;
  cl_path : string;
  cl_kind : string;
  cl_subject : string;
  cl_verdict : string;
  cl_body_hash : string;
  cl_author : string;
  cl_at : string;
}

let put_claim p ~path ~kind ~subject ~verdict ~body_bytes ~author =
  ensure_midden p;
  let body_hash = midden_exn "claim blob" (MS.put_blob (Db.midden p) body_bytes) in
  let entity = Midden.Entity_id.of_key ~namespace:"claim" path in
  let doc =
    TS.record
      [ TS.req_str "body-hash" body_hash;
        TS.req_str "kind" kind;
        TS.req_str "path" path;
        TS.req_str "subject" subject;
        TS.req_str "verdict" verdict ]
  in
  let receipt =
    midden_upsert p ~entity ~schema:TS.claim_hash ~author
      ~reason:("claim:" ^ kind) doc
  in
  Direct.return (entity, body_hash, receipt.MS.seq)

let list_claims p ?kind ?(limit = 500) () =
  ensure_midden p;
  Db.q ~params:[ p_opt kind; p_int limit ] p
    "SELECT entity::text, path, kind, subject, verdict, \"body-hash\", \
     author, at::text FROM claim_by_kind \
     WHERE ($1::text IS NULL OR kind = $1) \
     ORDER BY at DESC, path LIMIT $2"
  >>= fun rows ->
  Direct.return
    (List.map
       (fun r ->
         {
           cl_entity = text r 0 "claim.entity";
           cl_path = text r 1 "claim.path";
           cl_kind = text r 2 "claim.kind";
           cl_subject = text r 3 "claim.subject";
           cl_verdict = text r 4 "claim.verdict";
           cl_body_hash = text r 5 "claim.body-hash";
           cl_author = text r 6 "claim.author";
           cl_at = text r 7 "claim.at";
         })
       rows)
