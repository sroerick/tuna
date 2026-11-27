(* Tuna_store.Tuna_schemas: authoritative Midden schemas and declarative
   projections for Tuna's state/product plane.

   Every schema here is registered idempotently into Midden_store on
   first touch / server boot, and its canonical SHA-256 hash is pinned
   by Midden.Borge_binding.schema_hash so tuna/tuna.borg can pin them by
   identity. *)

module C = Midden.Csexp
module S = Midden.Schema
module MS = Midden_store.Store

let atom s = C.Atom s
let form xs = C.List xs

let schema_form name version fields =
  form
    [ atom "schema"; atom name;
      form [ atom "version"; atom (string_of_int version) ];
      form (atom "fields" :: fields) ]

let field ?(opt = false) name typ =
  let base = [ atom name; atom typ ] in
  form (if opt then base @ [ atom ":optional" ] else base)

let def_form =
  schema_form "def" 1
    [ field "identity-id" ":string";
      field "identity-name" ":string";
      field "name" ":string";
      field "value-hash" ":string" ]

let def_visibility_form =
  schema_form "def_visibility" 1
    [ field "identity" ":string";
      field "public" ":bool" ]

let repl_session_form =
  schema_form "repl_session" 1
    [ field "identity-id" ":string";
      field "last-run-id" ":string" ]

let message_form =
  schema_form "message" 1
    [ field "body" ":string";
      field "body-hash" ":string";
      field "kind" ":string";
      field ~opt:true "re" ":string";
      field "recipient" ":string";
      field "sender" ":string" ]

let retention_policy_form =
  schema_form "retention_policy" 1
    [ field "enabled" ":bool";
      field "identity-id" ":string";
      field "keep-days" ":int";
      field "keep-failures" ":bool" ]

let cron_run_form =
  schema_form "cron_run" 1
    [ field "box" ":string";
      field "detail" ":string";
      field ~opt:true "ended-at" ":string";
      field ~opt:true "job" ":string";
      field "run-id" ":string";
      field ~opt:true "started-at" ":string";
      field ~opt:true "verdict" ":string" ]

let cron_job_form =
  schema_form "cron_job" 1
    [ field "box" ":string";
      field "detail" ":string";
      field "enabled" ":bool";
      field "job" ":string";
      field ~opt:true "schedule" ":string" ]

let cron_heartbeat_form =
  schema_form "cron_heartbeat" 1
    [ field "box" ":string";
      field "detail" ":string";
      field "name" ":string" ]

let todo_form =
  schema_form "todo" 1
    [ field "created" ":int";
      field "state" ":string";
      field "title" ":string";
      field "who" ":string" ]

let claim_form =
  schema_form "claim" 1
    [ field "body-hash" ":string";
      field "kind" ":string";
      field "path" ":string";
      field "subject" ":string";
      field "verdict" ":string" ]

let tree_compat_form =
  schema_form "tree_compat" 1
    [ field "owner" ":string";
      field "path" ":string";
      field "value-hash" ":string";
      field "version" ":int" ]

let op_audit_form =
  schema_form "op_audit" 1
    [ field "actor" ":string";
      field "op" ":string";
      field "op-hash" ":string";
      field "op-seq" ":int";
      field "path" ":string";
      field ~opt:true "prev-version" ":int";
      field "ts-unix" ":int";
      field ~opt:true "value-hash" ":string";
      field ~opt:true "version" ":int" ]

let parse_exn f =
  match S.of_csexp f with
  | Ok s -> s
  | Error e -> failwith ("tuna_schemas: " ^ S.string_of_error e)

let def_schema = parse_exn def_form
let def_visibility_schema = parse_exn def_visibility_form
let repl_session_schema = parse_exn repl_session_form
let message_schema = parse_exn message_form
let retention_policy_schema = parse_exn retention_policy_form
let cron_run_schema = parse_exn cron_run_form
let cron_job_schema = parse_exn cron_job_form
let cron_heartbeat_schema = parse_exn cron_heartbeat_form
let todo_schema = parse_exn todo_form
let claim_schema = parse_exn claim_form
let tree_compat_schema = parse_exn tree_compat_form
let op_audit_schema = parse_exn op_audit_form

let all_schemas =
  [ def_schema; def_visibility_schema; repl_session_schema; message_schema;
    retention_policy_schema; cron_run_schema; cron_job_schema;
    cron_heartbeat_schema; todo_schema; claim_schema; tree_compat_schema;
    op_audit_schema ]

let schema_hash s = Midden.Borge_binding.schema_hash s

let def_hash = schema_hash def_schema
let def_visibility_hash = schema_hash def_visibility_schema
let repl_session_hash = schema_hash repl_session_schema
let message_hash = schema_hash message_schema
let retention_policy_hash = schema_hash retention_policy_schema
let cron_run_hash = schema_hash cron_run_schema
let cron_job_hash = schema_hash cron_job_schema
let cron_heartbeat_hash = schema_hash cron_heartbeat_schema
let todo_hash = schema_hash todo_schema
let claim_hash = schema_hash claim_schema
let tree_compat_hash = schema_hash tree_compat_schema
let op_audit_hash = schema_hash op_audit_schema

let schema_by_name name =
  List.find_opt (fun (s : S.schema) -> String.equal s.name name) all_schemas

(* Declarative projections maintained atomically by Midden_store. *)

let todo_counts_proj =
  form
    [ atom "projection"; atom "todo_counts";
      form [ atom "reads"; atom "todo"; atom "1" ];
      form [ atom "group"; atom "state" ];
      form [ atom "aggregate"; form [ atom "count" ] ] ]

let todo_open_proj =
  form
    [ atom "projection"; atom "todo_open";
      form [ atom "reads"; atom "todo"; atom "1" ];
      form [ atom "where"; form [ atom "="; atom "state"; atom "\"open\"" ] ];
      form [ atom "columns"; atom "created"; atom "state"; atom "title"; atom "who" ];
      form [ atom "index"; form [ atom "created" ] ] ]

let todo_by_created_proj =
  form
    [ atom "projection"; atom "todo_by_created";
      form [ atom "reads"; atom "todo"; atom "1" ];
      form [ atom "columns"; atom "created"; atom "state"; atom "title"; atom "who" ];
      form [ atom "index"; form [ atom "created" ]; form [ atom "state"; atom "created" ] ] ]

let def_view_proj =
  form
    [ atom "projection"; atom "def_view";
      form [ atom "reads"; atom "def"; atom "1" ];
      form [ atom "columns"; atom "identity-id"; atom "identity-name"; atom "name"; atom "value-hash"; atom "at" ];
      form [ atom "index"; form [ atom "identity-id"; atom "name" ]; form [ atom "identity-name"; atom "name" ] ] ]

let message_inbox_proj =
  form
    [ atom "projection"; atom "message_inbox";
      form [ atom "reads"; atom "message"; atom "1" ];
      form [ atom "columns"; atom "seq"; atom "sender"; atom "recipient"; atom "kind"; atom "re"; atom "body"; atom "body-hash"; atom "at" ];
      form [ atom "index"; form [ atom "recipient"; atom "seq" ]; form [ atom "seq" ] ] ]

let cron_run_idx_proj =
  form
    [ atom "projection"; atom "cron_run_idx";
      form [ atom "reads"; atom "cron_run"; atom "1" ];
      form [ atom "columns"; atom "box"; atom "run-id"; atom "job"; atom "verdict"; atom "started-at"; atom "ended-at"; atom "detail"; atom "at" ];
      form [ atom "index"; form [ atom "box"; atom "run-id" ]; form [ atom "job" ]; form [ atom "verdict" ] ] ]

let claim_by_kind_proj =
  form
    [ atom "projection"; atom "claim_by_kind";
      form [ atom "reads"; atom "claim"; atom "1" ];
      form [ atom "columns"; atom "path"; atom "kind"; atom "subject"; atom "verdict"; atom "body-hash"; atom "author"; atom "at" ];
      form [ atom "index"; form [ atom "kind" ]; form [ atom "verdict" ] ] ]

let tree_compat_by_path_proj =
  form
    [ atom "projection"; atom "tree_compat_by_path";
      form [ atom "reads"; atom "tree_compat"; atom "1" ];
      form [ atom "columns"; atom "path"; atom "value-hash"; atom "version"; atom "owner"; atom "at" ];
      form [ atom "index"; form [ atom "path" ] ] ]

let op_audit_by_seq_proj =
  form
    [ atom "projection"; atom "op_audit_by_seq";
      form [ atom "reads"; atom "op_audit"; atom "1" ];
      form [ atom "columns"; atom "op-seq"; atom "path"; atom "op"; atom "value-hash"; atom "prev-version"; atom "version"; atom "actor"; atom "ts-unix"; atom "op-hash" ];
      form [ atom "index"; form [ atom "op-seq" ]; form [ atom "path"; atom "op-seq" ] ] ]

let all_projections =
  [ todo_counts_proj; todo_open_proj; todo_by_created_proj; def_view_proj;
    message_inbox_proj; cron_run_idx_proj; claim_by_kind_proj;
    tree_compat_by_path_proj; op_audit_by_seq_proj ]

let ensure_all (p : Db.pool) =
  if not p.Db.schemas_ready then begin
    let m = Db.midden p in
    List.iter
      (fun (sc : S.schema) ->
        match MS.register_schema m (S.to_csexp sc) with
        | Ok _ -> ()
        | Error e ->
            failwith
              (Printf.sprintf "tuna_schemas: register %s failed: %s" sc.name
                 (MS.string_of_error e)))
      all_schemas;
    List.iter
      (fun proj_form ->
        match MS.define_projection m proj_form with
        | Ok _ -> ()
        | Error e ->
            failwith
              (Printf.sprintf "tuna_schemas: define_projection failed: %s"
                 (MS.string_of_error e)))
      all_projections;
    p.Db.schemas_ready <- true
  end

(* Helper to build a canonical Midden document record (byte-sorted, omitting None). *)
let record (fields : (string * C.t option) list) : C.t =
  let present =
    List.filter_map
      (fun (k, v_opt) -> Option.map (fun v -> (k, v)) v_opt)
      fields
  in
  match Midden.Record.canonical present with
  | Ok doc -> doc
  | Error e -> failwith ("tuna_schemas.record: " ^ Midden.Record.string_of_error e)

let req_str k v = (k, Some (C.Atom v))
let opt_str k v = (k, Option.map (fun s -> C.Atom s) v)
let req_int k v = (k, Some (C.Atom (Int64.to_string v)))
let opt_int k v = (k, Option.map (fun n -> C.Atom (Int64.to_string n)) v)
let req_bool k v = (k, Some (C.Atom (if v then "true" else "false")))
