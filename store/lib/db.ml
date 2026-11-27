(* DB connectivity: wraps Midden_store.Store / Midden_store.Db so Tuna's
   lean kernel tables and Midden's durable store share ONE fiber pool,
   ONE Eio context, and ONE Postgres connection pool.

   Connection and Midden bootstrap are LAZY (matching the original lazy
   pool semantics so pure tests like agent_contract_tests can construct a
   dummy pool without an active Eio context). *)

module Pg = Pgx_eio
module V = Pgx.Value
module MDb = Midden_store.Db
module MStore = Midden_store.Store

let p_str s = V.of_string s

type config = {
  socket_dir : string;
  port : int;
  database : string;
  user : string;
}

let config_from_env () =
  let get k d = match Sys.getenv_opt k with Some v -> v | None -> d in
  {
    socket_dir = get "TUNA_DB_HOST" "/tmp";
    port = int_of_string (get "TUNA_DB_PORT" "5434");
    database = get "TUNA_DB_NAME" "tuna";
    user = get "TUNA_DB_USER" "tuna";
  }

type pool = {
  cfg : config;
  size : int;
  mutable midden_opt : MStore.t option;
  mutable schemas_ready : bool;
}

let to_midden_cfg (cfg : config) : MDb.config =
  {
    MDb.host = cfg.socket_dir;
    port = cfg.port;
    database = cfg.database;
    user = cfg.user;
  }

let init ?(size = 8) cfg =
  { cfg; size; midden_opt = None; schemas_ready = false }

let midden p =
  match p.midden_opt with
  | Some m -> m
  | None ->
      let m = MStore.connect ~size:p.size (to_midden_cfg p.cfg) in
      p.midden_opt <- Some m;
      m

let raw_pool p = MStore.pool (midden p)

let with_pool p f = MDb.with_pool (raw_pool p) f
let ping p = MStore.ping (midden p)
let q ?params p sql = MDb.q ?params (raw_pool p) sql
let q_unit ?params p sql = MDb.q_unit ?params (raw_pool p) sql
let q_conn ?params c sql = MDb.q_conn ?params c sql
let q_conn_unit ?params c sql = MDb.q_conn_unit ?params c sql
let with_tx p f = MDb.with_tx (raw_pool p) f

let apply_migrations p ~dir =
  q_unit p
    "CREATE TABLE IF NOT EXISTS schema_migrations (name text PRIMARY KEY, \
     applied_at timestamptz NOT NULL DEFAULT now())";
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort String.compare
  in
  let count = ref 0 in
  let apply file =
    let name = Filename.basename file in
    match
      q ~params:[ p_str name ] p
        "SELECT 1 FROM schema_migrations WHERE name = $1"
    with
    | [] ->
        let sql =
          let ic = open_in_bin (Filename.concat dir file) in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          s
        in
        ignore (with_pool p (fun c -> Pg.simple_query c sql));
        q_unit ~params:[ p_str name ] p
          "INSERT INTO schema_migrations (name) VALUES ($1) ON CONFLICT (name) \
           DO NOTHING";
        count := !count + 1
    | _ -> ()
  in
  List.iter apply files;
  !count
