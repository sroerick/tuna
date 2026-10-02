(* DB connectivity: pure-pgx-over-Eio connection + a fiber pool.

   rim-eio (borg/rim-eio.borg §store): pgx_lwt departs; the pool is an
   Eio [Mutex]/[Condition] over a free-list instead of the Lwt_mvar,
   and every accessor is direct-style (see Direct).  Server entry
   points take a pool and use [with_pool]/[q].

   The pool laws from the Lwt version survive verbatim: connections are
   created LAZILY up to [size], and a release can never block.  The
   mvar's list-cell subtlety is replaced by an explicit free list under
   one mutex; a taker that finds the list empty and every slot taken
   waits on the condition until a release signals.

   Env contract (same as scripts/dev.sh):
     TUNA_DB_HOST  socket dir      default /tmp
     TUNA_DB_PORT  postgres port   default 5434
     TUNA_DB_NAME  database        default tuna
     TUNA_DB_USER  user            default tuna

   Migration application: the SERVER assumes the schema exists
   (scripts/dev.sh applies migrations/*.sql).  [apply_migrations] is
   provided for tests/scripts that must bootstrap a fresh cluster; it
   re-implements dev.sh's runner semantics (each file guards itself in
   schema_migrations, applied in lexical order). *)

module Pg = Pgx_eio
module V = Pgx.Value

(* param type is V.t (= v option): of_* constructors return t directly *)
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

let connect_one cfg =
  (* ~host = the socket dir: our IO instantiation (pgx_eio.ml) mirrors
     libpq and routes '/'-prefixed hosts to <dir>/.s.PGSQL.<port>, so
     $PGHOST can never hijack a tuna connection. *)
  (Pg.connect ~host:cfg.socket_dir ~port:cfg.port ~user:cfg.user
     ~database:cfg.database () : Pg.t)

type pool = {
  cfg : config;
  size : int;
  mutable free : Pg.t list;
  mutable created : int;
  mutable waiters : int;
  mutex : Eio.Mutex.t;
  cond : Eio.Condition.t;
}

let init ?(size = 8) cfg =
  (* connections are created LAZILY by with_pool up to [size]. *)
  {
    cfg;
    size;
    free = [];
    created = 0;
    waiters = 0;
    mutex = Eio.Mutex.create ();
    cond = Eio.Condition.create ();
  }

(* Acquire a connection, blocking the calling FIBER (never the domain)
   when all [size] connections exist and none is idle.  [created] is
   reserved under the mutex before the (network-blocking) connect, so
   concurrent takers never over-create. *)
let take_conn ({ cfg; size; mutex; cond; _ } as p) =
  let rec go () =
    Eio.Mutex.lock mutex;
    match p.free with
    | c :: rest ->
        p.free <- rest;
        Eio.Mutex.unlock mutex;
        c
    | [] ->
        if p.created < size then begin
          p.created <- p.created + 1;
          Eio.Mutex.unlock mutex;
          match connect_one cfg with
          | c -> c
          | exception e ->
              Eio.Mutex.lock mutex;
              p.created <- p.created - 1;
              Eio.Condition.broadcast cond;
              Eio.Mutex.unlock mutex;
              raise e
        end
        else begin
          (* all conns exist and none idle: wait for a release *)
          p.waiters <- p.waiters + 1;
          Eio.Condition.await cond mutex;
          p.waiters <- p.waiters - 1;
          Eio.Mutex.unlock mutex;
          go ()
        end
  in
  go ()

(* Non-blocking release: reinsert on the free list and wake a waiter. *)
let release ({ mutex; cond; _ } as p) c =
  Eio.Mutex.lock mutex;
  p.free <- c :: p.free;
  Eio.Condition.broadcast cond;
  Eio.Mutex.unlock mutex

(* NOTE: a connection whose protocol stream breaks is put back and will
   fail on next use; v0 does not replace it (single dev server, short
   lifetime).  Revisit if a long-lived service needs self-healing. *)
let with_pool ({ free = _; _ } as p) f =
  let c = take_conn p in
  Fun.protect ~finally:(fun () -> release p c) (fun () -> f c)

let ping p = with_pool p (fun c -> Pg.ping c)

(* run a parameterized query, returning raw rows *)
let q ?params p sql = with_pool p (fun c -> Pg.execute ?params c sql)
let q_unit ?params p sql = with_pool p (fun c -> Pg.execute_unit ?params c sql)

let apply_migrations p ~dir =
  (* ensure the bookkeeping table exists before querying it — on a
     fresh database the first migration file is what creates it *)
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
    q ~params:[ p_str name ] p
      "SELECT 1 FROM schema_migrations WHERE name = $1"
    |> function
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

(* Raw-connection queries for code that manages its own transaction
   scope (Store.ns_fork via with_tx); the pooled q/q_unit would take a
   SECOND connection and run outside the BEGIN/COMMIT. *)
let q_conn ?params c sql = Pg.execute ?params c sql
let q_conn_unit ?params c sql = Pg.execute_unit ?params c sql

(* Transactional multi-statement execution: [f] runs on one pooled
   connection inside BEGIN/COMMIT; any failure rolls back and re-raises. *)
let with_tx p (f : Pg.t -> 'a) : 'a =
  with_pool p (fun c ->
      Pg.execute_unit c "BEGIN";
      match f c with
      | r ->
          Pg.execute_unit c "COMMIT";
          r
      | exception e ->
          (try Pg.execute_unit c "ROLLBACK" with _ -> ());
          raise e)
