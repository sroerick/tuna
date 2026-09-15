(* DB connectivity: unix-socket pgx_lwt connection + a simple mvar
   pool.  Server entry points take a pool and use [with_pool]/[q] —
   every accessor in Store goes through here.

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
open Lwt.Infix

module Pg = Pgx_io
module V = Pgx.Value
(* param type is V.t (= v option): of_* constructors return t directly *)
let p_str s = V.of_string s

type config = { socket_dir : string; port : int; database : string; user : string }

let config_from_env () =
  let get k d = match Sys.getenv_opt k with Some v -> v | None -> d in
  { socket_dir = get "TUNA_DB_HOST" "/tmp"
  ; port = int_of_string (get "TUNA_DB_PORT" "5434")
  ; database = get "TUNA_DB_NAME" "tuna"
  ; user = get "TUNA_DB_USER" "tuna" }

let connect_one cfg =
  (* ~host = the socket dir: our IO instantiation (pgx_io.ml) mirrors
     libpq and routes '/'-prefixed hosts to <dir>/.s.PGSQL.<port>, so
     $PGHOST can never hijack a tuna connection. *)
  Pg.connect ~host:cfg.socket_dir ~port:cfg.port ~user:cfg.user
    ~database:cfg.database ()

type pool = {
  cfg : config
; free : Pg.t Lwt_mvar.t
; size : int
; created : int ref  (* connections created so far (<= size); Lwt is
                       cooperative so the check+incr is atomic w.r.t.
                       other pools users up to the first await *) }

let init ?(size = 8) cfg =
  (* connections are created LAZILY by with_pool up to [size], not
     pre-spun: pre-spinning deadlocks because Lwt_mvar.put blocks on a
     full mvar and nobody has taken the first connection yet. *)
  let free = Lwt_mvar.create_empty () in
  Lwt.return { cfg; free; size; created = ref 0 }

let take_conn ({ cfg; free; size; _ } as p) =
  match Lwt_mvar.take_available free with
  | Some c -> Lwt.return c  (* reuse a pooled connection when one is free *)
  | None ->
    if !(p.created) < size then begin
      p.created := !(p.created) + 1;
      connect_one cfg
    end
    else Lwt_mvar.take free  (* all size conns created and none free: wait for a return *)

(* NOTE: a connection whose protocol stream breaks is put back and will
   fail on next use; v0 does not replace it (single dev server, short
   lifetime).  Revisit if a long-lived daemon needs self-healing. *)
let with_pool ({ free; _ } as p) f =
  take_conn p >>= fun c ->
  Lwt.finalize (fun () -> f c) (fun () -> Lwt_mvar.put free c)

let ping p = with_pool p (fun c -> Pg.ping c)

(* run a parameterized query, returning raw rows *)
let q ?params p sql = with_pool p (fun c -> Pg.execute ?params c sql)
let q_unit ?params p sql = with_pool p (fun c -> Pg.execute_unit ?params c sql)

let apply_migrations p ~dir =
  (* ensure the bookkeeping table exists before querying it — on a
     fresh database the first migration file is what creates it *)
  q_unit p
    "CREATE TABLE IF NOT EXISTS schema_migrations (name text PRIMARY KEY, \
     applied_at timestamptz NOT NULL DEFAULT now())"
  >>= fun () ->
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort String.compare
  in
  let count = ref 0 in
  let apply file =
    let name = Filename.basename file in
    q
      ~params:[ p_str name ]
      p
      "SELECT 1 FROM schema_migrations WHERE name = $1"
    >>= fun seen ->
    (match seen with
    | [] ->
      let sql =
        let ic = open_in_bin (Filename.concat dir file) in
        let n = in_channel_length ic in
        let s = really_input_string ic n in
        close_in ic;
        s
      in
      with_pool p (fun c -> Pg.simple_query c sql) >>= fun _results ->
      q_unit
        ~params:[ p_str name ]
        p
        "INSERT INTO schema_migrations (name) VALUES ($1) ON CONFLICT (name) \
         DO NOTHING"
      >>= fun () -> count := !count + 1; Lwt.return ()
    | _ -> Lwt.return ())
  in
  let rec loop = function
    | [] -> Lwt.return !count
    | f :: rest -> apply f >>= fun () -> loop rest
  in
  loop files

(* Raw-connection queries for code that manages its own transaction
   scope (Store.ns_fork via with_tx); the pooled q/q_unit would take a
   SECOND connection and run outside the BEGIN/COMMIT. *)
let q_conn ?params c sql = Pg.execute ?params c sql
let q_conn_unit ?params c sql = Pg.execute_unit ?params c sql

(* Transactional multi-statement execution: [f] runs on one pooled
   connection inside BEGIN/COMMIT; any failure rolls back and re-raises. *)
let with_tx p (f : Pg.t -> 'a Lwt.t) : 'a Lwt.t =
  with_pool p (fun c ->
      Pg.execute_unit c "BEGIN"
      >>= fun () ->
      Lwt.catch
        (fun () ->
          f c >>= fun r -> Pg.execute_unit c "COMMIT" >>= fun () -> Lwt.return r)
        (fun e ->
          Pg.execute_unit c "ROLLBACK"
          >>= fun () -> Lwt.fail e))
