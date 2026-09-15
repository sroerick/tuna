#!/bin/bash
# Tuna dev orchestration: temp Postgres + the server binary.
#
#   scripts/dev.sh start      # boot PG (if needed) + build + run server
#   scripts/dev.sh start-pg   # boot PG only
#   scripts/dev.sh stop       # stop server (leaves PG running)
#   scripts/dev.sh stop-pg    # stop PG (cluster persists)
#   scripts/dev.sh clean      # stop and DELETE the PG cluster
#   scripts/dev.sh status     # what's live
#
# Env (defaults below), all consumed by the server as TUNA_* too:
#   TUNA_DATA_DIR   (default /tmp/tuna-pgsup)
#   TUNA_DEV_DIR    (default /tmp/tuna-dev)
#   TUNA_DB_PORT    (default 5434)
#   TUNA_DB_NAME    (default tuna)
#   TUNA_DB_USER    (default tuna)
#   TUNA_HTTP_PORT  (default 18090)

set -e

ROOT="${TUNA_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
DATA_DIR="${TUNA_DATA_DIR:-/tmp/tuna-pgsup}"
DEV_DIR="${TUNA_DEV_DIR:-/tmp/tuna-dev}"
SOCKET_DIR="/tmp"
DB_PORT="${TUNA_DB_PORT:-5434}"
DB_NAME="${TUNA_DB_NAME:-tuna}"
DB_USER="${TUNA_DB_USER:-tuna}"
HTTP_PORT="${TUNA_HTTP_PORT:-18090}"
OPAMSwitch="poohstack"

mkdir -p "$DEV_DIR"
pg_pidfile="$DEV_DIR/pg.pid"
sr_pidfile="$DEV_DIR/server.pid"

have_opam_env=0
opam_env() {
  if [ "$have_opam_env" -eq 0 ]; then
    eval "$(opam env --switch=$OPAMSwitch --set-switch)"
    have_opam_env=1
  fi
}

db_up() {
  psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -tAc "SELECT 1" >/dev/null 2>&1
}

apply_migrations() {
  if db_up; then
    # idempotent SQL: each file records itself in schema_migrations
    for sql in "$ROOT"/migrations/*.sql; do
      [ -e "$sql" ] || continue
      name=$(basename "$sql")
      seen=$(psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -tAc \
        "SELECT 1 FROM schema_migrations WHERE name='$name'" 2>/dev/null || echo "")
      if [ "$seen" != "1" ]; then
        psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -v ON_ERROR_STOP=1 -f "$sql" >/dev/null \
          && psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c \
             "INSERT INTO schema_migrations (name) VALUES ('$name') ON CONFLICT DO NOTHING" >/dev/null \
          && echo "[dev] migration applied: $name"
      fi
    done
  fi
}

ensure_db() {
  # The tuna db/user may be missing even when the cluster is up
  # (dropped by hand, initdb re-run, etc.) — create it on EVERY path.
  if ! psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -tAc "SELECT 1" >/dev/null 2>&1; then
    psql -h "$SOCKET_DIR" -p "$DB_PORT" -U "$DB_USER" -d postgres -c "CREATE DATABASE $DB_NAME" >/dev/null
  fi
}

start_pg() {
  if [ -f "$pg_pidfile" ] && kill -0 "$(cat "$pg_pidfile")" 2>/dev/null; then
    echo "[dev] pg already running (pid $(cat "$pg_pidfile"))"
    ensure_db
    apply_migrations
    return
  fi
  if [ -d "$DATA_DIR" ] && pg_ctl -D "$DATA_DIR" status >/dev/null 2>&1; then
    echo "[dev] pg already running (cluster_exists)"
    pg_ctl -D "$DATA_DIR" status | head -1
    ensure_db
    apply_migrations
    return
  fi
  if [ ! -d "$DATA_DIR" ]; then
    echo "[dev] initializing PG data dir at $DATA_DIR"
    initdb -D "$DATA_DIR" -U "$DB_USER" --auth=trust >/dev/null
  fi
  echo "[dev] starting PG on port $DB_PORT (socket $SOCKET_DIR)"
  pg_ctl -D "$DATA_DIR" -o "-k $SOCKET_DIR -p $DB_PORT" -l "$DEV_DIR/pg.log" start
  sleep 1
  until db_up; do sleep 0.5; done
  ensure_db
  apply_migrations
  echo "[dev] pg ready (db=$DB_NAME on :$DB_PORT)"
}

build_all() {
  opam_env
  echo "[dev] building (poohstack)..."
  (cd "$ROOT" && dune build @all)
}

start_server() {
  build_all
  if [ -f "$sr_pidfile" ] && kill -0 "$(cat "$sr_pidfile")" 2>/dev/null; then
    echo "[dev] server already running (pid $(cat "$sr_pidfile")) on :$HTTP_PORT"
    return
  fi
  opam_env
  echo "[dev] starting server on :$HTTP_PORT (logs: $DEV_DIR/server.log)"
  # Re-boot discipline: if root already exists the server requires its
  # ORIGINAL token.  dev.sh keeps the generated token in
  # $DEV_DIR/bootstrap.token (first boot: absent -> server prints one
  # to the log, we capture it); operator TUNA_BOOTSTRAP_TOKEN wins.
  BOOT_TOKEN="${TUNA_BOOTSTRAP_TOKEN:-}"
  [ -n "$BOOT_TOKEN" ] || BOOT_TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/bootstrap.token" 2>/dev/null | tail -1)
  [ -n "$BOOT_TOKEN" ] || BOOT_TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/server.log" 2>/dev/null | tail -1)
  echo "[dev] boot token: ${BOOT_TOKEN:+known}${BOOT_TOKEN:-none (first boot)}"
  env TUNA_DB_HOST="$SOCKET_DIR" TUNA_DB_PORT="$DB_PORT" TUNA_DB_NAME="$DB_NAME" \
    TUNA_DB_USER="$DB_USER" TUNA_HTTP_PORT="$HTTP_PORT" \
    $( [ -n "$BOOT_TOKEN" ] && printf 'TUNA_BOOTSTRAP_TOKEN=%s' "$BOOT_TOKEN" ) \
    nohup "$ROOT/_build/default/server/bin/main.exe" >"$DEV_DIR/server.log" 2>&1 &
  sleep 0.3
  # persist a freshly generated token (after the log got its line)
  if grep -q '^TUNA_BOOTSTRAP_TOKEN=' "$DEV_DIR/server.log" 2>/dev/null; then
    grep '^TUNA_BOOTSTRAP_TOKEN=' "$DEV_DIR/server.log" | tail -1 > "$DEV_DIR/bootstrap.token"
  fi
  echo $! > "$sr_pidfile"
  sleep 1
  if curl -sS -m 3 "http://127.0.0.1:$HTTP_PORT/health" >/dev/null 2>&1; then
    echo "[dev] server ready (http://127.0.0.1:$HTTP_PORT)"
  else
    echo "[dev] server not answering yet — check $DEV_DIR/server.log" >&2
  fi
}

stop_server() {
  if [ -f "$sr_pidfile" ] && kill -0 "$(cat "$sr_pidfile")" 2>/dev/null; then
    kill "$(cat "$sr_pidfile")" && echo "[dev] server stopped"
    rm -f "$sr_pidfile"
  else
    echo "[dev] server not running"
  fi
}

stop_pg() {
  if pg_ctl -D "$DATA_DIR" status >/dev/null 2>&1; then
    pg_ctl -D "$DATA_DIR" -m fast stop && echo "[dev] pg stopped (cluster kept at $DATA_DIR)"
  else
    echo "[dev] pg not running"
  fi
}

status() {
  if pg_ctl -D "$DATA_DIR" status >/dev/null 2>&1; then
    echo "  running  pg on :$DB_PORT (db $DB_NAME)"
  else
    echo "  stopped  pg"
  fi
  if curl -sS -m 1 "http://127.0.0.1:$HTTP_PORT/health" >/dev/null 2>&1; then
    echo "  running  server on :$HTTP_PORT"
  else
    echo "  stopped  server"
  fi
  if db_up; then apply_migrations; fi
}

clean() {
  stop_server
  stop_pg
  rm -rf "$DATA_DIR" && echo "[dev] cluster deleted ($DATA_DIR)"
}

case "${1:-}" in
  start)     start_pg; start_server ;;
  start-pg)  start_pg ;;
  stop)      stop_server ;;
  stop-pg)   stop_pg ;;
  clean)     clean ;;
  status)    status ;;
  *) sed -n '2,13p' "$0" | sed 's/^# \{0,1\}//' ; exit 1 ;;
esac
