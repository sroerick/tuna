#!/usr/bin/env bash
# pp-slice T4: long-lived serving kit, distinct from scripts/dev.sh.
#
#   scripts/serve.sh start    # PG (if needed) + migrations + build + serve
#   scripts/serve.sh stop     # stop the server (leaves PG running)
#   scripts/serve.sh status   # what is live
#   scripts/serve.sh restart
#
# Unlike dev.sh (a rebuild-and-run dev loop), serve.sh is the operator
# path: it applies migrations, (re)generates the public artifacts
# (src.tgz), serves the built binary under nohup with a pidfile, and
# leaves the process up.  All concrete knobs come from TUNA_* env (see
# scripts/pp-env.sh for the isolated pp-slice slots).  PG lifecycle is
# delegated to dev.sh start-pg, which honors the same TUNA_DB_* knobs.
#
# Env:
#   TUNA_DATA_DIR / TUNA_DEV_DIR / TUNA_DB_HOST / TUNA_DB_PORT /
#   TUNA_DB_NAME / TUNA_DB_USER / TUNA_HTTP_PORT / TUNA_PUBLIC_DIR
#   TUNA_OPAM_SWITCH (default poohstack; empty = current switch)
#   TUNA_BOOTSTRAP_TOKEN / TUNA_BOOTSTRAP_PASSWORD (see Api.serve)
set -e

ROOT="${TUNA_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
DEV_DIR="${TUNA_DEV_DIR:-/tmp/tuna-pp-dev}"
HTTP_PORT="${TUNA_HTTP_PORT:-18092}"
PUBLIC_DIR="${TUNA_PUBLIC_DIR:-/tmp/tuna-pp-public}"
OPAM_SWITCH="${TUNA_OPAM_SWITCH-poohstack}"

mkdir -p "$DEV_DIR" "$PUBLIC_DIR"
sr_pidfile="$DEV_DIR/serve.pid"

have_opam_env=0
opam_env() {
  if [ "$have_opam_env" -eq 0 ]; then
    if [ -n "$OPAM_SWITCH" ]; then
      eval "$(opam env --switch="$OPAM_SWITCH" --set-switch)"
    else
      eval "$(opam env)"
    fi
    LIBEV_DIR="${TUNA_LIBEV_DIR:-$HOME/.local/lib}"
    [ -d "$LIBEV_DIR" ] && export LIBRARY_PATH="${LIBRARY_PATH:+$LIBRARY_PATH:}$LIBEV_DIR"
    PG_BIN="${TUNA_PG_BIN:-$HOME/pg/bin}"
    if [ -d "$PG_BIN" ]; then
      export PATH="$PG_BIN:$PATH"
      export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$PG_BIN/../lib"
    fi
    have_opam_env=1
  fi
}

running() { [ -f "$sr_pidfile" ] && kill -0 "$(cat "$sr_pidfile")" 2>/dev/null; }

start() {
  opam_env
  echo "[serve] booting PG via dev.sh start-pg (TUNA_DB_* honored)..."
  "$ROOT/scripts/dev.sh" start-pg
  echo "[serve] building @all..."
  ( cd "$ROOT" && dune build @all )
  echo "[serve] building public artifacts..."
  "$ROOT/scripts/deploy/build-public.sh" "$PUBLIC_DIR"
  if running; then
    echo "[serve] server already running (pid $(cat "$sr_pidfile")) on :$HTTP_PORT"
    return
  fi
  opam_env
  # Same re-boot discipline as dev.sh: the root identity requires its
  # ORIGINAL token, kept in $DEV_DIR/bootstrap.token from the first boot.
  BOOT_TOKEN="${TUNA_BOOTSTRAP_TOKEN:-}"
  [ -n "$BOOT_TOKEN" ] || BOOT_TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/bootstrap.token" 2>/dev/null | tail -1)
  [ -n "$BOOT_TOKEN" ] || BOOT_TOKEN=$(sed -n 's/^TUNA_BOOTSTRAP_TOKEN=//p' "$DEV_DIR/server.log" 2>/dev/null | tail -1)
  echo "[serve] starting server on :$HTTP_PORT (logs: $DEV_DIR/server.log)"
  env TUNA_DB_HOST="${TUNA_DB_HOST:-/tmp}" \
    TUNA_DB_PORT="${TUNA_DB_PORT:-5436}" \
    TUNA_DB_NAME="${TUNA_DB_NAME:-tuna}" \
    TUNA_DB_USER="${TUNA_DB_USER:-tuna}" \
    TUNA_HTTP_PORT="$HTTP_PORT" \
    TUNA_STATIC_DIR="$ROOT/server/static" \
    TUNA_PUBLIC_DIR="$PUBLIC_DIR" \
    $( [ -n "$BOOT_TOKEN" ] && printf 'TUNA_BOOTSTRAP_TOKEN=%s' "$BOOT_TOKEN" ) \
    nohup "$ROOT/_build/default/server/bin/main.exe" >"$DEV_DIR/server.log" 2>&1 &
  echo $! > "$sr_pidfile"
  sleep 0.3
  if grep -q '^TUNA_BOOTSTRAP_TOKEN=' "$DEV_DIR/server.log" 2>/dev/null; then
    grep '^TUNA_BOOTSTRAP_TOKEN=' "$DEV_DIR/server.log" | tail -1 > "$DEV_DIR/bootstrap.token"
  fi
  sleep 1
  if curl -sS -m 3 "http://127.0.0.1:$HTTP_PORT/health" >/dev/null 2>&1; then
    echo "[serve] ready (http://127.0.0.1:$HTTP_PORT)"
  else
    echo "[serve] not answering yet — check $DEV_DIR/server.log" >&2
  fi
}

stop() {
  if running; then
    kill "$(cat "$sr_pidfile")" && rm -f "$sr_pidfile" && echo "[serve] server stopped"
  else
    echo "[serve] server not running"
  fi
}

status() {
  if running; then
    echo "  running  serve on :$HTTP_PORT (pid $(cat "$sr_pidfile"))"
  else
    echo "  stopped  serve"
  fi
  if curl -sS -m 1 "http://127.0.0.1:$HTTP_PORT/health" >/dev/null 2>&1; then
    echo "  health   ok"
  fi
}

case "${1:-}" in
  start)   start ;;
  stop)    stop ;;
  restart) stop; start ;;
  status)  status ;;
  *) sed -n '2,9p' "$0" | sed 's/^# \{0,1\}//'; exit 1 ;;
esac
