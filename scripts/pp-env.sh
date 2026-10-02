# Tuna pp-slice instance env: THIS WORKTREE's isolated slots.
#
#   source scripts/pp-env.sh
#
# Slots owned by this loop (never re-use master's): PG :5436, HTTP :18092.
# The cluster lives at /tmp/tuna-pp-pg (data) + /tmp/tuna-pp-dev (state).
# Consumed by scripts/dev.sh, scripts/test-store.sh, dune runtest (PG-gated
# suites honor TUNA_DB_*), and the server binary itself.

ROOT="${TUNA_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

export TUNA_DB_HOST=/tmp
export TUNA_DB_PORT=5436
export TUNA_DB_NAME=tuna
export TUNA_DB_USER=tuna
export TUNA_DATA_DIR=/tmp/tuna-pp-pg
export TUNA_DEV_DIR=/tmp/tuna-pp-dev
export TUNA_HTTP_PORT=18092

# opam switch for every shell in this tree (AGENTS.md law)
eval "$(opam env --switch=poohstack --set-switch)"

echo "[pp-env] tuna pp-slice instance: PG socket /tmp:$TUNA_DB_PORT  HTTP :$TUNA_HTTP_PORT"
echo "[pp-env] root: $ROOT"
