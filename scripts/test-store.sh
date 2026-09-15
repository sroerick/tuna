#!/bin/bash
# Gate for the M5 store integration tests (run via tests/dune):
#   - postgres down  -> skip silently (unit tests must never need PG)
#   - postgres up    -> TUNA_TEST_PG=1 store_tests with migrations dir
# Usage: test-store.sh <exe> <migrations-dir> [db-name]
set -e
exe=$(readlink -f "$1"); mig=$2
DB_USER="${TUNA_DB_USER:-tuna}"
if ! psql -h /tmp -p "${TUNA_DB_PORT:-5434}" -U "$DB_USER" \
     -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
  echo "store tests skipped (postgres down; scripts/dev.sh start-pg)"
  exit 0
fi
# The suite runs against its OWN scratch database (tuna_test): it
# bootstraps identities named 'root'/'m7-root', and running it against
# the dev db would poison the dev server's TUNA_BOOTSTRAP boot
# (bootstrap_identity is get-or-create — a foreign 'root' row makes
# the server refuse to start).  Recreated fresh on every run.
TEST_DB="${3:-tuna_test}"
psql -h /tmp -p "${TUNA_DB_PORT:-5434}" -U "$DB_USER" -d postgres -q -c \
  "DROP DATABASE IF EXISTS $TEST_DB" >/dev/null
psql -h /tmp -p "${TUNA_DB_PORT:-5434}" -U "$DB_USER" -d postgres -q -c \
  "CREATE DATABASE $TEST_DB" >/dev/null
TUNA_TEST_PG=1 TUNA_TEST_MIGRATIONS="$mig" TUNA_DB_NAME="$TEST_DB" "$exe"
