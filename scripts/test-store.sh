#!/bin/bash
# Gate for the M5 store integration tests (run via tests/dune):
#   - postgres down  -> skip silently (unit tests must never need PG)
#   - postgres up    -> TUNA_TEST_PG=1 store_tests with migrations dir
# Usage: test-store.sh <exe> <migrations-dir>
set -e
exe=$(readlink -f "$1"); mig=$2
if ! psql -h /tmp -p "${TUNA_DB_PORT:-5434}" -U "${TUNA_DB_USER:-tuna}" \
     -d "${TUNA_DB_NAME:-tuna}" -tAc "SELECT 1" >/dev/null 2>&1; then
  echo "store tests skipped (postgres down; scripts/dev.sh start-pg)"
  exit 0
fi
TUNA_TEST_PG=1 TUNA_TEST_MIGRATIONS="$mig" "$exe"
