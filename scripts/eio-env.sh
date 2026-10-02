#!/usr/bin/env bash
# rim-eio worktree env (branch rim-eio).  Source this before every
# command in this worktree.  Ports are the reserved eio slots: PG :5437,
# HTTP :18097 (see .ralph/rim-eio.md isolation notes).  Everything takes
# the TUNA_* knobs scripts/dev.sh + scripts/test-store.sh already honor.
export TUNA_DB_HOST=/tmp
export TUNA_DB_PORT=5437
export TUNA_DB_NAME=tuna
export TUNA_DB_USER=tuna
export TUNA_DATA_DIR=/tmp/tuna-eio-pg
export TUNA_DEV_DIR=/tmp/tuna-eio-dev
export TUNA_HTTP_PORT=18097
