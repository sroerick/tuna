# Tests

Alcotest suites; `dune runtest` runs them all. PG-gated integration
suites (store, substrate, m11, fed, deriv) go through
`scripts/test-store.sh`: postgres down -> skipped silently, postgres up
-> each suite gets its own scratch database.

## Deferred: deriv PG e2e (2026-09-30)

The deriv "pg" group in deriv_tests.ml (server-built record verifies
offline through Tuna_deriv alone; builder refusals 404/409) is written
and compiles green, but this box has no usable Postgres to drive it
against:

- :5434 answers on the /tmp socket but its cluster is broken - the
  postmaster is alive and its data dir is gone
  (`FATAL: could not open file "base/1/2601"`); cleaning that up needs
  a human, not a test run.
- :5433 is a different cluster without the `tuna` role.

Per standing rules we did not install Postgres. Nothing to flip: the
suites stay TUNA_TEST_PG-gated, so CI (service-container PG) and any
healthy dev stack (`scripts/dev.sh start-pg`) run them exactly as
written.
