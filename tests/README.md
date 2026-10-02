# Tests

Alcotest suites; `dune runtest` runs them all. PG-gated integration
suites (store, substrate, m11, fed, deriv, demand) go through
`scripts/test-store.sh`: postgres down -> skipped silently, postgres up
-> each suite gets its own scratch database.

## demand suite (2026-10-02)

`tests/demand_tests.ml` pins the demand-memo boundary
(borg/sharing.borg §demand-memo) end-to-end through `Run.execute_run`:
a clean firing computed by run 1 is cached in the caller's garden and a
later run of the same program serves it free (fewer distinct firings,
same result hash); a program whose only firing answers a prim persists
NOTHING (the hardened dirty rule - the prim re-executes live on every
run); identities do not see each other's gardens (a warms, b stays
cold, b's own repeat hits); and the feature is off by default (no
garden read, no garden write).

## deriv PG e2e (un-deferred 2026-10-01)

The deriv "pg" group in deriv_tests.ml (server-built record verifies
offline through Tuna_deriv alone; builder refusals 404/409) now runs
green against the recovered dev cluster. The 2026-09-30 deferral was an
environment fault, not a code fault:

- :5434 answered on the /tmp socket but its cluster was broken - the
  postmaster was a zombie whose data dir had lost `PG_VERSION` and
  `global/pg_control` (`FATAL: could not open file "base/16388/2601"`).
  Fixed by killing the zombie, `rm -rf /tmp/tuna-pgsup`, re-`initdb`
  and re-applying migrations (`scripts/dev.sh` bails on the missing
  `PG_VERSION`, so the re-init was by hand).
- Nothing to flip in the suite: it stayed TUNA_TEST_PG-gated, so CI
  (service-container PG) and any healthy dev stack
  (`scripts/dev.sh start-pg`) run it exactly as written.
