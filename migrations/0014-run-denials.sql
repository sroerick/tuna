-- 0014: run denial surfacing (grants.invocation addendum).
--
-- The v0.2 todo-probe defect (FINDINGS.md non-F 2026-10-02, defect 1):
-- a run whose prim call is denied finishes status=normal with a
-- poisoned result; the denial is visible only by reading journal rows.
-- The law is unchanged (denial is data; the run continues per program
-- semantics; acceptance 1 still pins status normal) — what changes is
-- OBSERVABILITY: the run row now carries the count of journaled grant
-- denials, so a run's outcome shows its own denials without a journal
-- scan.  Additive and defaulted: every existing run reads 0.
-- Self-guarding like 0001: inserts itself into schema_migrations in
-- the same transaction as the DDL.

BEGIN;

ALTER TABLE runs ADD COLUMN IF NOT EXISTS denial_count bigint NOT NULL DEFAULT 0;

INSERT INTO schema_migrations (name) VALUES ('0014-run-denials.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
