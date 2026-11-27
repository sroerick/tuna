-- 0020: retention policies (journal/retention-gc policy knob).
--
-- Journals are unbounded by semantics, so retention is a POLICY knob
-- per caller identity.  One row per identity: keep_days (> 0) is the
-- age cut, keep_failures decides whether failed and unverified runs
-- are spared.  NO row, or enabled = false, collects nothing: there is
-- no built-in period and no timer in the server, so a sweep is an
-- explicit admin call (a cron entry may make it).  GC of a journal
-- severs replayability, so collection always goes through the existing
-- single-transaction tombstone path (journal_gced_at +
-- journal_gced_policy + verify_status 'gced') and never a silent
-- delete that would later read as successful verification.
-- Self-guarding like 0001: inserts itself into schema_migrations in
-- the same transaction as the DDL.

BEGIN;

CREATE TABLE IF NOT EXISTS retention_policies (
  identity uuid PRIMARY KEY REFERENCES identities(id) ON DELETE CASCADE,
  keep_days int NOT NULL CHECK (keep_days > 0),
  keep_failures bool NOT NULL DEFAULT true,
  enabled bool NOT NULL DEFAULT false,
  updated_by uuid NULL REFERENCES identities(id),
  updated_at timestamptz NOT NULL DEFAULT now());

INSERT INTO schema_migrations (name) VALUES ('0020-retention-policies.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
