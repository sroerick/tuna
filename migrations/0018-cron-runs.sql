-- 0018: cron telemetry ingest (sundial T1) - the central cron record.
--
-- A cron run is NOT a claim and NOT an interpreter run: it is central
-- telemetry, high cardinality, cited by run_id, and it gets its own
-- table.  The box is the identity the bearer resolves to - a payload
-- naming another box is refused whole - so a compromised box forges
-- rows about itself and nothing else.  The shipper retries blindly:
-- UNIQUE (box, run_id) + ON CONFLICT DO UPDATE makes a duplicate post
-- a no-op and lets a later post supersede.  received_at is server
-- time, the only column central invents.  There is no dispatch
-- endpoint here: the moment central can name a command, central
-- compromise is RCE on every box.
--
-- Self-guarding like 0001/0013: inserts itself into schema_migrations
-- in the same transaction as the DDL; reapply is a no-op.

BEGIN;

CREATE TABLE IF NOT EXISTS cron_runs (
  box text NOT NULL,                    -- identity the bearer names
  run_id text NOT NULL,                 -- the box's own id for one run
  job text NULL,
  verdict text NULL,
  started_at timestamptz NULL,
  ended_at timestamptz NULL,
  detail jsonb NOT NULL DEFAULT '{}'::jsonb,
  received_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (box, run_id));

CREATE INDEX IF NOT EXISTS cron_runs_box_received_idx
  ON cron_runs (box, received_at DESC);
CREATE INDEX IF NOT EXISTS cron_runs_job_received_idx
  ON cron_runs (job, received_at DESC);
CREATE INDEX IF NOT EXISTS cron_runs_verdict_idx ON cron_runs (verdict);

CREATE TABLE IF NOT EXISTS cron_heartbeats (
  box text NOT NULL,
  name text NOT NULL DEFAULT '',
  last_seen timestamptz NOT NULL DEFAULT now(),
  detail jsonb NOT NULL DEFAULT '{}'::jsonb,
  PRIMARY KEY (box, name));

CREATE TABLE IF NOT EXISTS cron_jobs (
  box text NOT NULL,
  job text NOT NULL,
  schedule text NULL,
  enabled boolean NOT NULL DEFAULT true,
  detail jsonb NOT NULL DEFAULT '{}'::jsonb,
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (box, job));

CREATE TABLE IF NOT EXISTS cron_rollups (
  box text NOT NULL,
  job text NOT NULL,
  day date NOT NULL,
  runs bigint NOT NULL DEFAULT 0,
  ok bigint NOT NULL DEFAULT 0,
  fail bigint NOT NULL DEFAULT 0,
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (box, job, day));

INSERT INTO schema_migrations (name) VALUES ('0018-cron-runs.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
