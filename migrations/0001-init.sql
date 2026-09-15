-- 0001 initial schema: identities, programs, runs, journals, grants.
-- Idempotent by construction: this file runs once; inserts itself into
-- schema_migrations in the same transaction as the DDL.

BEGIN;

CREATE TABLE IF NOT EXISTS schema_migrations (
  name text PRIMARY KEY,
  applied_at timestamptz NOT NULL DEFAULT now());

-- who is calling: agent identities with bearer tokens (tuna-grants grant-token)
CREATE TABLE IF NOT EXISTS identities (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text UNIQUE NOT NULL,
  token_hash text NOT NULL,             -- sha256 hex of bearer token
  is_admin bool NOT NULL DEFAULT false,
  created_at timestamptz NOT NULL DEFAULT now());

-- content-addressed programs. canonical serialization is ternary text
-- (reference/tree-calculus conventions); hash = sha256(ternary)
CREATE TABLE IF NOT EXISTS programs (
  hash text PRIMARY KEY,                -- sha256 hex of canon(canonical serialization)
  ternary text NOT NULL,                -- canonical serialization
  ir jsonb NULL,                        -- compiled-from IR + provenance map (call-sites.provenance), null for raw trees
  created_by uuid NULL REFERENCES identities(id),
  created_at timestamptz NOT NULL DEFAULT now());

-- runs (the run row; v0 spec section 2/4.2)
CREATE TABLE IF NOT EXISTS runs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  program_hash text NOT NULL REFERENCES programs(hash),
  input_hashes text[] NOT NULL DEFAULT '{}',   -- hashes of input trees
  fuel bigint NOT NULL,
  size_cap bigint NOT NULL,
  result_hash text NULL,                -- null while running/failed
  result_ternary text NULL,
  step_count bigint NULL,
  status text NOT NULL,                 -- running|normal|fuel_exhausted|size_exhausted|error
  caller uuid NULL REFERENCES identities(id),
  parent_run_id uuid NULL REFERENCES runs(id),   -- live replays / REPL transcripts link back
  verify_status text NULL,              -- verified|failed|gced|null (unverified yet)
  verified_at timestamptz NULL,
  journal_gced_at timestamptz NULL,     -- retention tombstone (journal.retention-gc)
  journal_gced_policy text NULL,
  created_at timestamptz NOT NULL DEFAULT now());
CREATE INDEX IF NOT EXISTS runs_program_idx ON runs(program_hash);
CREATE INDEX IF NOT EXISTS runs_caller_idx ON runs(caller);

-- capability grants (grants.grant-token)
CREATE TABLE IF NOT EXISTS grants (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  prim text NOT NULL,
  args_attenuation jsonb NOT NULL DEFAULT '{}',
  caller uuid NOT NULL REFERENCES identities(id),
  minted_by uuid NULL REFERENCES grants(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL);

-- the effect journal (journal.row-schema). append-only by discipline.
CREATE TABLE IF NOT EXISTS journals (
  run_id uuid NOT NULL REFERENCES runs(id),
  seq bigint NOT NULL,
  callsite_path text NOT NULL DEFAULT '/0',      -- canonical path in program tree
  prim text NOT NULL,
  prim_contract text NOT NULL,                   -- contract version pin (replay.prim-versioning)
  grant_id uuid NULL REFERENCES grants(id),
  args_ternary text NULL,                        -- small payloads inline
  args_hash text NULL,
  result_ternary text NULL,
  result_hash text NULL,
  error text NULL,
  wall_ms int NULL,
  host_build text NOT NULL,
  prev_hash text NOT NULL,                       -- hash chain within run
  row_hash text NOT NULL,                        -- sha256 over canonical row encoding
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, seq));

-- derived journals from counterfactual edits (journal.counterfactual-edits)
CREATE TABLE IF NOT EXISTS derived_journals (
  run_id uuid PRIMARY KEY REFERENCES runs(id),
  parent_run_id uuid NOT NULL REFERENCES runs(id),
  created_at timestamptz NOT NULL DEFAULT now());

INSERT INTO schema_migrations (name) VALUES ('0001-init.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
