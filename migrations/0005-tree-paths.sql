-- 0005: the M10 tree substrate.  tree_paths is a DERIVED index: a
-- mutable path -> (value_hash, version) map over content-addressed
-- values -- never storage truth (the values-not-hierarchies law,
-- tuna.borg watch note: no parent pointers, no hierarchy columns).
-- tree_ops is the append-only effect log over the index, sha256-chained
-- by op_hash: rewind is a pure fold of this log.

BEGIN;

CREATE TABLE IF NOT EXISTS tree_paths (
  path text NOT NULL,
  path_hash text PRIMARY KEY,   -- sha256 hex of the path string, lowercase
  value_hash text NOT NULL,     -- sha256 hex of the value ternary (tree_values)
  version bigint NOT NULL DEFAULT 1,
  owner text NOT NULL,          -- identity id of the last writer
  updated_at timestamptz NOT NULL DEFAULT now());
CREATE INDEX IF NOT EXISTS tree_paths_owner_idx ON tree_paths (owner);
CREATE INDEX IF NOT EXISTS tree_paths_path_idx ON tree_paths (path);

-- one row per substrate op (get|put|cas|list|fork|denials).  value_hash
-- is NULL for get/list reads and for writes that did not take (denials,
-- CAS conflicts): only rows with a value_hash and version carried an
-- effect.  op_hash chains over the previous row's op_hash, genesis =
-- 64*'0' (like the run journal chain).
CREATE TABLE IF NOT EXISTS tree_ops (
  seq bigserial PRIMARY KEY,
  path text NOT NULL,
  op text NOT NULL,             -- get|put|cas|list|fork
  value_hash text NULL,         -- NULL for get/list and non-effects
  prev_version bigint NULL,
  version bigint NULL,
  actor text NOT NULL,          -- identity id performing the op
  ts timestamptz NOT NULL DEFAULT now(),
  op_hash text NOT NULL);       -- sha256(prev_op_hash || row-concat)

INSERT INTO schema_migrations (name) VALUES ('0005-tree-paths.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
