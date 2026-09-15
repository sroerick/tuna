-- 0006: the content-addressed VALUE side of the M10 substrate.  Values
-- are immutable ternary blobs addressed by their own sha256 (lowercase
-- hex); tree_paths (0005) references them by hash only.  Dedup by hash,
-- no path columns here -- the storage-substrate law (tuna.borg watch
-- note): content-addressed VALUES are the primitive, and the derived
-- path index must never leak into the value store.

BEGIN;

CREATE TABLE IF NOT EXISTS tree_values (
  hash text PRIMARY KEY,        -- sha256 hex of the ternary
  ternary text NOT NULL UNIQUE);

INSERT INTO schema_migrations (name) VALUES ('0006-values.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
