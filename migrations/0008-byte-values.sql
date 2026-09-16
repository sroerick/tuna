-- 0008: the BYTE kind of the value store (M11, borg/byte-values.borg).
-- byte_values stores raw bytes (HTML, JSON, templates, media) addressed
-- by sha256 over the EXACT stored bytes, lowercase hex - the same
-- address law as tree_values (0006), a different payload kind.  ONE
-- NAMESPACE, TWO KINDS: readers probe both stores by hash, and where
-- the distinction matters (value/get, route templates) the caller
-- states the expected kind.  Kinds do not coalesce: content that is
-- simultaneously valid ternary and wanted as bytes lives in both
-- tables under the same hash.  No kind column on tree_paths - the path
-- index stores hashes only and stays kind-blind.  Dedup by hash like
-- tree_values.

BEGIN;

CREATE TABLE IF NOT EXISTS byte_values (
  hash text PRIMARY KEY,        -- sha256 hex of the exact bytes, lowercase
  bytes bytea NOT NULL);

INSERT INTO schema_migrations (name) VALUES ('0008-byte-values.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
