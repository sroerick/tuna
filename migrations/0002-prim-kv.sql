-- 0002: key/value store for the store/get + store/put prims (M7).
-- Keys and values are whole canonical trees; the key hash is the
-- content address of the key ternary.

BEGIN;

CREATE TABLE IF NOT EXISTS prim_kv (
  key_hash text PRIMARY KEY,     -- sha256 hex of the key ternary
  key_ternary text NOT NULL,     -- canonical key tree
  value_ternary text NOT NULL,   -- canonical value tree
  updated_at timestamptz NOT NULL DEFAULT now());

INSERT INTO schema_migrations (name) VALUES ('0002-prim-kv.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
