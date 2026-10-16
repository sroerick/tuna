-- 0016: runner tokens (todo/g2-parity-identities, the glochid
-- issue-token precedent): a NAMED, individually revocable Bearer per
-- identity, so a runner holds a revocable credential, never the
-- operator's primary token (identities.token_hash stays the boot
-- credential, unchanged).  A live runner token authenticates at FULL
-- bearer parity -- the same identity row -- so grants and the tree-op
-- floor (g1) apply unchanged.  Revocation is the lifecycle: no TTL;
-- minted by admins only, listed without hashes, revoked by the admin
-- or the owning identity.
-- Self-guarding like 0001: inserts itself into schema_migrations in
-- the same transaction as the DDL.

BEGIN;

CREATE TABLE IF NOT EXISTS runner_tokens (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  name text NOT NULL,                    -- what holds it ("vultr-runner-1")
  token_hash text NOT NULL,              -- sha256 hex; raw leaves the process once
  created_at timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz NULL,
  revoked_at timestamptz NULL);

CREATE UNIQUE INDEX IF NOT EXISTS runner_tokens_hash_idx
  ON runner_tokens (token_hash);
CREATE INDEX IF NOT EXISTS runner_tokens_identity_idx
  ON runner_tokens (identity_id);

INSERT INTO schema_migrations (name) VALUES ('0016-runner-tokens.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
