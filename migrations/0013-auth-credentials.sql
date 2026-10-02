-- 0013: accounts (the pp-slice): password credentials + opaque
-- browser sessions for EXISTING identities.  Bearer-token auth
-- (identities.token_hash, 0001) is unchanged and remains the agent
-- tier; this adds the browser tier:
--   * credentials — kind-tagged second factors per identity.  v1 carries
--     kind='password' rows with INTERIM hashing, honestly recorded:
--     secret_hash = 'sha256$<salt_hex>$<digest_hex>' where digest =
--     sha256(salt_hex ^ ":" ^ password) — pragma, the same interim
--     format and digest recipe pricklypear shipped before its argon2id
--     upgrade transparent-rehashes.  argon2id is named as planned and
--     MAY land the same way (verify legacy + rewrite on success), which
--     is why the format is pinned format-text-compatible.
--   * auth_sessions — opaque session tokens, only sha256 stored; a
--     login mints one, the cookie carries it, logout revokes the row.
--   * auth_log — login/verify attempts (PP 002 auth_log shape) so a
--     later throttle/forensics pass has data instead of guesses.
-- Self-guarding like 0001: inserts itself into schema_migrations in
-- the same transaction as the DDL.

BEGIN;

CREATE TABLE IF NOT EXISTS credentials (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  kind text NOT NULL,                    -- 'password' (interim sha256$salt$digest)
  secret_hash text NOT NULL,             -- format-pinned interim hash string
  created_at timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz NULL,
  UNIQUE (identity_id, kind));

CREATE INDEX IF NOT EXISTS credentials_identity_idx ON credentials (identity_id);

CREATE TABLE IF NOT EXISTS auth_sessions (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),   -- the token's opaque row id
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  token_hash text NOT NULL,              -- sha256 hex of the opaque session token
  expires_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL);

CREATE INDEX IF NOT EXISTS auth_sessions_token_idx ON auth_sessions (token_hash);
CREATE INDEX IF NOT EXISTS auth_sessions_identity_idx ON auth_sessions (identity_id);

CREATE TABLE IF NOT EXISTS auth_log (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NULL REFERENCES identities(id),  -- NULL when unknown/failed
  kind text NOT NULL,                    -- 'password' | 'session' | 'bearer'
  success boolean NOT NULL,
  happened_at timestamptz NOT NULL DEFAULT now());

CREATE INDEX IF NOT EXISTS auth_log_identity_idx ON auth_log (identity_id);
CREATE INDEX IF NOT EXISTS auth_log_kind_idx ON auth_log (kind);

INSERT INTO schema_migrations (name) VALUES ('0013-auth-credentials.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
