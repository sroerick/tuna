-- 0018: OIDC sign-in ("Sign in with ChatGPT"): the browser tier's
-- agentless path.  A human signs in with their ChatGPT account and tuna
-- provisions the identity itself — no agent-minted credential in the
-- middle (the /api/signup flow is bypassed entirely).
--
--   * oidc_logins — one-shot flow state.  Only sha256(state) is stored,
--     with the PKCE verifier + nonce it guards; a consume is an UPDATE
--     that fires only for a live, unused row, so replay and expiry give
--     the same "no such login" answer.
--   * oidc_identities — provider subject -> identity link.  The ONE
--     credential store is unchanged: identities keep their bearer hash
--     (the agent tier); OIDC humans simply never use theirs.
--
-- Self-guarding like 0001/0013: inserts itself into schema_migrations
-- in the same transaction as the DDL.

BEGIN;

CREATE TABLE IF NOT EXISTS oidc_logins (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  state_hash text NOT NULL,              -- sha256 hex of the opaque state token
  nonce text NOT NULL,
  code_verifier text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL,
  used_at timestamptz NULL);

CREATE INDEX IF NOT EXISTS oidc_logins_state_idx ON oidc_logins (state_hash);
CREATE INDEX IF NOT EXISTS oidc_logins_expiry_idx ON oidc_logins (expires_at);

CREATE TABLE IF NOT EXISTS oidc_identities (
  provider text NOT NULL,                -- 'chatgpt'
  subject text NOT NULL,                 -- the provider's stable sub claim
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  linked_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (provider, subject));

INSERT INTO schema_migrations (name) VALUES ('0018-chatgpt-oidc.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;