-- 0001-kernel.sql: Lean Tuna evaluator kernel tables.
-- State/product data (values, defs, messages, cron, retention policies,
-- routes, claims, /todo board, tree_compat, op_audit) lives in Midden
-- (midden_values, midden_commits, midden_heads, midden_projections,
-- midden_cursors, and schema default views), bootstrapped by Midden_store.
--
-- Kernel tables stay flat SQL so high-rate evaluator runs/journals never
-- contend on Midden's single-writer advisory lock, and gc_journal /
-- redact_journal_row / deriv-check keep their exact semantics.

BEGIN;

CREATE TABLE IF NOT EXISTS schema_migrations (
  name text PRIMARY KEY,
  applied_at timestamptz NOT NULL DEFAULT now()
);

-- Identities & bearer auth
CREATE TABLE IF NOT EXISTS identities (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  name text UNIQUE NOT NULL,
  token_hash text NOT NULL,
  is_admin bool NOT NULL DEFAULT false,
  created_at timestamptz NOT NULL DEFAULT now()
);

-- Content-addressed programs (Base-243 Tpack bytes + IR/provenance/source)
CREATE TABLE IF NOT EXISTS programs (
  hash text PRIMARY KEY,
  bytes bytea NOT NULL,
  ir jsonb NULL,
  source text NULL,
  created_by uuid NULL REFERENCES identities(id),
  created_at timestamptz NOT NULL DEFAULT now()
);

-- Evaluator runs (Base-243 result_bytes, sharing/demand/denial telemetry, GC tombstone)
CREATE TABLE IF NOT EXISTS runs (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  program_hash text NOT NULL REFERENCES programs(hash),
  input_hashes text[] NOT NULL DEFAULT '{}',
  fuel bigint NOT NULL,
  size_cap bigint NOT NULL,
  result_hash text NULL,
  result_bytes bytea NULL,
  step_count bigint NULL,
  status text NOT NULL,
  caller uuid NULL REFERENCES identities(id),
  parent_run_id uuid NULL REFERENCES runs(id),
  verify_status text NULL,
  verified_at timestamptz NULL,
  journal_gced_at timestamptz NULL,
  journal_gced_policy text NULL,
  semantics text NOT NULL DEFAULT 'v0',
  demand_sharing boolean NOT NULL DEFAULT false,
  demand_hits bigint NOT NULL DEFAULT 0,
  denial_count bigint NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS runs_program_idx ON runs(program_hash);
CREATE INDEX IF NOT EXISTS runs_caller_idx ON runs(caller);

-- Capability grants (with structural Midden scope columns + lineage)
CREATE TABLE IF NOT EXISTS grants (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  prim text NOT NULL,
  args_attenuation jsonb NOT NULL DEFAULT '{}',
  path_prefix text NULL,
  scope_schema text NULL,
  scope_namespace text NULL,
  scope_projection text NULL,
  caller uuid NOT NULL REFERENCES identities(id),
  minted_by uuid NULL REFERENCES identities(id),
  parent_grant uuid NULL REFERENCES grants(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL
);

-- Hash-chained run effect journal
CREATE TABLE IF NOT EXISTS journals (
  run_id uuid NOT NULL REFERENCES runs(id),
  seq bigint NOT NULL,
  callsite_path text NOT NULL DEFAULT '/0',
  prim text NOT NULL,
  prim_contract text NOT NULL,
  grant_id uuid NULL REFERENCES grants(id),
  args_ternary text NULL,
  args_hash text NULL,
  result_ternary text NULL,
  result_hash text NULL,
  error text NULL,
  wall_ms int NULL,
  host_build text NOT NULL,
  prev_hash text NOT NULL,
  row_hash text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, seq)
);

-- Counterfactual fork lineage
CREATE TABLE IF NOT EXISTS derived_journals (
  run_id uuid PRIMARY KEY REFERENCES runs(id),
  parent_run_id uuid NOT NULL REFERENCES runs(id),
  created_at timestamptz NOT NULL DEFAULT now()
);

-- Cross-run clean-firing demand memo (Base-243 result_bytes)
CREATE TABLE IF NOT EXISTS demand_memo (
  caller text NOT NULL,
  fun_hash text NOT NULL,
  arg_hash text NOT NULL,
  result_hash text NOT NULL,
  result_bytes bytea NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  last_seen_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (caller, fun_hash, arg_hash)
);

CREATE INDEX IF NOT EXISTS demand_memo_caller_idx ON demand_memo(caller);

-- Opt-in firing traces
CREATE TABLE IF NOT EXISTS run_traces (
  run_id uuid PRIMARY KEY REFERENCES runs(id) ON DELETE CASCADE,
  semantics text NOT NULL,
  raw_firings bigint NOT NULL DEFAULT 0,
  charged bigint NOT NULL DEFAULT 0,
  memo_hits bigint NOT NULL DEFAULT 0,
  dirty_firings bigint NOT NULL DEFAULT 0,
  loop_detected boolean NOT NULL DEFAULT false,
  recorded int NOT NULL DEFAULT 0,
  truncated boolean NOT NULL DEFAULT false,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS trace_events (
  run_id uuid NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
  seq int NOT NULL,
  kind text NOT NULL,
  rule text NOT NULL DEFAULT '',
  fun_prefix text NOT NULL DEFAULT '',
  arg_prefix text NOT NULL DEFAULT '',
  note text NOT NULL DEFAULT '',
  PRIMARY KEY (run_id, seq)
);

-- Browser accounts, sessions, runner tokens, OIDC
CREATE TABLE IF NOT EXISTS credentials (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  kind text NOT NULL,
  secret_hash text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz NULL,
  UNIQUE (identity_id, kind)
);

CREATE INDEX IF NOT EXISTS credentials_identity_idx ON credentials(identity_id);

CREATE TABLE IF NOT EXISTS auth_sessions (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  token_hash text NOT NULL,
  expires_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz NULL
);

CREATE INDEX IF NOT EXISTS auth_sessions_token_idx ON auth_sessions(token_hash);
CREATE INDEX IF NOT EXISTS auth_sessions_identity_idx ON auth_sessions(identity_id);

CREATE TABLE IF NOT EXISTS auth_log (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NULL REFERENCES identities(id),
  kind text NOT NULL,
  success boolean NOT NULL,
  happened_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS auth_log_identity_idx ON auth_log(identity_id);
CREATE INDEX IF NOT EXISTS auth_log_kind_idx ON auth_log(kind);

CREATE TABLE IF NOT EXISTS runner_tokens (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  name text NOT NULL,
  token_hash text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  last_used_at timestamptz NULL,
  revoked_at timestamptz NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS runner_tokens_hash_idx ON runner_tokens(token_hash);
CREATE INDEX IF NOT EXISTS runner_tokens_identity_idx ON runner_tokens(identity_id);

CREATE TABLE IF NOT EXISTS oidc_logins (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  state_hash text NOT NULL,
  nonce text NOT NULL,
  code_verifier text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  expires_at timestamptz NOT NULL,
  used_at timestamptz NULL
);

CREATE INDEX IF NOT EXISTS oidc_logins_state_idx ON oidc_logins(state_hash);
CREATE INDEX IF NOT EXISTS oidc_logins_expiry_idx ON oidc_logins(expires_at);

CREATE TABLE IF NOT EXISTS oidc_identities (
  provider text NOT NULL,
  subject text NOT NULL,
  identity_id uuid NOT NULL REFERENCES identities(id) ON DELETE CASCADE,
  linked_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (provider, subject)
);

-- Legacy store/get + store/put prim KV (kept for verify-4 / verify-11 replay tests)
CREATE TABLE IF NOT EXISTS prim_kv (
  key_hash text PRIMARY KEY,
  key_ternary text NOT NULL,
  value_ternary text NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now()
);

INSERT INTO schema_migrations (name) VALUES ('0001-kernel.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
