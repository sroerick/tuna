-- 0004: the REPL (M9).  repl_dict is the per-identity name->tree
-- dictionary: a define compiles its term (with the dictionary already
-- substituted — compile IS reduction) and stores the resulting
-- canonical tree under (identity, name).  repl_state is the per-
-- identity REPL session pointer: journaled REPL rounds chain their run
-- rows via runs.parent_run_id, so each session is a walkable parent
-- chain rooted at the identity's first round.

BEGIN;

CREATE TABLE IF NOT EXISTS repl_dict (
  identity_id uuid NOT NULL REFERENCES identities(id),
  name text NOT NULL,
  ternary text NOT NULL,          -- canonical ternary of the defined tree
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (identity_id, name));

CREATE TABLE IF NOT EXISTS repl_state (
  identity_id uuid PRIMARY KEY REFERENCES identities(id),
  last_run_id uuid NOT NULL REFERENCES runs(id),
  updated_at timestamptz NOT NULL DEFAULT now());

INSERT INTO schema_migrations (name) VALUES ('0004-repl-dict.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
