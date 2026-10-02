-- 0011: the demand-memo (borg/sharing.borg §demand-memo).  A CROSS-RUN
-- cache of CLEAN firings only: a firing is the (fun-tree, arg-tree) pair
-- keyed by content digest; the first demander computes it, later
-- demanders in OTHER runs reuse the recorded answer.  The dirty rule
-- hardens into a boundary law here: a firing whose evaluation answered
-- a prim NEVER enters this table, so grant liveness checks and journal
-- audit rows are untouched by construction -- the shared table is
-- pure-cache only.
--
-- Scoping: own-garden.  Rows are keyed by caller, so a run only ever
-- consumes answers its own identity produced (the narrowest of the
-- trust options the chapter lists; widening it is an owner call).
--
-- runs gains two demand columns: demand_sharing (was the shared table
-- consulted for this run) and demand_hits (how many of its firings were
-- answered from the shared table, not computed).  A run with hits > 0
-- has its STEP COUNT treated as environment-dependent at verification
-- time (the shared table is a host, like the wall clock); its RESULT
-- identity is still checked in full, because clean firings are pure and
-- a same-garden answer is deterministic.

BEGIN;

CREATE TABLE IF NOT EXISTS demand_memo (
  caller text NOT NULL,           -- garden: the identity whose run produced it
  fun_hash text NOT NULL,         -- sha256 hex of the function tree
  arg_hash text NOT NULL,         -- sha256 hex of the argument tree
  result_hash text NOT NULL,      -- sha256 hex of the answer tree
  result_ternary text NOT NULL,   -- the answer (canonical ternary)
  created_at timestamptz NOT NULL DEFAULT now(),
  last_seen_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (caller, fun_hash, arg_hash));

CREATE INDEX IF NOT EXISTS demand_memo_caller_idx ON demand_memo (caller);

ALTER TABLE runs
  ADD COLUMN IF NOT EXISTS demand_sharing boolean NOT NULL DEFAULT false;
ALTER TABLE runs
  ADD COLUMN IF NOT EXISTS demand_hits bigint NOT NULL DEFAULT 0;

INSERT INTO schema_migrations (name) VALUES ('0011-demand-memo.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
