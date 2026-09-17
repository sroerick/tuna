-- 0009: the SHARING semantics version of the run row (borg/sharing.borg).
-- runs.semantics names the accounting law that produced the row's
-- numbers: 'v0' is the canonical engine (one step per triage firing;
-- every row that ever existed before this migration), 'v1' is the
-- distinct-work law (memoized firings keyed by content digest,
-- firings that answered a prim never memoize, in-flight re-entry is
-- divergence and records status 'loop').  Replay reads the row and
-- re-executes under its OWN law: verification is a per-version
-- identity, never a cross-engine claim (FINDINGS.md F1).  The default
-- backfills every existing row without a rewrite.

ALTER TABLE runs
  ADD COLUMN IF NOT EXISTS semantics text NOT NULL DEFAULT 'v0';

INSERT INTO schema_migrations (name) VALUES ('0009-sharing-semantics.sql')
  ON CONFLICT (name) DO NOTHING;
