-- 0010: run traces (borg/trace.borg): opt-in per-firing observability.
-- A trace is a capped event log over a run's counted firings plus a
-- summary row.  It never affects step counts, fuel, or evaluation
-- order (the counting law stays AGENTS.md rule 4 verbatim); the
-- summary row's presence marks a traced run.  Events carry 16-hex
-- prefixes of the SAME structural content digests the v1 memo law
-- keys, so a trace reads against tree_values and replay.  Bulk data:
-- GC-able under the journals' retention policy when one exists.

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
  created_at timestamptz NOT NULL DEFAULT now());

CREATE TABLE IF NOT EXISTS trace_events (
  run_id uuid NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
  seq int NOT NULL,
  kind text NOT NULL,
  rule text NOT NULL DEFAULT '',
  fun_prefix text NOT NULL DEFAULT '',
  arg_prefix text NOT NULL DEFAULT '',
  note text NOT NULL DEFAULT '',
  PRIMARY KEY (run_id, seq));

INSERT INTO schema_migrations (name) VALUES ('0010-run-traces.sql')
  ON CONFLICT (name) DO NOTHING;
