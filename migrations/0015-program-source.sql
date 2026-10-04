-- 0015: source retention (call-sites.provenance completion).
--
-- The provenance map (programs.ir tags) carries per-node source spans
-- {off,len}, but the source TEXT itself was discarded at upload — the
-- spans referenced offsets into a string the DB never kept, so the
-- "resolves to a source span" law only worked for corpus programs
-- whose source lives on disk.  The v0.2-defect disposition pass
-- (observability) retains the text: post_source / REPL def + eval
-- rounds store what they compiled; bare-ternary uploads and
-- patch-derived programs have no source and stay NULL, honestly.
-- Self-guarding like 0001: inserts itself into schema_migrations in
-- the same transaction as the DDL.

BEGIN;

ALTER TABLE programs ADD COLUMN IF NOT EXISTS source text;

INSERT INTO schema_migrations (name) VALUES ('0015-program-source.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
