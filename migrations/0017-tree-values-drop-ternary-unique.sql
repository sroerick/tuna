-- 0017: drop the UNIQUE btree on tree_values.ternary (tree_values_ternary_key).
-- The index stores the COMPRESSED value and PG btree caps index rows at
-- ~2704B (1/3 page): any mixed-text value past ~1030 chars failed its insert
-- with PG 54000 (index row size exceeds btree version 4 maximum 2704 for
-- index tree_values_ternary_key) -> /api/tree/put 500. Root-caused 10-21 from
-- the tuna.log traceback (registry put).
-- Dedup is already carried by hash (sha256 of the ternary) as PRIMARY KEY,
-- and the only insert path is ON CONFLICT (hash) DO NOTHING, so the unique
-- constraint on the raw value is redundant belt-and-suspenders that now
-- actively breaks large claims. After this lands, value equality is
-- guaranteed by the hash, not by a btree over the blob.
BEGIN;

ALTER TABLE tree_values DROP CONSTRAINT IF EXISTS tree_values_ternary_key;

INSERT INTO schema_migrations (name) VALUES ('0017-tree-values-drop-ternary-unique.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
