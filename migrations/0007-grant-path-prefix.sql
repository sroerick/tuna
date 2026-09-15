-- 0007: path_prefix on grants (M10 garden shape).  NULL = matches
-- everything (all v1 rows keep admitting exactly what they admitted);
-- a non-null prefix narrows the grant to tree paths starting with it
-- (simple string prefix, checked live at every prim call together with
-- the existing exists/unrevoked/caller checks).

BEGIN;

ALTER TABLE grants ADD COLUMN IF NOT EXISTS path_prefix text;

INSERT INTO schema_migrations (name) VALUES ('0007-grant-path-prefix.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
