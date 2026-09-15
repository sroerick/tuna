-- 0003: fix the grants.minted_by foreign key. 0001 pointed it at
-- grants(id) (unusable: a grant would have to reference a parent
-- grant); the API mints grants with the CALLING IDENTITY's id, so the
-- constraint must reference identities(id).  Fresh clusters get the
-- corrected DDL in 0001; this migration repairs clusters where 0001
-- was already applied with the wrong constraint.

BEGIN;

ALTER TABLE grants DROP CONSTRAINT IF EXISTS grants_minted_by_fkey;

ALTER TABLE grants
  ADD CONSTRAINT grants_minted_by_fkey
  FOREIGN KEY (minted_by) REFERENCES identities(id);

INSERT INTO schema_migrations (name) VALUES ('0003-fix-grants-fk.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
