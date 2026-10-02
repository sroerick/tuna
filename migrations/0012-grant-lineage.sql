-- 0012: grant lineage for delegation-attenuation (borg/grants.borg
-- §delegation-attenuation).  grants.minted_by already records the
-- MINTING IDENTITY (migration 0003 repointed its FK at identities —
-- the API has always attributed mints that way).  Attenuation needs
-- grant-to-grant lineage too: parent_grant is the grant this row was
-- derived FROM.  The pair answers both audit questions:
--   minted_by    -> "which identity minted this"
--   parent_grant -> "which capability was narrowed to make this"
-- Live revocation walks parent_grant upward: revoking a grant kills
-- its whole descendant subtree's future use (attenuation must not
-- escape revocation).  History is untouched (journals answer replay).

BEGIN;

ALTER TABLE grants ADD COLUMN IF NOT EXISTS parent_grant uuid NULL REFERENCES grants(id);

CREATE INDEX IF NOT EXISTS grants_parent_grant_idx ON grants(parent_grant);

INSERT INTO schema_migrations (name) VALUES ('0012-grant-lineage.sql')
  ON CONFLICT (name) DO NOTHING;

COMMIT;
