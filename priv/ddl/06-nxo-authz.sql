-- Authorization table.
--
-- Given a column (e.g., "age") specify:
--
--   - UI Label (label)
--   - a user appropriate description (description)
--   - a default value (default_value) shown when col is null
--   - a default value (unauthz_value) shown when col is unauthz
--
-- and some authorization information:
--
--   (a) if is_public is true, no further authz is required
-- 
--   (b) if require_ownership is true, the record's owner_org must
--   match the organization of the user making the request, authz_grp
--   (for the organization) applies unless it is null.
--
--   (c) if is_public is false the user must be logged in. (C)
--
--  What there IS NOT is an authz group WITHOUT organizational
--  ownership.  I can't see that being a member of HIPAARead (or
--  whatever) for MEE should allow a user access to a HIPAARead
--  specification of, say, UCLA.
--
-- The action indicates what action the user is authz'd to take.
-- Currently the specifications for the application aren't solid
-- enough to know all the choices but for now we'll assume 'read'.
--
-- Each column specification will have both a default org_id (global)
-- or the UUID of the org the rule is associated with.  In this way
-- there's a default set of authorizations and potential overrides for
-- each org.
CREATE TABLE IF NOT EXISTS nxo_field_authz (
  org_id            UUID NOT NULL
                         REFERENCES nxo_orgs(org_id) ON DELETE CASCADE,       
  field_column      VARCHAR(256) NOT NULL,
  field_table       VARCHAR(256) NOT NULL,
  action            VARCHAR(256) NOT NULL DEFAULT 'read',
  label             VARCHAR(256) NOT NULL,
  description       TEXT NULL,
  default_value     TEXT NULL,
  unauthz_value     TEXT NULL,
  is_public         BOOLEAN DEFAULT false,
  authz_grp         UUID REFERENCES nxo_groups(group_id) NULL DEFAULT NULL,
  require_ownership BOOLEAN DEFAULT false,
  UNIQUE (org_id, field_column, action)
);


-- Load up the nxo_field_authz table.  Note that this only loads
-- columns for the global org_id and is meant to set defaults.  The
-- defaults in question are:
--   action: read
--   is_public: false
--   require_ownership: false
INSERT INTO nxo_field_authz (org_id, field_column, field_table, label)
  (SELECT DISTINCT
     global_org_id() as org_id,
     column_name,
     table_name,
     column_name
   FROM information_schema.columns
   WHERE table_schema = 'public' and table_name like '{{name}}_%')
ON CONFLICT DO nothing;


-- (Note to self: this returns metadata and as such needs the owner's
-- org_id passed in.)
CREATE OR REPLACE FUNCTION field_authz(
     p_action varchar               -- some action: read, write, &c
   , p_owner_org_id UUID            -- owner of the record in question
   , p_user_id UUID DEFAULT NULL    -- the logged in user_id
)
RETURNS TABLE (
    field_column varchar
  , label varchar
  , description text
  , default_value text
  , unauthz_value text
  , authorized bool)  
AS $$
#variable_conflict use_column

  DECLARE
    p_user_uuid uuid := to_uuid(p_user_id);
  BEGIN
   RETURN QUERY WITH all_authz AS (
      SELECT *, 1 as priority
      FROM nxo_field_authz
      WHERE org_id = p_owner_org_id
            AND action = p_action      
      UNION
      SELECT *, 2 as priority
      FROM nxo_field_authz
      WHERE org_id = global_org_id() -- these are defaults
            AND action = p_action      
    
      ORDER BY priority
    )
    SELECT DISTINCT ON (field_column)
        field_column
      , label
      , description
      , default_value
      , unauthz_value
      , CASE
          -- if is_public is true, anyone can view it (A)
          WHEN is_public THEN true
          -- this is anyone logged in (the default) (C)
          WHEN not require_ownership
               and p_user_id IS NOT NULL THEN true
          -- this is someone in the organization (B)
          WHEN require_ownership
               and p_user_id IS NOT NULL
               and authz_grp IS NULL
               and is_user_org(p_user_id, p_owner_org_id) THEN true
          -- this is someone in the organization and in the org's group (B)
          WHEN require_ownership
               and p_user_id IS NOT NULL
               and is_user_group(p_user_id, authz_grp, org_id) THEN true
          -- if none of the above is true, the user lacks permission
          ELSE false
        END
    FROM all_authz
    ORDER BY field_column, priority;  
  END;
$$ LANGUAGE plpgsql;
