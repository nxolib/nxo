CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS nxo_users (
  user_id        UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  email          VARCHAR(128) NOT NULL UNIQUE,
  password       VARCHAR(64)  NOT NULL,
  phone          VARCHAR(32)  NOT NULL DEFAULT '',
  first_name     VARCHAR(64)  NOT NULL DEFAULT '',
  last_name      VARCHAR(64)  NOT NULL DEFAULT '',
  description    VARCHAR(256) NOT NULL DEFAULT '',
  samaccountname VARCHAR(64)  NOT NULL DEFAULT '',
  active         BOOLEAN NOT NULL DEFAULT true,
  roles          TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[]
  );

CREATE TABLE IF NOT EXISTS nxo_groups (
  group_name  VARCHAR(128) NOT NULL PRIMARY KEY,
  group_label VARCHAR(64)  NOT NULL UNIQUE,
  description VARCHAR(256) NOT NULL DEFAULT '',
  global_only BOOLEAN DEFAULT false
  );

CREATE TABLE IF NOT EXISTS nxo_orgs (
  org_abbrv   VARCHAR(48)  NOT NULL PRIMARY KEY,
  org_name    VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(256) NOT NULL DEFAULT ''
  );

CREATE TABLE IF NOT EXISTS nxo_org_contact (
  org_abbrv    VARCHAR(48)  PRIMARY KEY REFERENCES nxo_orgs ON DELETE CASCADE,
  address_1    VARCHAR(256) NOT NULL DEFAULT '',
  address_2    VARCHAR(256) NOT NULL DEFAULT '',
  address_3    VARCHAR(256) NOT NULL DEFAULT '',
  city         VARCHAR(128) NOT NULL DEFAULT '',
  state        VARCHAR(128) NOT NULL DEFAULT '',
  country      VARCHAR(128) NOT NULL DEFAULT '',
  postcode     VARCHAR(16)  NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS nxo_org_phone (
  org_abbrv    VARCHAR(48)  NOT NULL REFERENCES nxo_orgs ON DELETE CASCADE,
  label        VARCHAR(128) NOT NULL,
  phone        VARCHAR(128) NOT NULL,
  CONSTRAINT nxo_org_phone_label UNIQUE(org_abbrv, label)
);

CREATE TABLE IF NOT EXISTS nxo_user_orgs (
  user_id    UUID NOT NULL REFERENCES nxo_users ON DELETE CASCADE,
  org_abbrv  VARCHAR(48) NOT NULL REFERENCES nxo_orgs ON DELETE CASCADE,
  is_primary BOOLEAN NOT NULL DEFAULT true,
  is_contact BOOLEAN NOT NULL DEFAULT false,
  qualifier  VARCHAR(256) NOT NULL DEFAULT '', -- what on earth is this?
  title      VARCHAR(128) NOT NULL DEFAULT '',
  PRIMARY KEY(user_id, org_abbrv)
);

CREATE TABLE IF NOT EXISTS nxo_user_phone (
  user_id   UUID NOT NULL REFERENCES nxo_users ON DELETE CASCADE,
  org_abbrv VARCHAR(48) NOT NULL REFERENCES nxo_orgs ON DELETE CASCADE,
  label     VARCHAR(128) NOT NULL,
  phone     VARCHAR(128) NOT NULL,
  CONSTRAINT nxo_user_phone_label UNIQUE(user_id, org_abbrv, label)
);

CREATE TABLE IF NOT EXISTS nxo_realms (
  realm  VARCHAR(128) NOT NULL PRIMARY KEY,
  groups TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[]
);

CREATE TABLE IF NOT EXISTS nxo_api_keys (
  user_id UUID PRIMARY KEY REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  api_key UUID NOT NULL
);


-- Sufficient groups, orgs, realms, and a seed user to boot the NXO
-- framework.  Note that the groups and realms defined here should not
-- be deleted.  The seed user and test organizations can be.
--
-- Note also that new groups may be added, perhaps during the runtime.
-- This allows for dynamic authz (especially when there are multiple
-- organizations).  New realms may not be added during the runtime
-- (these are used in the code itself) but one supposes that the
-- definitions of the existing realms *could* be changed.  No UI is
-- provided to enact these changes.

INSERT INTO nxo_groups(group_name, group_label, description, global_only)
  VALUES
    ('users',          'Site User',             'Default User Group', false),
    ('administrators', 'Global Administrators', 'All Access',         true),
    ('usermgmt',       'User Managers',         'User Managers',      false),
    ('datamgmt',       'Data Managers',         'Data Managers',      false),
    ('passwd',         'Stale Password',        'Requres PW Change',  true),
    ('api',            'API Users',             'API Access',         false),
    ('pending',        'Pending Users',         'Pending Users',      true)
  ON CONFLICT DO NOTHING;

INSERT INTO nxo_orgs(org_name, org_abbrv, description)
  VALUES
    ('Global Default', 'global', 'Default Organization'),
    ('Ace Corp.',      'ace',    'Ace Test Organization'),
    ('Acme Co.',       'acme',   'Acme Test Organization')
  ON CONFLICT DO NOTHING;

INSERT INTO nxo_realms (realm, groups)
  VALUES
  ('global::admin_something',
    '{ "global::administrators", "global::usermgmt", "global::datamgmt" }'),
  ('global::admin_everything',
    '{ "global::administrators" }'),
  ('global::admin_users',
    '{ "global::administrators", "global::usermgmt" }'),
  ('global::admin_data',
    '{ "global::administrators", "global::datamgmt" }'),
  ('global::api',
    '{ "global::administrators", "global::api" }')
 ON CONFLICT DO NOTHING;


INSERT INTO nxo_users (email, password, phone, first_name, last_name,
                       description, active, roles)
  VALUES
    ('quux@example.com',
     '$2a$12$SRBNjPnX167CP/J9t7.S/ep.XZMmfmFTuRKGBOmIRpcbVmWN0vV5O',
     '504-555-1212', 'Seed', 'User', 'Seed Account', true,
     '{ "global::administrators", "global::usermgmt", "global::users" }' )
  ON CONFLICT DO NOTHING;
