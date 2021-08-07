CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Basic users table.
--
-- user_id is immutable which allows email to change. password is blank for
-- accounts backed by MEEI's AD; for these accounts, samaccountname will have a
-- value (the AD "username").
CREATE TABLE IF NOT EXISTS nxo_users (
  user_id        UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  email          VARCHAR(128) NOT NULL UNIQUE,
  password       VARCHAR(64) NOT NULL,
  phone          VARCHAR(32) NULL,
  first_name     VARCHAR(64) NULL,
  last_name      VARCHAR(64) NULL,
  description    VARCHAR(256) NULL,
  samaccountname VARCHAR(64) NULL,
  active         BOOLEAN DEFAULT true
  );



-- Basic groups table.
--
-- Groups are used to authorize users (and could also be considered "roles").
CREATE TABLE IF NOT EXISTS nxo_groups (
  group_id    UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  group_name  VARCHAR(128) NOT NULL UNIQUE,
  group_label VARCHAR(64) NOT NULL,
  description VARCHAR(256) NULL,
  global_only BOOLEAN DEFAULT false
  );

-- Organization table.
--
-- Information about organizations/affilliations a user might belong to.
-- Uploaded data and authz functios will use organizations to mark data
-- appropriately.
CREATE TABLE IF NOT EXISTS nxo_orgs (
org_id      UUID DEFAULT gen_random_uuid() PRIMARY KEY,
org_name    VARCHAR(128) NOT NULL,
org_abbrv   VARCHAR(48) NOT NULL UNIQUE,
description VARCHAR(256) NULL
);

CREATE INDEX IF NOT EXISTS nxo_orgs_lower_idx
  ON nxo_orgs ((LOWER(org_abbrv)));


-- There's a "global" org we use as a default for things like
-- authorizations; here's an easy way to return its UUID.
INSERT INTO nxo_orgs(org_name, org_abbrv, description)
  VALUES ('Global Default', 'global', 'Default Organization')
  ON CONFLICT DO nothing;


-- User/Group association table.
CREATE TABLE IF NOT EXISTS nxo_user_groups (
  user_id  UUID NOT NULL REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  group_id UUID NOT NULL REFERENCES nxo_groups(group_id) ON DELETE CASCADE,
  org_id   UUID NULL REFERENCES nxo_orgs(org_id) ON DELETE CASCADE,
  CONSTRAINT usergrp_constr UNIQUE (user_id, group_id, org_id)
  );

-- User/Organization association table.
--
-- is_primary notes the primary (or default) group for a user. is_contact
-- indicates that the user should be listed on the organization's contact
-- details page.
CREATE TABLE IF NOT EXISTS nxo_user_orgs (
  user_id    UUID NOT NULL REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  org_id     UUID NOT NULL REFERENCES nxo_orgs(org_id) ON DELETE CASCADE,
  is_primary BOOLEAN DEFAULT true,
  is_contact BOOLEAN DEFAULT false,
  qualifier  VARCHAR(256) NULL,
  title      VARCHAR(128) NULL,
  PRIMARY KEY(user_id, org_id)
);

-- Organization contact (address, name) table.
CREATE TABLE IF NOT EXISTS nxo_org_contact (
  org_id       UUID PRIMARY KEY REFERENCES nxo_orgs(org_id) ON DELETE CASCADE,
  address_1    VARCHAR(256) NULL,
  address_2    VARCHAR(256) NULL,
  address_3    VARCHAR(256) NULL,
  city         VARCHAR(128) NULL,
  state        VARCHAR(128) NULL,
  country      VARCHAR(128) NULL,
  postcode     VARCHAR(16) NULL
);

-- Phone numbers (orgs, users, &c).
CREATE TABLE IF NOT EXISTS nxo_phones (
  phone_id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  org_id   UUID NULL REFERENCES nxo_orgs(org_id) ON DELETE CASCADE,
  user_id  UUID NULL REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  label    VARCHAR(128) NOT NULL,
  phone    VARCHAR(128) NOT NULL,
  CONSTRAINT org_constr  UNIQUE (org_id, label),
  CONSTRAINT user_constr UNIQUE (user_id, label)
);

-- authz groups
CREATE TABLE IF NOT EXISTS nxo_realms (
  realm_id    UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  realm       VARCHAR(128) NOT NULL UNIQUE,
  description VARCHAR(255) NOT NULL,
  required    BOOLEAN NOT NULL DEFAULT false,
  group_ids   UUID[] NOT NULL DEFAULT ARRAY[]::UUID[]
);
