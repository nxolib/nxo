-- User/Group/Org functions.
--

/* user_id - retrieve user_id from email, samaccountname, or uuid

  example:
    SELECT user_id('bunny@bapi.us');

  returns:
    user_id uuid

  parameters:
    p_name varchar: email, samaccountname, or uuid

  notes:
    If p_name is a uuid, it must be cast to varchar.
*/
CREATE OR REPLACE FUNCTION user_id(p_name varchar)
RETURNS UUID AS $$
  SELECT user_id
    FROM nxo_users
   WHERE email = p_name
         or samaccountname = p_name
         or user_id::TEXT = p_name
   LIMIT 1
$$ LANGUAGE SQL;


/* user_add - add a TEST user to the database

  example:
    SELECT user_add('quux@example.com')

  returns:
    user_id uuid

  parameters:
    p_email varchar: an email address (or other unique string)

  notes:
    This function creates test users with bogus passwords (i.e., the
    users created herewith are unable to actually log into the
    website).  This function is intended for ephemeral test users.
*/
CREATE OR REPLACE FUNCTION user_add(p_email varchar)
RETURNS UUID AS $$
  INSERT INTO nxo_users(email, password, description)
  VALUES (p_email, 'no-password-here', 'test account')
  ON CONFLICT DO NOTHING
  RETURNING user_id
$$ LANGUAGE SQL;


/* user_add - add a functioning user to the database

  example:
    SELECT user_add('quux@example.com', ...)

  returns:
    user_id uuid

  parameters:
    p_email varchar: an email address
    p_password varchar: an encrypted password
    p_phone varchar: a phone number
    p_first_name varchar: first name
    p_last_name varchar: last name
    p_active boolean: active/inactive flag
    p_description text (OPTIONAL): free-form description field
    p_samaccountname varchar (OPTIONAL): AD account name
    p_user_id uuid (OPTIONAL): user_id

  notes:
    Will create or (in case of user_id conflict) update an nxo_user
    entry.
*/
CREATE OR REPLACE FUNCTION user_add(p_email varchar,
                                    p_passwd varchar,
                                    p_phone varchar,
                                    p_first_name varchar,
                                    p_last_name varchar,
                                    p_active boolean,
                                    p_description varchar DEFAULT NULL,
                                    p_samaccountname varchar DEFAULT NULL,
                                    p_user_id uuid DEFAULT gen_random_uuid())
RETURNS UUID AS $$
  INSERT INTO nxo_users(user_id, email, password, phone,
                        first_name, last_name, description, samaccountname,
                        active)
  VALUES(p_user_id, p_email, p_passwd, p_phone,
         p_first_name, p_last_name, p_description, p_samaccountname,
         p_active)

  ON CONFLICT (user_id) DO UPDATE
    SET email      = p_email,
        password   = p_passwd,
        phone      = p_phone,
        first_name = p_first_name,
        last_name  = p_last_name,
        active     = p_active
    WHERE nxo_users.user_id = p_user_id

  RETURNING user_id;
$$ LANGUAGE SQL;


/* user_activate - set user's active flag to true

  example:
    SELECT user_active(user_id('bunny@bapi.us'));

  returns:
    user_id uuid

  parameters:
    p_user_id uuid: the uuid of the user to activate
*/
CREATE OR REPLACE FUNCTION user_activate(p_user UUID)
RETURNS UUID AS $$
  UPDATE nxo_users
  SET active = true
  WHERE user_id = p_user
  RETURNING user_id;
$$ LANGUAGE SQL;


/* user_deactivate - set user's active flag to false

  example:
    SELECT user_deactive(user_id('bunny@bapi.us'));

  returns:
    user_id uuid

  parameters:
    p_user_id uuid: the uuid of the user to deactivate
*/
CREATE OR REPLACE FUNCTION user_deactivate(p_user UUID)
RETURNS UUID AS $$
  UPDATE nxo_users
  SET active = false
  WHERE user_id = p_user
  RETURNING user_id;
$$ LANGUAGE SQL;


/* user_rm - remove a user from the database

  example:
    SELECT user_rm(user_id('bunny@bapi.us'));

  returns:
    user_id uuid

  parameters:
    p_user uuid: the uuid of the user to delete
*/
CREATE OR REPLACE FUNCTION user_rm(p_user UUID)
RETURNS UUID AS $$
  DELETE FROM nxo_users
  WHERE user_id = p_user
  RETURNING p_user
$$ LANGUAGE SQL;


/* group_id - given a group name, return its id

  example:
    SELECT group_id('administrators');

  returns:
     group_id uuid

  parameters:
     p_name varchar: the group name
*/

CREATE OR REPLACE FUNCTION group_id(p_name varchar) RETURNS UUID AS $$
  SELECT group_id
    FROM nxo_groups
   WHERE group_name = p_name
$$ LANGUAGE SQL;


/* group_add - create a new group

  example:
    SELECT group_add('newgrp', 'my new group', 'testing', false);

  returns:
    group_id uuid - the created group

  parameters:
    p_name varchar: group name
    p_label varchar: group label (default '')
    p_desc varchar: group description (default null)
    p_global_only boolean: global group flag (default false)
*/

CREATE OR REPLACE FUNCTION group_add(p_name varchar,
                                     p_label varchar DEFAULT '',
                                     p_desc varchar DEFAULT NULL,
                                     p_global_only boolean DEFAULT false)
RETURNS UUID AS $$
  INSERT INTO nxo_groups(group_name, group_label, description, global_only)
  VALUES (p_name, p_label, p_desc, p_global_only)
  ON CONFLICT DO NOTHING
  RETURNING group_id
$$ LANGUAGE SQL;

/* group_rm - delete a group

  example:
    SELECT group_rm(group_id('newgrp'));

  returns:
     group_id uuid - the deleted group

  parameters:
    p_group_id uuid: the group id
*/

CREATE OR REPLACE FUNCTION group_rm(p_group_id UUID) RETURNS UUID AS $$
  DELETE FROM nxo_groups WHERE group_id = p_group_id RETURNING p_group_id
$$ LANGUAGE SQL;



CREATE OR REPLACE FUNCTION org_add(p_name varchar,
                                   p_abbrv varchar,
                                   p_desc varchar DEFAULT NULL)
RETURNS UUID AS $$
  INSERT INTO nxo_orgs(org_name, org_abbrv, description)
  VALUES (p_name, p_abbrv, p_desc)
  ON CONFLICT DO NOTHING
  RETURNING org_id
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION org_rm(p_org_id UUID) RETURNS UUID AS $$
  DELETE FROM nxo_orgs WHERE org_id = p_org_id RETURNING p_org_id
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION org_id(p_abbrv varchar) RETURNS UUID AS $$
  SELECT org_id FROM nxo_orgs WHERE org_abbrv = p_abbrv
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION global_org_id() RETURNS UUID AS $$
  SELECT org_id('global')
$$ LANGUAGE SQL;


-- useful for testing
CREATE OR REPLACE FUNCTION add_user_org(p_user_id UUID, p_org_id UUID)
RETURNS UUID AS $$
  INSERT INTO nxo_user_orgs(user_id, org_id)
  VALUES(p_user_id, p_org_id)
  ON CONFLICT DO NOTHING
  RETURNING user_id
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION is_user_org(p_user_id UUID, p_org_id UUID)
RETURNS BOOLEAN AS $$
  SELECT CASE (select count(*)
               from nxo_user_orgs
               where user_id = p_user_id and org_id = p_org_id)
            WHEN 0 THEN false
            ELSE true
          END
$$ LANGUAGE SQL;


/* add_user_group - add a user to an org/group

  example:
    SELECT add_user_group(user_id('u'), group_id('g'), org_id('o'));

  returns:
    user_id uuid

  parameters:
    p_user uuid: user_id
    p_group uuid: group_id
    p_org uuid: org_id
*/
CREATE OR REPLACE FUNCTION add_user_group(p_user_id UUID,
                                          p_group_id UUID,
                                          p_org_id UUID)
RETURNS UUID AS $$
  INSERT INTO nxo_user_groups(user_id, group_id, org_id)
  VALUES(p_user_id, p_group_id, p_org_id)
  ON CONFLICT DO NOTHING
  RETURNING user_id
$$ LANGUAGE SQL;


/* is_user_group - determine if a user is part of an org/group

  example:
    SELECT is_user_group(user_id('u'), group_id('g'), org_id('o'));

  returns:
    boolean: t for in the org/group; f otherwise

  parameters:
    p_user uuid: user_id
    p_group uuid: group_id
    p_org uuid: org_id
*/
CREATE OR REPLACE FUNCTION is_user_group(p_user UUID,
                                         p_group UUID,
                                         p_org UUID)
RETURNS BOOLEAN AS $$
  SELECT CASE (select count(*)
               from nxo_user_groups
               where user_id = p_user
                     and org_id = p_org
                     and group_id = p_group)
            WHEN 0 THEN false
            ELSE true
          END
$$ LANGUAGE SQL;


-- UUID functions.  I expect to use these in conjunction with some of
-- the authz functionality.  Note that there are two defined so that
-- we can speed up the normal case (when a UUID is actually supplied)
-- but still catch non-UUID, converting them to NULLs when the
-- (expensive) exception is thrown.  This isn't generally useful, if
-- you know you have a UUID varchar, just ::UUID it!
CREATE OR REPLACE FUNCTION to_uuid(p_in UUID)
RETURNS UUID AS $$
  BEGIN
    RETURN p_in;
  END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION to_uuid(p_in varchar)
RETURNS UUID AS $$
  BEGIN
   RETURN p_in::UUID;
 EXCEPTION
   WHEN data_exception THEN RETURN NULL;
  END
$$ LANGUAGE plpgsql;

-- Given a table, column, and value return a count of how many times
-- that value appears in the specified table.column.  Used for unique
-- value form validation.
CREATE OR REPLACE FUNCTION check_dup(
       tbl character varying,
       col character varying,
       val character varying)
RETURNS integer
AS $$
  DECLARE
    c int;
  BEGIN
    EXECUTE format('select count(*) from %I where %I::text = $1', tbl, col)
      INTO STRICT c
      USING val;
    RETURN c;
  END;
$$ LANGUAGE plpgsql;

-- As above, but don't count rows with a corresponding id_col=id_val
CREATE OR REPLACE FUNCTION check_dup(
       tbl character varying,
       col character varying,
       val character varying,
       id_col character varying,
       id_val character varying)
RETURNS integer
AS $$
  DECLARE
    c int;
  BEGIN
    EXECUTE format('select count(*)
                      from %I
                     where %I::text = $1 and %I::text != $2',
            tbl, col, id_col)
      INTO STRICT c
      USING val, id_val;
    RETURN c;
  END;
$$ LANGUAGE plpgsql;
