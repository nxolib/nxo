nxo
=====

An OTP library augmenting the native functionality provided by the
Nitrogen framework.

Page Generation
---------------

Settings
--------

Authorization
-------------

Authorization to perform a function is ultimately based on a user
participating in a _Role_.  Let's look at how Roles are constructed.

## Components of the Authorization Scheme

### Organizations

Organizations are a way of assigning an administrative association (an
institution or a department for instance) to a user.  In the NXO
bootstrap code there are three Organizations: global, Ace, and Acme.
The global organization cannot be renamed or removed as it is the
default organization for all NXO users.  Ace and Acme are example
organizations that may be removed.  Organizations are managed in the
NXO Administration UI.

The only required Organization is "global" and not all NXO
applications will need more Organizations defined.

``` text
tbdb_db=# select * from nxo_orgs;
 org_abbrv |    org_name    |      description
-----------+----------------+------------------------
 global    | Global Default | Default Organization
 ace       | Ace Corp.      | Ace Test Organization
 acme      | Acme Co.       | Acme Test Organization
```

### Groups

Groups are a way of naming buckets of functionality.  For instance
"Site User", "Global Administrators", and "Data Managers" are all
groups.  Groups have label (as above) and names ("users",
"administrators", "datamgmt") that are used internally.  Users **do
not** belong to groups directly (although the User Management UI hides
that detail from adminstrators).  Groups are managed in the NXO
Administration UI.

Note that some Groups are denoted as "Global."  This limits their
participation in certain Roles, described below.

``` text
tbdb_db=# select * from nxo_groups;
   group_name   |      group_label      |    description     | global_only
----------------+-----------------------+--------------------+-------------
 users          | Site User             | Default User Group | f
 administrators | Global Administrators | All Access         | t
 usermgmt       | User Managers         | User Managers      | f
 datamgmt       | Data Managers         | Data Managers      | f
 passwd         | Stale Password        | Requres PW Change  | t
 api            | API Users             | API Access         | f
 pending        | Pending Users         | Pending Users      | t
```

### Roles

Roles are the combination of Organization + Group.  For instance,
"global::users" is the Role all NXO users assume.  A user might have
data management access for the Ace Organzation (the Role
"ace::datamgmt") but not either globally or for the Acme Organization.
Roles are not created directly; instead, they're derived from existing
Organizations and Groups.

Note that all Groups are "available" to the global organization.
However, Groups marked as "Global" are **only** available to the
global organization.

``` text
tbdb_db=# select user_id, active, roles from nxo_users;
-[ RECORD 1 ]-------------------------------------------------------------
user_id | 7f38d2e4-c415-4079-ad41-d071feb89418
active  | t
roles   | {global::administrators,global::usermgmt,global::users}
-[ RECORD 2 ]-------------------------------------------------------------
user_id | 45b11c8b-4b5a-428c-9fb2-a0580f6de974
active  | t
roles   | {global::users,acme::datamgmt,acme::users,ace::users,ace::usermgmt}
-[ RECORD 3 ]-------------------------------------------------------------
user_id | 79c6026d-7127-40d8-bd1e-eb7522bffb27
active  | t
roles   | {global::users}
```

### Realms

Realms are collections of Roles and function as if they were a Role.
For instance, the Realm "global::admin_users" comprises
"global::administrators" (the superuser role) and "global::usermgmt".
This allows asking "Does User X particiapte in global::admin_users?"
instead of enumerating each Role individually.

The rationale behind Realms is to provide some abstraction in the
application code.  Not only do we need to ask only one question (as
above) but should the business requirements change the Realm
definition will change but the application code need not.

Note that Realms are defined in the database and are not currently
exposed in the UI.

``` text
tbdb_db=# select * from nxo_realms ;
-[ RECORD 1 ]------------------------------------------------------
realm  | global::admin_something
roles | {global::administrators,global::usermgmt,global::datamgmt}
-[ RECORD 2 ]------------------------------------------------------
realm  | global::admin_everything
roles | {global::administrators}
-[ RECORD 3 ]------------------------------------------------------
realm  | global::admin_users
roles | {global::administrators,global::usermgmt}
-[ RECORD 4 ]------------------------------------------------------
realm  | global::admin_data
roles | {global::administrators,global::datamgmt}
-[ RECORD 5 ]------------------------------------------------------
realm  | global::api
roles | {global::administrators,global::api}

```

## Inactive Users

A user may be marked as "Inactive" in the User Management UI.
Inactive users cannot authenticate (either locally or against a
directory server).  Marking a user as Inactive does not alter the
Roles the user participates in (meaning that if a user's active status
is toggled from Inactive to Active the user's authorizations have not
changed).

Put another way, Active/Inactive status is orthogonal to
Authorizations.


Build
-----

    $ rebar3 compile
