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

## Components of the Authorization Schema

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

### Groups

Groups are a way of naming buckets of functionality.  For instance
"Site User", "Global Administrators", and "Data Managers" are all
groups.  Groups have labels (as above) and names ("users",
"administrators", "datamgmt") that are used internally.  Users **do
not** belong to groups directly (although the User Management UI hides
that detail from adminstrators).  Groups are managed in the NXO
Administration UI.

Note that some Groups are denoted as "Global."  This limits their
participation in certain Roles, described below.

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
