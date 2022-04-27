NXO Refactoring TODOs
===========

  - [x] Add audit event handler.
  - [x] Factor out 2FA code.
  - [x] update nxo_security_handler to use a custom *_not_authorized:
    the app designer should be able to specify a module;
  - [x] bootstrap rebar3 plugin?
  - [x] bottom admin buttons
  - [x] implement page for retrieving audit data
  - [ ] make nxo_authz dynamic and run-time configured


Nice (but not essential) Things
-----------
  - [ ] fancy up audit page (search, paging, &c.)
  - [ ] Change password page should be refactored to use validation lib.

Notes to self:
---

authz_default.yml is partially written.  The plan here is to load that (like settings.yml) into a DB file and replace (a) all of those macros and (b) most of nxo_authz.  It's unclear that "realms" should be managed at runtime since the available realms must align with calls (e.g., nxo_authz:can/1) in the code.  It does seem like -security() and -postback_security() should respect realms as well as groups though.  Haven't thought through realm + organization.
