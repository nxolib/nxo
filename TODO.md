NXO Refactoring TODOs
===========

  - [ ] Change password page should be refactored to use validation lib.
  - [x] Add audit event handler.
  - [x] Factor out 2FA code.
  - [ ] update nxo_security_handler to use a custom *_not_authorized:
    the app designer should be able to specify a module; further,
    if wf:user is undef, maybe show a login screen?
  - [ ] bootstrap rebar3 plugin?
  - [ ] bottom admin buttons?
