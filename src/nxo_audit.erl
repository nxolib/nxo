-module(nxo_audit).
-include("nxo.hrl").
-export([
          record/1
        , audit_record_to_map/1
        ]).

%% Some notes about NXO auditing (and event management).
%%
%% The NXO application starts an event handler on startup.  The PID of
%% the event handler is available via nxo_event:event_handler().
%%
%% The nxo_event module contains some convenience functions to
%% add/delete handlers as well as the notify/1 function that sends a
%% gen_event:notify to the NXO handler.
%%
%% NXO contains two default handlers (which may, of course, be removed
%% when implementing applications with NXO):
%%
%%    nxo_development_handler: when is_development() is true, this
%%    handler is installed.  It simply prints all events to the
%%    terminal.
%%
%%    nxo_audit_handler: receives notifications with an #audit{}
%%    record as its parameter and inserts that record into the DB.  It
%%    does this asynchronously.
%%
%% Note there's a convenience metnod, audit_record_to_map/1, that will
%% convert an #audit{} record into a DB appropriate parameter map.
%%
%% The nxo_audit table does not allow NULLs; unsupplied values are
%% empty strings.
%%
%% The record is defined with the fields:
%%
%%    - activity
%%    - user_id
%%    - target
%%    - result
%%    - comment
%%
%% Of these, only activity and user_id are required.
%%
%% This module is called by the nxo_audit_handler to format the
%% #audit{} and insert it into the nxo_audit table in the DB.

record(Record) when is_record(Record, audit) ->
  record(audit_record_to_map(Record));

record(Params) when is_map(Params) ->
  NormalizedParams = normalized_params(Params),
  [ok, _] = nxo_db:q(audit_insert, NormalizedParams).

normalized_params(Params) ->
  %% activity and user_id are not optional.
  maps:merge(#{ target => [],
                result => [],
                comment => [] }, Params).

audit_record_to_map(Record) ->
  lists:foldl(fun({I, E}, Acc) -> Acc#{E => element(I, Record)} end,
              #{},
              lists:zip(lists:seq(2, (record_info(size, audit))),
                        (record_info(fields, audit)))).
