-module(nxo_audit).
-include("nxo.hrl").
-export([
          event_handler/0
        , notify/1
        , add_handler/1
        , add_handler/2
        , delete_handler/1
        , delete_handler/2
        , record/1
        , audit_record_to_map/1
        ]).

notify(Msg) ->
  gen_event:notify(event_handler(), Msg).

%% @doc Returns the PID of the NXO event handler.
-spec event_handler() -> pid().
event_handler() ->
  nprocreg:get_pid(?EVENT).

%% @doc Adds an event handler
add_handler(Module) ->
  add_handler(Module, []).

add_handler(Module, Args) ->
  gen_event:add_handler(event_handler(), Module, Args).

%% @doc Deletes an event handler
delete_handler(Module) ->
  delete_handler(Module, []).

delete_handler(Module, Args) ->
  gen_event:delete_handler(event_handler(), Module, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FUNCTIONS FOR THE DEFAULT NXO_AUDIT_HANDLER %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
record(Record) when is_record(Record, audit) ->
  record(audit_record_to_map(Record));

record(Params) when is_map(Params) ->
  NormalizedParams = normalized_params(Params),
  ?PRINT(nxo_db:q(audit_insert, NormalizedParams)).

normalized_params(Params) ->
  %% activity and user_id are not optional.
  maps:merge(#{ target => undefined,
                result => undefined,
                comment => undefined }, Params).

audit_record_to_map(Record) ->
  lists:foldl(fun({I, E}, Acc) -> Acc#{E => element(I, Record)} end,
              #{},
              lists:zip(lists:seq(2, (record_info(size, audit))),
                        (record_info(fields, audit)))).
