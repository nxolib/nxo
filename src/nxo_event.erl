-module(nxo_event).
-include("nxo.hrl").
-export([
          event_handler/0
        , notify/1
        , add_handler/1
        , add_handler/2
        , delete_handler/1
        , delete_handler/2
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
