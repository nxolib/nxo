%%%-------------------------------------------------------------------
-module(nxo_audit_handler).
-include("nxo.hrl").
-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).

init([]) ->
  {ok, #state{}}.

handle_event(Record, State) when is_record(Record, audit) ->
  nxo_audit:record(Record),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(Arg, _State) ->
  logger:info("~p terminated with arg ~p", [?MODULE, Arg]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.
