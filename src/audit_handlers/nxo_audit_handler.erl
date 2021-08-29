%%%-------------------------------------------------------------------
-module(nxo_audit_handler).
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

handle_event({setting_change, {Group, Setting, Val, OldVal, User}}, State) ->
  Params = #{ activity => settings_change,
              user_id => User,
              target => Group ++ "/" ++ Setting,
              result => Val,
              comment => "was: " ++ OldVal },
  nxo_audit:record(Params),
  {ok, State};

handle_event({api_key_change, User}, State) ->
  Params = #{ activity => api_key_change,
              user_id => User },
  nxo_audit:record(Params),
  {ok, State};

handle_event({authentication_event, {User, Target, Result}}, State) ->
  Params = #{ activity => authentication_event,
              user_id => User,
              target => Target,
              result => Result},
  nxo_audit:record(Params),
  {ok, State};

handle_event({authentication_event, {User, Target, Result, Comment}}, State) ->
  Params = #{ activity => authentication_event,
              user_id => User,
              target => Target,
              result => Result,
              comment => Comment },
  nxo_audit:record(Params),
  {ok, State};

handle_event({password_changed, UserID}, State) ->
  Params = #{ activity => password_changed,
              user_id => UserID,
              target => undefined,
              result => success },
  nxo_audit:record(Params),
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
