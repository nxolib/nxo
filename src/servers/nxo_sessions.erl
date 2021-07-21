%%%-------------------------------------------------------------------
%%% @author Bunny Lushington <bunnylushington@lucy>
%%% @copyright (C) 2019, Bunny Lushington
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2019 by Bunny Lushington <bunnylushington@lucy>
%%%-------------------------------------------------------------------
-module(nxo_sessions).
-include("nxo.hrl").
-behaviour(gen_server).

%% API
-export([
          start/2
        , reset/2
        , is_registered/1
        , kill/1
        , event/1
        , show_warning/1
        , do_warning/1
        , do_terminate/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER(ID), {global, {?MODULE, ID}}).
-define(WARNING_TIME, nxo:session_warning()).  % one minute before timeout.
-define(TIMEOUT, nxo:session_timeout()).


%%%===================================================================
%%% API
%%%===================================================================

start(Context, ID) ->
  gen_server:start(?SERVER(ID), ?MODULE,
                   [Context, ID, ?TIMEOUT, ?WARNING_TIME], []).


reset(Context, ID) ->
  case is_registered(ID) of
    PID when is_pid(PID) -> gen_server:cast(?SERVER(ID), {reset, Context});
    undefined            -> start(Context, ID)
  end.

event({reset, Context, ID}) ->
  ?MODULE:reset(Context, ID),
  nxo_modal:close().

show_warning(State) ->
  ID = maps:get(id, State),
  gen_server:cast(?SERVER(ID), show_warning).

is_registered(ID) ->
  global:whereis_name({?MODULE, ID}).

kill(State) when is_map(State) ->
  kill(maps:get(id, State));
kill(ID) ->
  gen_server:cast(?SERVER(ID), kill).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Context, ID, Timeout, Warning]) ->
  process_flag(trap_exit, true),
  State = #{ctx => Context, id => ID, status => ok,
            timeout => Timeout, Warning => Warning},
  {ok, State, Timeout}.


handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({reset, Context}, State) ->
  Timeout = maps:get(timeout, State, ?TIMEOUT),
  NewState = maps:merge(State, #{ctx => Context, status => ok}),
  {noreply, NewState, Timeout};

handle_cast(show_warning, State) ->
  ?MODULE:do_warning(State),
  {noreply, State, maps:get(warning, State, ?WARNING_TIME)};

handle_cast(kill, State) ->
  ?MODULE:do_terminate(State),
  {stop, normal, State}.

handle_info(timeout, State) ->
  case maps:get(status, State) of
    ok ->
      ?MODULE:show_warning(State),
      NewState = maps:merge(State, #{status => warning}),
      {noreply, NewState};
    warning ->
      ?MODULE:kill(State),
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, Status) -> Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_warning(State) ->
  wf_context:context(maps:get(ctx, State)),
  wf_context:clear_action_queue(),
  display_warning_modal(State),
  wf:flush().

display_warning_modal(State) ->
  Postback = {reset, maps:get(ctx, State), maps:get(id, State)},
  Body = #panel{text="Your session will timeout in one minute."},
  Button = #button{class="btn btn-primary",
                   postback=Postback,
                   delegate=?MODULE,
                   text="Continue Session"},
  Title = "Inactive Session Warning",
  nxo_modal:open({}, Title, Body, [{modal_footer, Button}]).

do_terminate(State) ->
  wf_context:context(maps:get(ctx, State)),
  wf_context:clear_action_queue(),
  nxo_db:query(user_audit_insert, [wf:user(), true, "timeout", ""]),
  wf:logout(),
  wf:session(auth_message, "Logged out due to inactivity."),
  wf:redirect("/login"),
  wf:flush().
