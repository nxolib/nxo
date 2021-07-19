-module(nxo_postback_handler).
-behaviour(postback_handler).
-include("nxo.hrl").
-export([
          init/2
        , finish/2
        , postback_request/2
        ]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

postback_request(_Config, _State) ->
  Fn = fun nxo_page_security:postback_security_check/0,
  nxo_security_handler:authorize(postback, Fn),
  nxo_security_handler:reset_session_timer().
