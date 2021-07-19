-module (nxo_security_handler).
-include("nxo.hrl").
-behaviour (security_handler).
-export ([
           init/2
         , finish/2
         , authorize/2
         , reset_session_timer/0
         ]).


init(_Config, State) ->
  reset_session_timer(),
  case nxo:global_auth_allowed() of
    true  -> authorize(first_request,
                       fun nxo_page_security:page_security_check/0);
    false -> wf_context:page_module(page_login)
  end,
  {ok, State}.

finish(_Config, State) ->
  {ok, State}.

authorize(EventType, AuthFn) ->
  %% i expect the EventType to be 'first_request' or 'postback' but
  %% other than logging that information, we're not doing much with
  %% it.
  case AuthFn() of
    {allow, _Spec, _User, _Msg} ->
      ok;
    {deny, Spec, User, Msg} ->
      Data = auth_log_data(EventType, Spec, User, Msg),
      logger:notice("~p~n", [Data]),
      wf_context:event_module(page_not_authorized),
      wf_context:page_module(page_not_authorized)
  end.

reset_session_timer() ->
  case wf:user() of
    undefined -> ok;
    ID        -> nxo_sessions:reset(wf_context:context(), ID)
  end.

auth_log_data(EventType, Spec, User, Msg) ->
  #{ event_type => EventType,
     spec => Spec,
     user => User,
     msg => Msg,
     context_page => wf_context:page_module(),
     context_event => wf_context:event_module() }.
