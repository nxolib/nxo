-module(nxopage_mailcheck).
-include("nxo.hrl").
-export([
          event/1
        , main/0
        , title/0
        , default/0
        , button/0
        ]).

-security({group, administrators}).
-postback_security({group, administrators}).

main() ->
  #template{ file=nxo:template("mailcheck.html") }.

title() -> "Mail Check".

default() ->
  maps:get(<<"email">>, wf:session(user_data), []).

button() ->
  #button{ class="btn btn-small btn-primary",
           postback=mailcheck,
           text="Send Test Mail" }.

event(mailcheck) ->
  _Res = nxo_mail:send(mailcheck, wf:q(address), #{ userid => wf:user() }),
  wf:update(result, "Mail sent.").
