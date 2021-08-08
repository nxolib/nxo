-module(nxopage_debug).
-include("nxo.hrl").
-export([main/0, title/0, body/0]).

-security({groups, [administrators]}).
-postback_security({groups, [administrators]}).
-origin_security({groups, [administrators]}).

main() ->
   #template{ file=nxo:template("debug.html") }.

title() ->
  "NXO Debugging".

body() ->
  ?PRINT(wf:session(user_data)).
