-module(nxopage_not_authorized).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        ]).

-security(none).
-postback_security(none).

main() -> #template{ file=nxo:template("not_authorized.html") }.

title() -> "Not Authorized".

body() -> ok.

event(_) ->
  wf:redirect("/not_authorized").
