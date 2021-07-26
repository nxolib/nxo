-module(nxopage_request_access_confirm).
-include("nxo.hrl").

-export([
          main/0
        , title/0
        ]).

-security(none).
-postback_security(none).

main() -> #template{ file=nxo:template("request_access_confirm.html") }.

title() -> "Request Access Confirmation".
