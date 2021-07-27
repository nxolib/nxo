-module(nxopage_audit).
-include("nxo.hrl").
-export([
          body/0
        , title/0
        , main/0
        ]).

-security({groups, [administrators]}).
-postback_security({groups, [administrators]}).

main() ->
  #template{ file=nxo:template("audit.html") }.

title() ->
  "Audit Log".

body() ->
  Data = #{ log => nxo_db:q(audit_recent)},
  #template{ text=nxo_template:pretty_render(audit, Data) }.
