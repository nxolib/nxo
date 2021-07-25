-module(nxopage_api).
-include("nxo.hrl").
-export([main/0, title/0, body/0, event/1]).

-security({groups, [administrators, api]}).
-postback_security({groups, [administrators, api]}).

main() -> #template{ file=nxo:template("api.html") }.

title() -> "API Key Management".

body() ->
  APIKey = nxo_api:user_key(wf:user()),
  #panel{class="row",
     body=[
           #textbox{class="offset-sm-1 col-sm-4 form-control",
                    id=api_key,
                    text=APIKey,
                    readonly=true},
           #button{class="ml-3 btn btn-sm btn-primary col-sm-2",
                   text="Regenerage Key",
                   postback=regenerate}
          ]}.

event(regenerate) ->
  NewKey = nxo_api:change_key(wf:user()),
  wf:set(api_key, NewKey).
