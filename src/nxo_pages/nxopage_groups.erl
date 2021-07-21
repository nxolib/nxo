-module(nxopage_groups).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        , button/1
        , button/2
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() -> #template{ file=nxo:template("groups.html") }.

title() -> "Group Management".

body() ->
  Groups = nxo_auth_group:all(),
  #template{ text=nxo_template:render(group_list, #{groups => Groups }) }.

button(add) ->
  #button{ postback=add,
           body=nxo:fa(plus),
           class="btn btn-sm btn-success" }.

button(edit, ID) ->
  #button{ postback={edit, ID},
           body=nxo:fa(edit),
           class="btn btn-sm btn-primary" };
button(delete, ID) ->
  #button{ postback={delete, ID},
           body=nxo:fa(ban),
           class="btn btn-sm btn-danger" }.

event(add) ->
  wf:redirect("/group_form");
event({edit, ID}) ->
  wf:redirect("/group_form/" ++ ID);
event({delete, ID}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, ID} });
event({delete_confirmed, ID}) ->
  nxo_auth_group:delete_group(ID),
  wf:update(group_body, body()).
