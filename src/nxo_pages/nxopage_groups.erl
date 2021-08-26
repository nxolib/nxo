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
  Groups = nxo_group:all(),
  #template{ text=nxo_template:render(group_list, #{groups => Groups }) }.

button(add) ->
  #button{ postback=add,
           body=nxo:fa(plus),
           class="btn btn-sm btn-success" }.

button(edit, GroupName) ->
  #button{ postback={edit, GroupName},
           body=nxo:fa(edit),
           class="btn btn-sm btn-primary" };
button(delete, GroupName) ->
  #button{ postback={delete, GroupName},
           body=nxo:fa(ban),
           class="btn btn-sm btn-danger" }.

event(add) ->
  wf:redirect("/group_form");
event({edit, GroupName}) ->
  wf:redirect("/group_form/" ++ GroupName);
event({delete, GroupName}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, GroupName} });
event({delete_confirmed, GroupName}) ->
  nxo_group:delete_group(GroupName),
  wf:update(group_body, body()).
