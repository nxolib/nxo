-module(nxopage_group_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , body/0
        , event/1
        , button/1
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  #template{ file=nxo:template("group_form.html") }.

title() ->
  "Group Management".

body() ->
  Group = case wf:path_info() of
            [] -> #{};
            GroupName -> hd(nxo_group:find(GroupName))
          end,
  #template{ text=nxo_template:pretty_render(group_form, Group) }.

event(group_form_submit) ->
  submission().

button(submit) ->
  #button{ id=group_form_submit,
           postback=group_form_submit,
           class="btn btn-primary",
           text=title() }.

submission() ->
  case nxo_validate:validate(group_form, #{group_id => wf:path_info()}) of
    true ->
      nxo_db:q(group_add, nxo_datamap:apply(group_form)),
      wf:redirect("/groups");
    false ->
      nxo_view:report_validation_failure()
  end.
