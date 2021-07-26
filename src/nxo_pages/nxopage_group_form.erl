-module(nxopage_group_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , event/1
        , submit_button/0
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  nxo_forms:find_obj(fun nxo_auth_group:find_group/1),
  #template{ file=nxo:template("group_form.html") }.

title() ->
  case nxo_forms:obj_defined() of
    false -> "Add Group";
    true  -> "Edit Group"
  end.

event(group_form_submit) ->
  submission().

submit_button() ->
  #button{ id=group_form_submit,
           postback=group_form_submit,
           class="btn btn-primary",
           text=title() }.

submission() ->
  ID = case nxo_forms:obj_defined() of
         false -> nxo:uuid();
         _     -> wf:state(objID)
       end,
  case nxo_validate:validate(group_form, #{group_id => wf:state(objID)}) of
    true ->
      nxo_db:q(group_add, [ID | nxo_datamap:apply(group_form)]),
      wf:redirect("/groups");
    false ->
      nxo_view:report_validation_failure()
  end.
