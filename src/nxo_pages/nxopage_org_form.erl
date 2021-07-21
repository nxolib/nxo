-module(nxopage_org_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , event/1
        , submit_button/0
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  nxo_forms:find_obj(fun nxo_org:find/1),
  #template{ file=nxo:template("org_form.html") }.

title() ->
  case nxo_forms:obj_defined() of
    false -> "Add Organization";
    true  -> "Edit Organization"
  end.

event(org_form_submit) ->
  submission().

submit_button() ->
  #button{ id=org_form_submit,
           postback=org_form_submit,
           class=?btn_primary,
           text=title() }.

submission() ->
  ID = case nxo_forms:obj_defined() of
         false -> nxo:uuid();
         _     -> nxo_forms:obj_id()
       end,
  case nxo_validate:validate(org_form, #{org_id => ID}) of
    true ->
      OrgDetails = nxo_datamap:apply(org_add),
      OrgContact = nxo_datamap:apply(org_contact_add),
      nxo_org:upsert(ID, OrgDetails, OrgContact),
      wf:redirect("/organization");
    false ->
      nxo_view:report_validation_failure()
  end.
