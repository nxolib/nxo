-module(nxopage_org_form).
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
  #template{ file=nxo:template("org_form.html") }.

title() ->
  "Organization Management".

body() ->
  Org = case wf:path_info() of
           [] -> #{};
           OrgAbbrv -> hd(nxo_org:find(OrgAbbrv))
         end,
  #template{ text=nxo_template:pretty_render(org_form, Org) }.

event(org_form_submit) ->
  submission().

button({submit, OrgName}) ->
  Text = case OrgName of
           [] -> "Create Organization";
           _ -> "Update " ++ OrgName
         end,
  #button{ id=org_form_submit,
           postback=org_form_submit,
           class=?btn_primary,
           text=Text }.

submission() ->
  case nxo_validate:validate(org_form, #{ org_id => wf:path_info() }) of
    true ->
      OrgDetails = nxo_datamap:apply(org_add),
      OrgContact = nxo_datamap:apply(org_contact_add),
      nxo_org:upsert(OrgDetails, OrgContact),
      wf:redirect("/organization");
    false ->
      nxo_view:report_validation_failure()
  end.
