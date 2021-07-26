-module(nxopage_user_form).
-include("nxo.hrl").

-export([ main/0
        , title/0
        , event/1
        , submit_button/0
        , group_form/0
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  nxo_forms:find_obj(fun nxo_auth_user:find/1),
  #template{ file=nxo:template("user_form.html") }.

title() ->
  case nxo_forms:obj_defined() of
    false -> "Add User";
    true  -> "Edit User"
  end.

event(org_form_submit) ->
  submission().

submit_button() ->
  #button{ id=org_form_submit,
           postback=org_form_submit,
           delegate=?MODULE,
           class="btn btn-primary",
           text=title() }.

submission() ->
  ID = case nxo_forms:obj_defined() of
         false -> nxo:uuid();
         _     -> wf:state(objID)
       end,
  case nxo_validate:validate(user_form, #{user_id => wf:state(objID)}) of
    true ->
      [UID, OID] = nxo_db:cascading_update(
                     [ {user_add, [ID | nxo_datamap:apply(user_add)]},
                       {user_org_add, nxo_datamap:apply(user_org_add)}
                     ]),
      update_groups(UID, OID, selected_groups()),
      wf:redirect("/users");
    false ->
      nxo_view:report_validation_failure()
  end.

%% user_org_groups is a pretty fancy query.  It first deletes all of
%% the existing UID/OID associations in the table then inserts all of
%% the selected GIDs (possibly changing the OID to match the global
%% group if it's a global_only group). Note that the last three
%% parameters (UIDParam, OIDParam, GIDs) are arrays that are unnested
%% in the query and **must** be the same length (hence the duplicate).
update_groups(UID, OID, GIDs) ->
  BinGIDs = lists:map(fun wf:to_binary/1, GIDs),
  UIDParam = lists:duplicate(length(GIDs), UID),
  OIDParam = lists:duplicate(length(GIDs), OID),
  nxo_db:query(user_org_groups, [UID, OID, UIDParam, OIDParam, BinGIDs]).


% prepares the Organization and Group part of the form.  we did that
% part separately so that future versions could add multiple instances
% of this form segment (if the user is in more than one organization).
group_form() ->
  Data = #{orgs => nxo_org:all(),
           user => wf:state(obj),
           groups => nxo_auth_group:all()},
  #template{ text=nxo_template:render(user_group, Data) }.

% returns a list of group_ids that have their checkboxes checked.
selected_groups() ->
  lists:filter(fun(G) -> wf:q(G) == G end, nxo_auth_group:ids()).
