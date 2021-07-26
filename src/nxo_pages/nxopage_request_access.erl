-module(nxopage_request_access).
-include("nxo.hrl").

-export([
          main/0
        , title/0
        , body/0
        , button/0
        , event/1
        ]).

-security(none).
-postback_security(none).

main() -> #template{ file=nxo:template("request_access.html") }.

title() -> "Request Access".

body() ->
  Data = #{ orgs => nxo_org:all() },
  #template{ text=nxo_template:render(request_form, Data) }.

event(new_user) ->
  submission().

button() ->
  #button{ class="btn btn-small btn-primary",
           text="Request Access",
           postback=new_user }.

submission() ->
  case nxo_validate:validate(request_form) of
    true ->
      process_request(),
      wf:redirect("/request_access_confirm");
    false ->
      report_validation_failure()
  end.

process_request() ->
  %% add the user
  Params = [nxo:uuid() | nxo_datamap:apply(user_add)],
  UID = nxo_db:q(scalar, user_add, Params),

  %% maybe add to an organization
  OID = wf:q(org_id),
  case nxo:is_uuid(OID) of
    true -> nxo_db:query(org_add_user,
                         [UID, OID, true, wf:q(is_contact), wf:q(title)]);
    false -> ok
  end,

  nxo:notify({acct_request, UID, wf:params()}),

  %% add project information
  %% R = nxo_db:query(new_user_project_details,
  %%                  [UID | nxo_datamap:apply(new_user_project_details)]),

  %% add the user to the pending group
  nxo_auth_group:add_to_group(UID, pending).

  %% XXX: This causes a cowboy process crash?
  %% notify admins of account request
  %% nxo_mail:send_to_admin(account_requested,
  %%                        #{ subject => "TBDB Access Requested",
  %%                           acct_email => wf:q(email) }).


report_validation_failure() ->
  wf:wire(#script{ script="window.scrollTo(0,0)" }),
  wf:wire(#alert{ text="There are invalid fields." }).
