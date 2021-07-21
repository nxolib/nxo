-module(nxopage_organization).
-include("nxo.hrl").

-export([
          main/0
        , title/0
        , body/0
        , event/1
        , button/1
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() -> #template{ file=nxo:template("organization.html") }.

title() -> "Organization Management".

body() ->
  Orgs = nxo_db:q(all_orgs),
  #template{ text=nxo_template:render(org_list, #{orgs => Orgs}) }.

event(add) ->
  wf:redirect("/org_form");

event({edit, ID}) ->
  wf:redirect("/org_form/" ++ ID);
event({info, ID}) ->
  page_org_info:open_panel(ID);
event({delete, ID}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, ID} });
event({delete_confirmed, ID}) ->
  nxo_org:delete(ID),
  wf:update(org_body, body()).

button(add) ->
  #button{postback=add,
          body=nxo:fa(plus),
          class=?btn_success};
button({edit, ID}) ->
  #button{postback={edit, ID},
          body=nxo:fa(edit),
          class=?btn_primary};
button({info, ID}) ->
  #button{postback={info, ID},
          body=nxo:fa("info-circle"),
          class=?btn_primary};
button({delete, ID}) ->
  case nxo_authz:is(superuser) of
    false -> [];
    true  -> #button{body=nxo:fa(ban),
                     class=?btn_danger,
                     postback={delete, ID}}
  end.
