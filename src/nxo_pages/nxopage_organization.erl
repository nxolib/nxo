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

event({edit, Abbrv}) ->
  wf:redirect("/org_form/" ++ Abbrv);
event({info, Abbrv}) ->
  nxopage_org_info:open_panel(Abbrv);
event({delete, Abbrv}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, Abbrv} });
event({delete_confirmed, Abbrv}) ->
  nxo_org:delete(Abbrv),
  wf:update(org_body, body()).

button(add) ->
  #button{postback=add,
          body=nxo:fa(plus),
          class=?btn_success};
button({edit, Abbrv}) ->
  #button{postback={edit, Abbrv},
          body=nxo:fa(edit),
          class=?btn_primary};
button({info, Abbrv}) ->
  #button{postback={info, Abbrv},
          body=nxo:fa("info-circle"),
          class=?btn_primary};
button({delete, Abbrv}) ->
  case wf:role(administrators) of
    false -> [];
    true  -> #button{body=nxo:fa(ban),
                     class=?btn_danger,
                     postback={delete, Abbrv}}
  end.
