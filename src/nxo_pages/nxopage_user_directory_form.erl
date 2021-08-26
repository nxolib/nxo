-module(nxopage_user_directory_form).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        , autocomplete_enter_event/2
        , autocomplete_select_event/2
        , organization_list/0
        , search_box/0
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).
-origin_security({groups, [administrators, usermgmt]}).

main() ->
  #template{ file=nxo:template("user_directory_form.html") }.

title() ->
  "Add Directory User".

body() ->
  ok.

event(organization_changed) ->
  wf:set(directory_search, []),
  case wf:q(directory_organization) of
    [] ->
      wf:disable(directory_search);
    OrgAbbrv ->
      wf:enable(directory_search)
  end.

autocomplete_select_event(Element, _Tag) ->
  ok.

autocomplete_enter_event(Term, _Tag) ->
  OrgAbbrv = wf:q(directory_organization),
  Results = [[ {id,    maps:get(<<"uid">>, Entry)},
               {label, maps:get(<<"displayname">>, Entry)},
               {value, maps:get(<<"displayname">>, Entry)} ]
             || Entry <- nxo_directory:search(Term, OrgAbbrv)],
  jsx:encode(Results).

organization_list() ->
  Options = [ #option{ text=maps:get(<<"org_name">>, O),
                       value=maps:get(<<"org_abbrv">>, O)}
            || O <- nxo_directory:directory_organizations() ],
  #dropdown{ class="custom-select",
             id=directory_organization,
             postback=organization_changed,
             options=[#option{} | Options] }.

search_box() ->
  #textbox_autocomplete{ tag=directory_search,
                         minLength=3,
                         disabled=true,
                         class="form-control",
                         html_id="directory_search",
                         id=directory_search }.
