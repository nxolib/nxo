-module(nxopage_user_ad_add).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , event/1
        , button/1
        , organization_list/0
        , usersearch/0
        , autocomplete_enter_event/2
        , autocomplete_select_event/2
        , form_value/1
        , group_form/0
        , submit_button/0
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).
-origin_security({groups, [administrators, usermgmt]}).

main() ->
  case wf:path_info() of
    [] ->
      #template{ file=nxo:template("user_ad_add.html") };
    ID ->
      wf:state(ad_data, nxo_ad:user_data(ID)),
      wf:disable(defer, password),
      #template{ file=nxo:template("user_form_ad.html") }
  end.

title() -> "Add Directory User".

organization_list() ->
  Options = [ #option{ text=maps:get(<<"org_name">>, O),
                       value=maps:get(<<"org_abbrv">>, O)}
            || O <- nxo_directory:directory_organizations() ],
  #dropdown{ class="custom-select",
             id=organization,
             options=[#option{} | Options] }.


usersearch() ->
  #textbox_autocomplete{ tag=directory_search,
                         minLength=3,
                         class="form-control",
                         html_id="directory_search",
                         id=directory_search }.

autocomplete_enter_event(Term, _Tag) ->
  Results = [ [
               {id,    proplists:get_value(<<"samaccountname">>, Entry)},
               {label, proplists:get_value(<<"displayname">>, Entry)},
               {value, proplists:get_value(<<"displayname">>, Entry)}
              ]
              || Entry <- nxo_ad:search_for_user(Term) ],
  jsx:encode(Results).


autocomplete_select_event(Element, _Tag) ->
  ID = proplists:get_value(<<"id">>, Element),
  Data = #{ user =>  nxo_ad:user_data(wf:to_list(ID)) },
  wf:update(userinfo,
            #template{ text=nxo_template:pretty_render(ad_user, Data) }).


button(ID) ->
  #button{ text="Continue",
           class="btn btn-small btn-primary",
           postback={add, ID} }.


event({add, ID}) ->
  wf:redirect("/user_ad_add/" ++ wf:to_list(ID)).

form_value(password) ->
  nxo_forms:form_value(password);
form_value(Field) ->
  maps:get(Field, wf:state(ad_data), []).

submit_button() ->
  page_user_form:submit_button().

group_form() ->
  page_user_form:group_form().
