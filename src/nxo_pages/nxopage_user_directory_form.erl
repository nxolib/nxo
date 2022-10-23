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
        , button/1
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

event(reset) ->
  wf:disable(continue),
  wf:disable(directory_search),
  wf:set(entry_specs, []),
  wf:set(directory_search, []),
  wf:set(directory_organization, []);

event(continue) ->
  [Mail, OrgAbbrv, Directory] =
    string:split(wf:q(entry_specs), ";;", all),
  display_user_form(Mail, OrgAbbrv, Directory);

event(organization_changed) ->
  wf:set(directory_search, []),
  wf:set(entry_specs, []),
  case wf:q(directory_organization) of
    [] ->
      wf:disable(directory_search);
    _OrgAbbrv ->
      wf:enable(directory_search)
  end.

autocomplete_select_event(Element, _Tag) ->
  wf:set(entry_specs, proplists:get_value(<<"id">>, Element)),
  wf:enable(continue).



autocomplete_enter_event(Term, _Tag) ->
  OrgAbbrv = wf:q(directory_organization),
  Results = [ [{id,    autocomplete_value(Entry)},
               {label, autocomplete_label(Entry)},
               {value, maps:get(mail, Entry)}]
              || Entry <- nxo_directory:search(account, Term, OrgAbbrv) ],
  jiffy:encode(Results).


button(submit) ->
  #button{ class="wfid_continue btn btn-sm btn-success",
           text="Continue",
           disabled=true,
           postback=continue };
button(reset) ->
  #button{ class="btn btn-sm btn-secondary",
           text="Reset Search",
           postback=reset }.


autocomplete_value(Entry) ->
  [Mail, {OrgAbbrv, Directory}] =
    [ maps:get(A, Entry) || A <- [mail, directory] ],
  <<Mail/binary, ";;", OrgAbbrv/binary, ";;", Directory/binary>>.

autocomplete_label(Entry) ->
  [LName, FName, {_, Directory}] =
    [ maps:get(A, Entry) || A <- [lname, fname, directory] ],
  <<LName/binary, ", ", FName/binary, " (", Directory/binary, ")">>.

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

display_user_form(Mail, OrgAbbrv, Directory) ->
  case nxo_directory:search(entry, Mail, OrgAbbrv) of
    [] -> ok;
    {entry, [Res | _]} ->
      Data = #{ all_groups => nxo_group:all(),
                directory_org => OrgAbbrv,
                directory => Directory,
                source => directory,
                email => maps:get(mail, Res),
                first_name => maps:get(fname, Res),
                last_name => maps:get(lname, Res) },
      Form = #template{ text=nxo_template:pretty_render(user_form, Data) },
      wf:update(eager, the_form, Form),
      OrgForm = nxopage_user_form:org_form(OrgAbbrv, nxo:uuid()),
      wf:insert_bottom(defer, org_forms, OrgForm)
  end.
