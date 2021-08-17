-module(nxopage_directory_form).
-include("nxo.hrl").
-export([ main/0
        , title/0
        , body/0
        , button/1
        , event/1
        ]).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() ->
  #template{ file=nxo:template("directory_form.html") }.

title() ->
  "Add/Edit Directory".

body() ->
  Orgs = #{ orgs => nxo_org:all() },
  Data = case string:split(wf:path_info(), "::") of
           [OrgAbbrv, Directory] ->
             case nxo_directory:find(wf:url_decode(OrgAbbrv),
                                     wf:url_decode(Directory)) of
               [] -> Orgs;
               [Dir] -> maps:merge(Dir, Orgs)
             end;
           _ -> Orgs
         end,
  #template{ text=nxo_template:render(directory_form, Data) }.

button({submit, OrgAbbrv}) ->
  Text = case OrgAbbrv of
           [] -> "Add Directory";
           _  -> "Update Directory"
         end,
  #button{ class=?btn_primary,
           postback=submit,
           text=Text}.

event(submit) ->
  case nxo_validate:validate(directory_form, #{}) of
    true ->
      Data = nxo_datamap:apply(directory_form),
      nxo_db:q(add_directory, Data),
      wf:redirect("/directories");
    false ->
      nxo_view:report_validation_failure()
  end.
