-module(nxopage_directories).
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
  #template{ file=nxo:template("directories.html") }.

title() ->
  "Directory Management".

body() ->
  Dirs = nxo_db:q(all_directories),
  #template{ text=nxo_template:render(directories, #{ dirs => Dirs }) }.

button(add) ->
  #button{ class=?btn_success,
           postback=add,
           body=nxo:fa(plus)
         };
button({edit, OrgAbbrv, Directory}) ->
  #button{ class=?btn_primary,
           postback={edit, OrgAbbrv, Directory},
           body=nxo:fa(edit)
         };
button({info, OrgAbbrv, Directory}) ->
  #button{ class=?btn_primary,
           postback={info, OrgAbbrv, Directory},
           body=nxo:fa("info-circle")
         };
button({delete, OrgAbbrv, Directory}) ->
  #button{ class=?btn_danger,
           postback={delete, OrgAbbrv, Directory},
           body=nxo:fa(ban)
         }.

event(add) ->
  wf:redirect("/directory_form");
event({edit, OrgAbbrv, Directory}) ->
  wf:redirect("/directory_form/" ++ OrgAbbrv ++ "::" ++ Directory);
event({info, OrgAbbrv, Directory}) ->
  nxopage_directory_info:open_panel(OrgAbbrv, Directory);
event({delete, OrgAbbrv, Directory}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, OrgAbbrv, Directory} });
event({delete_confirmed, OrgAbbrv, Directory}) ->
  nxo_directory:delete(OrgAbbrv, Directory),
  wf:update(dir_body, body()).
