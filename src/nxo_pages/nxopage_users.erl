-module(nxopage_users).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        , event/1
        , button/3
        ]).

-define(PERPAGE, 25).

-security({groups, [administrators, usermgmt]}).
-postback_security({groups, [administrators, usermgmt]}).

main() -> #template{ file=nxo:template("users.html") }.

title() -> "User Management".

body() ->
  Users = nxo_user:all(),
  #template{ text=nxo_template:pretty_render(user_list, #{users => Users}) }.

event(help) ->
  Button = #button{ class="btn btn-primary",
                    text="Close",
                    postback=close_button_legend },
  Opts = [{modal_footer, Button}],
  Content = button_legend(),
  nxo_modal:open({}, "User Management Legend", Content, Opts);

event(refresh) ->
  wf:update(body, body());

event(add) ->
  wf:redirect("/user_form");
event(add_directory) ->
  wf:redirect("/user_directory_form");
event({info, ID}) ->
  nxopage_user_info:open_panel(ID);
event({edit, ID}) ->
  wf:redirect("/user_form/" ++ ID);
event({activate, ID}) ->
  nxo_user:toggle_active_flag(ID, true),
  event(refresh);
event({inactivate, ID}) ->
  nxo_user:toggle_active_flag(ID, false),
  event(refresh);
event({mask, ID}) ->
  case nxo_user:find(ID) of
    [U] ->
      nxo_login_delegate:set_user(U, true),
      wf:redirect("/");
    _ ->
      ok
  end;
event({delete, ID}) ->
  wf:wire(#confirm{ text="Do you want to continue?",
                    postback={delete_confirmed, ID} });
event({delete_confirmed, ID}) ->
  nxo_db:query(user_delete, [ID]),
  event(refresh);
event(close_button_legend) ->
  nxo_modal:close().

button(add_directory, Icon, Type) ->
  case nxo_directory:any_defined() of
    true -> #button{ postback=add_directory,
                     body=nxo:fa(Icon),
                     class=button_class(Type) };
    false -> []
  end;
button(Postback, Icon, Type) ->
  #button{ postback=Postback,
           body=nxo:fa(Icon),
           class=button_class(Type) }.

button_class(Type) ->
  "btn btn-sm btn-" ++ wf:to_list(Type).


%% ----- The help legend -----

button_legend() ->
  #panel{
     body=
       [legend_row("plus",        "success", "Add a Local User"),
        legend_row("search-plus", "success", "Add a Directory User"),
        legend_row("info-circle", "primary", "Show User Info"),
        legend_row("edit",        "primary", "Edit User Particulars"),
        legend_row("users",       "primary", "Manage User Groups"),
        legend_row("toggle-on",   "success", "Toggle User Active/Inactive"),
        legend_row("user-secret", "warning", "Masquerade As a User"),
        legend_row("ban",         "danger",  "Delete a User")]}.

legend_row(Icon, Class, Text) ->
  Btn = #button{ class="btn btn-sm btn-" ++ Class, body=nxo:fa(Icon) },
  #panel{ class="row my-2",
          body=[#panel{ class="col-sm-1", body=Btn },
                #panel{ class="col-sm-11", body=Text }] }.
