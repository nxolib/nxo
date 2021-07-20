-module(nxo_view).
-include("nxo.hrl").
-export([ reload_page/0
        , report_validation_failure/0
        ]).

-export([ header/0
        , footer/0
        , menu/0
        , development_warning/0
        , development_watermark/0
        , database_development/0
        , login/0
        , admin_menu_list/0
        , account_menu/0
        ]).

%% @doc Reload the current page.  This is a side effect only function.
-spec reload_page() -> ok.
reload_page() ->
  try wf:redirect(wf:uri()) of
      _ -> ok
  catch
    _:_ -> ok
  end.

%% @doc JS to execute if there's a validation failure.
%%
%% Specifically, pop up a little alert and scroll to the top of the
%% page.  This is a side effect only function.
-spec report_validation_failure() -> ok.
report_validation_failure() ->
  wf:wire(#script{ script="window.scrollTo(0,0)" }),
  wf:wire(#alert{ text="There are invalid fields." }).


%% XXX: This could use a refactor.

%% @doc Provide the standard header.
header() -> #template{ file=nxo:template("header.html") }.

%% @doc Provide the standard footer.
footer() ->
  #template{ file=nxo:template("footer.html") }.


%% @doc Provide the menu.
menu() ->
  #template{ file=nxo:template("menu.html") }.


%% @doc Change the background color of the footer bar in the dev env.
development_watermark() ->
  case nxo:is_development() of
    true  -> ".footer, .navbar { background: #7CC1D9; }";
    false -> []
  end.

%% @doc Insert a development footer in the dev env.
development_warning() ->
  case nxo:is_development() of
    true  -> "DEVELOPMENT ONLY";
    false -> []
  end.

%% @doc Insert some database helpers.
database_development() ->
  case nxo:is_development() of
    false -> [];
    true  ->
      [#button{text="Recompile DTL",
               postback=recompile_dtl,
               delegate=nxo_template_delegate,
               class="btn btn-sm btn-warning mx-1 mb-1"},
       #button{text="Reload DDL",
               postback=reload_ddl,
               delegate=nxo_db_delegate,
               class="btn btn-sm btn-warning mx-1 mb-1"},
       #button{text="Clear SQL Cache",
               postback=clear_sql_cache,
               delegate=nxo_db_delegate,
               class="btn btn-sm btn-warning mx-1 mb-1"}]
  end.


%%%%%%%%%%%%%%%%%%%%%%
%% LOGIN FORM LOGIC %%
%%%%%%%%%%%%%%%%%%%%%%

%% @doc The user/pass box in the header is generated from the
%% login/0,1 functions.  If
%%   -- the path is /login, don't show anything
%%   -- the user is logged in, show the logout button
%%   -- otherwise, show the login form.
login() ->
  case wf:path() =:= "/login"
    orelse wf:path() =:= "/duoweb_login"
    orelse wf:page_module() =:= page_login
    orelse wf:page_module() =:= page_request_access
  of
    true -> [];
    false -> login(wf:user())
  end.

login(undefined) ->
  #panel{
     id=menu_login_form,
     class="navbar-collapse navbar-form navbar-right",
     body=[ #panel{ class="form-inline my-2 my-lg-0",
                    body=[ #textbox{ id=username,
                                     placeholder="Username",
                                     class="form-control-sm mr-sm-2" },
                           #password{ id=password,
                                      placeholder="Password",
                                      class="form-control-sm mr-sm-2",
                                      postback=login,
                                      delegate=page_login
                                    },
                           #button{ text="Sign In",
                                    class="btn btn-sm btn-light",
                                    postback=login,
                                    delegate=page_login } ]} ]};
login(_User) ->
  #listitem{
     class="nav-item dropdown",
     body=[#link{class="nav-link dropdown-toggle",
                 data_fields=[{toggle,dropdown}],
                 id=userDD,
                 html_id=user_dd,
                 url="#",
                 text=wf:session(display_name)},
           #panel{class="dropdown-menu dropdown-menu-right",
                  body=[user_menu(unmask),
                        user_menu(api),
                        user_menu(passwd),
                        user_menu(logout)]}]}.


%%%%%%%%%%%%%%%%%%%%%
%% MENU ITEM LOGIC %%
%%%%%%%%%%%%%%%%%%%%%

account_menu() ->
  case wf:user() of
    undefined -> [];
    _ -> user_menu()
  end.

user_menu() -> [].


user_menu(unmask) ->
  case wf:session_default(pre_masquerade_user, undefined) of
    undefined -> [];
    _ -> menu_item("/login/unmask", "Unmask")
  end;
user_menu(passwd) ->
  case nxo_auth_user:is_ad(wf:user()) of
    true  -> [];
    false -> menu_item("/password", "Change Password")
  end;
user_menu(api) ->
  case nxo_authz:may(api) of
    false -> [];
    true -> menu_item("/api", "API Token Management")
  end;
user_menu(logout) ->
  menu_item("/login/logout", "Logout").

%% @doc Decide what to show on the admin menu.
admin_menu_list() ->
  case nxo_authz:may(admin_something) of
    false -> [];
    true  -> admin_menu()
  end.

admin_menu() ->
  #listitem{class="nav-item dropdown ml-auto",
            body=[#link{class="nav-link dropdown-toggle",
                        data_fields=[{toggle,dropdown}],
                        id=adminDD,
                        html_id=admin_dd,
                        url="#",
                        text="Administration"},
                  #panel{class="dropdown-menu dropdown-menu-right",
                         body=[
                               admin_menu(app_settings),
                               admin_menu(user_management),
                               admin_menu(org_management),
                               admin_menu(group_management)]}]}.

admin_menu(app_settings) ->
  case nxo_authz:may(admin_everything) of
    false -> [];
    true -> [menu_item("/settings", "Registry Settings"),
             menu_item("/settings_audit", "Settings Audit Data"),
             menu_item("/mailcheck", "Test Email Connectivity"),
             rule()]
  end;
admin_menu(user_management) ->
  case nxo_authz:may(admin_users) of
    false -> [];
    true  -> [menu_item("/users", "User Management"),
              menu_item("/login_audit", "Login Audit Data"),
              rule()]
  end;
admin_menu(org_management) ->
  case nxo_authz:may(admin_users) of
    false -> [];
    true  -> [menu_item("/organization", "Organization Management"),
              rule()]
  end;
admin_menu(group_management) ->
  case nxo_authz:may(admin_everything) of
    false -> [];
    true  -> [menu_item("/groups", "Group Management"), rule()]
  end;
admin_menu(data_management) ->
  case nxo_authz:may(admin_data) of
    false -> [];
    true  -> [menu_item("/field_auth", "Field Authorizations"),
              menu_item("/function_authz", "Function Authorizations")]
  end.

menu_item(URL, Label) ->
  #link{class="dropdown-item", url=URL, text=Label}.

rule() -> #panel{class="dropdown-divider"}.
