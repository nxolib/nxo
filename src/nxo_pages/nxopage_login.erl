-module(nxopage_login).
-include("nxo.hrl").
-export([
          main/0
        , title/0
        , body/0
        ]).

-security(none).
-postback_security(none).

main() -> #template{ file=nxo:template("login.html") }.

title() -> "Login".

body() ->
  case wf:path_info() of
    "logout" -> nxo_login_delegate:event(logout);
    "unmask" -> nxo_login_delegate:event(unmask);
    _ -> case wf:user() of
           undefined -> show_content();
           _         -> wf:redirect("/")
         end
  end.

%% INTERNAL FUNCTIONS

show_content() ->
  case wf:session(login_failure) of
    undefined -> ok;
    Message   -> wf:insert_before(defer, login_panel, message_box(Message))
  end,
  #panel{
     id=login_panel,
     class="mt-4 ml-4 form-group",
     body=[
           #panel{ class="row",
                   body=[
                         #panel{
                            class="col-sm-5",
                            body=[ #textbox{ id=username,
                                             placeholder="Username",
                                             next=password,
                                             class="form-control" } ]},
                         #panel{
                            class="col-sm-5",
                            body=[ #password{ id=password,
                                              placeholder="Password",
                                              class="form-control",
                                              delegate=nxo_login_delegate,
                                              postback=login} ]},
                         #panel{
                            class="col-sm-2",
                            body=[ #button{ text="Sign In",
                                            id=login_button,
                                            class="btn btn-block btn-success",
                                            delegate=nxo_login_delegate,
                                            postback=login} ]} ]} ]}.

message_box(Message) ->
  #panel{ class="alert alert-secondary", text=Message }.
