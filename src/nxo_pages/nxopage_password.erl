-module(nxopage_password).
-include("nxo.hrl").
-export([
          event/1
        , main/0
        , title/0
        , body/0
        , password_check/2
        ]).

-security(authenticated).
-postback_security(authenticated).

main() ->
  case nxo_user:is_directory_user(wf:user()) of
    true  -> wf:redirect("/");
    false -> #template{ file=nxo:template("password.html") }
  end.

title() -> "Password Change".

body() ->
  password_validation(),
  [ #panel{id=result},
    #panel{
       class="form-group pt-5",
       body=[#label{
                class="vert_form_label",
                id="current_label",
                text="Current Password"},
             #password{
                class="form-control",
                next=new_password_1,
                id=current_password}]},
    #panel{
       class="form-group",
       body=[#label{
                class="vert_form_label",
                id="label_one",
                text="New Password"},
             #password{
                class="form-control",
                next=new_password_2,
                id=new_password_1}]},
    #panel{
       class="form-group",
       body=[#label{
                class="vert_form_label",
                id="label_two",
                text="Confirm New Password "},
             #password{
                class="form-control",
                id=new_password_2}]},
    #button{
       class="btn btn-primary btn-small",
       id=user_submit,
       postback=change_password,
       text="Change Password"}
  ].

event(_) ->
  case nxo_user:change_password(wf:user(), wf:q(new_password_1)) of
    ok ->
      wf:update(result, #span{class="p-3 mb-2 bg-success text-white",
                              text="Password changed."});
    failed ->
      wf:update(result, #span{class="p-3 mb-2 bg-danger text-white",
                              text="Failed to change password."})
  end.


min_length_text() ->
  Length = nxo_settings:get(user, length_words),
  iolist_to_binary(["Passwords must be ", Length, " characters."]).

min_length() ->
  wf:to_integer(nxo_settings:get(user, password_length)).

password_validation() ->
  wf:defer(user_submit, current_password,
           #validate{
              attach_to=current_label,
              validators=[#is_required{text="Required"},
                          #custom{text="Password is incorrect",
                                  function=fun ?MODULE:password_check/2}]}),
  wf:defer(user_submit, new_password_1,
           #validate{
              attach_to=label_one,
              validators=[#is_required{text="Required"},
                          #min_length{text=min_length_text(),
                                      length=min_length()}]}),
  wf:defer(user_submit, new_password_2,
           #validate{
              attach_to=label_two,
              validators=[#confirm_password{text="Passwords must match",
                                            password=new_password_1}]}).

password_check(_Tag, CurrentPassword) ->
  DBPassword = nxo_db:scalar_query(user_get_password, [wf:user()]),
  erlpass:match(CurrentPassword, DBPassword).
