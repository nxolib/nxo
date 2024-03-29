-module(nxo_login_delegate).
-include("nxo.hrl").
-export([
          event/1
        , set_user/1
        , set_user/2
        ]).

-security(none).
-postback_security(none).

event(login) ->
  [Username, Password] = wf:mq([username, password]),
  case login(Username, Password) of
    {true, UserData} ->
      set_user(UserData),
      wf:redirect_from_login("/");
    false ->
      wf:redirect_to_login("/login")
  end;

event(unmask) ->
  case nxo_user:find(wf:session(pre_masquerade_user)) of
    [UserData] -> set_user(UserData),
                  wf:session(pre_masquerade_user, undefined),
                  wf:redirect("/");
    _ -> ok
  end;

event(logout) ->
  nxo_sessions:kill(wf:user()),
  wf:logout(),
  wf:redirect("/").

set_user(UserData) ->
  set_user(UserData, false).

set_user(UserData, Masquerade) ->
  UserID = maps:get(<<"user_id">>, UserData),
  MaybeUser = wf:user(),
  wf:clear_session(),
  wf:session(display_name, display_name(UserData, Masquerade)),
  case Masquerade of
    true -> wf:session(pre_masquerade_user, MaybeUser);
    false -> ok
  end,
  wf:session(user_data, UserData),
  wf:user(UserID).


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%



display_name(UserData, false) ->
  display_name(UserData);
display_name(UserData, true) ->
  display_name(UserData) ++ " [mask]".

display_name(UserData) ->
  io_lib:format("~s ~s",
                [maps:get(<<"first_name">>, UserData),
                 maps:get(<<"last_name">>, UserData)]).

%% User can be an email address
-spec login(User :: string(), Pass :: string()) -> {true, map()} | false.

login(User, Pass) ->
  case (is_user_active(nxo_user:find(User))) of
    {true, UserData} -> authenticate_user(UserData, Pass);
    _ -> false
  end.

is_user_active([#{<<"active">> := true}=UserData]) ->
  {true, UserData};
is_user_active(_) ->
  false.

authenticate_user(#{ <<"source">> := <<"local">> }=UserData, Pass) ->
  case erlpass:match(Pass, maps:get(<<"password">>, UserData)) of
    true -> successful_audit(UserData);
    false -> failed_audit(UserData, "local password failure")
  end;
authenticate_user(#{ <<"source">> := <<"directory">>,
                     <<"email">> := Email,
                     <<"user_id">> := ID}=UserData, Pass) ->
  case nxo_db:q(user_directory_find, [ID]) of
    [Dir] ->
      OrgAbbrv = maps:get(<<"org_abbrv">>, Dir),
      case nxo_directory:authenticate(Email, Pass, OrgAbbrv) of
        true  -> successful_audit(UserData);
        false -> failed_audit(UserData, "directory password failure")
      end;
    _ ->
      failed_audit(UserData, "organization directory unavailable")
  end.

successful_audit(#{ <<"user_id">> := UserID }=UserData) ->
  nxo:notify(#audit{activity = authentication,
                    user_id = UserID,
                    target = login,
                    result = success}),
  {true, UserData}.

failed_audit(#{ <<"user_id">> := UserID }, Comment) ->
  nxo:notify(#audit{ activity = authentication,
                     user_id = UserID,
                     target = login,
                     result = fail,
                     comment = Comment }),
  false.
