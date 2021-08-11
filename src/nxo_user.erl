-module(nxo_user).
-include("nxo.hrl").
-export([
          all/0
        , find/1
        , id/1
        , toggle_active_flag/2
        , maybe_confirm_account/1
        , is_ad/1
        , change_password/2
        ]).

%% @doc Return all users in the users table.
all() ->
  nxo_db:q(user_all).

%% @doc Find a user by email or user_id or samaccountname.
find(EmailOrID) ->
  nxo_db:q(user_find, [EmailOrID]).

%% @doc Find a user_id by email or user_id or samacountname.
id(EmailOrID) ->
  case find(EmailOrID) of
    [User] -> maps:get(<<"user_id">>, User);
    _      -> undefined
  end.

%% @doc Determine if a user is a local or AD account.
is_ad(UserID) ->
  [UserData] = find(UserID),
  case maps:get(<<"samaccountname">>, UserData, null) of
    <<>> -> false;
    _    -> true
  end.

%% @doc Toggle a user active flag on (true) or off (false).
toggle_active_flag(ID, Flag) when Flag == true;
                                  Flag == false ->
  nxo_db:q(user_active_flag, [ID, Flag]),
  case Flag of
    true -> maybe_confirm_account(ID);
    false -> ok
  end;
toggle_active_flag(_, _) ->
  error(invalid_activate_flag).

%% @doc Possibly remove user from pending state.
maybe_confirm_account(ID) ->
  case nxo_roles:delete_role(ID, <<"global::pending">>) of
    false -> ok;
    true  -> nxo:notify({account_confirmed, ID})
  end.

%% @doc Change user password.
change_password(User, PlainPW) ->
  HashedPW = erlpass:hash(PlainPW),
  case nxo_db:q(user_set_password, [User, HashedPW], raw) of
    {ok, 1} -> nxo:notify({password_changed, User});
    _       -> failed
  end.
