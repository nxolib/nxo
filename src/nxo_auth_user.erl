-module(nxo_auth_user).
-include("nxo.hrl").
-export([
          all_users/0
        , find/1
        , id/1
        , toggle_active_flag/2
        , maybe_confirm_account/1
        , is_ad/1
        , change_password/2
        ]).

-type user_data() :: map().

%% @doc Return all users in the users table.
-spec all_users() -> [user_data()].
all_users() ->
  nxo_db:q(user_all).

%% @doc Find a user by email or user_id or samaccountname.
-spec find(EmailOrID :: string()) -> [user_data()] | [].
find(EmailOrID) ->
  nxo_db:q(user_find, [EmailOrID]).

%% @doc Find a user_id by email or user_id or samacountname.
-spec id(EmailOrID :: string()) -> binary() | undefined.
id(EmailOrID) ->
  case find(EmailOrID) of
    [User] -> maps:get(<<"user_id">>, User);
    _      -> undefined
  end.

%% @doc Determine if a user is a local or AD account.
-spec is_ad(binary()) -> boolean().
is_ad(UserID) ->
  [UserData] = find(UserID),
  case maps:get(<<"samaccountname">>, UserData, null) of
    null -> false;
    <<>> -> false;
    _    -> true
  end.

%% @doc Toggle a user active flag on (true) or off (false).
-spec toggle_active_flag(string(), boolean()) -> binary().
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
-spec maybe_confirm_account(string()) -> ok.
maybe_confirm_account(ID) ->
  case nxo_auth_group:remove_from_group(ID, pending) of
    {ok, 0} -> ok;
    {ok, 1} -> nxo:notify({account_confirmed, ID})
  end.

%% @doc Change user password.
-spec change_password(User :: string(), PlainPW :: string()) -> ok | failed.
change_password(User, PlainPW) ->
  HashedPW = erlpass:hash(PlainPW),
  case nxo_db:q(user_set_password, [User, HashedPW], raw) of
    {ok, 1} -> nxo:notify({password_changed, User});
    _       -> failed
  end.
