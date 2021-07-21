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
  nxo_db:map_query(user_all, []).

%% @doc Find a user by email or user_id or samaccountname.
-spec find(EmailOrID :: string()) -> {ok, user_data()} | not_found | err.
find(EmailOrID) ->
  nxo_db:map_query(user_find, [EmailOrID]).

%% @doc Find a user_id by email or user_id or samacountname.
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
  nxo_db:returning_query(user_active_flag, [ID, Flag]),
  case Flag of
    true -> maybe_confirm_account(ID);
    false -> ok
  end;
toggle_active_flag(_, _) ->
  error(invalid_activate_flag).

%% @doc Possibly remove user from pending state.
-spec maybe_confirm_account(string()) -> ok.
maybe_confirm_account(ID) ->
  case nxo_auth_group:remove_from_group(ID, ?ROLE_PENDING) of
    {ok, 0} -> ok;
    {ok, 1} -> nxo_mail:send_to_id(account_confirmed, ID,
                                  #{ subject => "Account Approved" })
  end.

%% @doc Change user password.
-spec change_password(User :: string(), PlainPW :: string()) -> ok | failed.
change_password(User, PlainPW) ->
  HashedPW = erlpass:hash(PlainPW),
  case nxo_db:query(user_set_password, [User, HashedPW]) of
    {ok, 1} -> ok;
    _       -> failed
  end.
