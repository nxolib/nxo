-module(nxo_api).
-include("nxo.hrl").
-export([
          user_key/1
        , change_key/1
        , login/1
        , log/2
        ]).

user_key(UserID) ->
  case nxo_db:scalar_query(get_api_key, [UserID]) of
    []  -> change_key(UserID);
    Key -> Key
  end.

change_key(UserID) ->
  Key = uuid:to_string(uuid:uuid4()),
  nxo_db:query(set_api_key, [UserID, Key]),
  nxo:notify(#audit{activity=api_key_change, user_id=UserID, result=success}),
  Key.

login(APIKey) ->
  case nxo_db:scalar_query(is_api_key, [APIKey]) of
    [] -> false;
    UserID ->
      [UserData] = nxo_user:find(UserID),
      nxo_login_delegate:set_user(UserData),
      true
  end.

log(Action, Path) ->
  nxo_db:query(api_log, [wf:user(), Action, Path]).
