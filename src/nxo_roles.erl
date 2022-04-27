-module(nxo_roles).
-include("nxo.hrl").
-export([
          all/1
        , has_role/2
        , has_role/3
        , add_role/2
        , delete_role/2
        , init/0
        , lookup/1
        , flush/0
        ]).

-define(GLOBAL_ORG, <<"global">>).
-define(CACHE, nxo_role_cahce).
-define(EXPIRE_TIME, 60*60*1000).

%% @doc Return a list of roles the specified user assumes.
-spec all(EmailOrID :: string()) -> [ binary() ].
all(EmailOrID) ->
  UserID = nxo_user:id(EmailOrID),
  nxo_db:q(all_roles, [UserID]).

has_role(undefined, _) ->
  false;  %% wf:user() returns undefined when not logged in.
has_role(UserID, Role) ->
  RoleSpec = wf:to_binary(Role),
  case binary:matches(RoleSpec, <<"::">>) of
    [] ->
      has_role(UserID, ?GLOBAL_ORG, Role);
    _ ->
      lookup({UserID, RoleSpec})
  end.

has_role(undefined, _, _) ->
  false;
has_role(UserID, Org, Role) ->
  RoleBin = wf:to_binary(Role),
  OrgBin = wf:to_binary(Org),
  RoleSpec = << OrgBin/binary, "::", RoleBin/binary >>,
  lookup({UserID, RoleSpec}).

add_role(UserID, Role) ->
  [ok, Res] = nxo_db:q(user_add_role, [UserID, wf:to_binary(Role)]),
  flush(),
  maps:get(count, Res) == 1.

delete_role(UserID, Role) ->
  [ok, Res] = nxo_db:q(user_delete_role, [UserID, wf:to_binary(Role)]),
  flush(),
  maps:get(count, Res) == 1.


%%%%%%%%%%%%%%%%%%%%%%%
%% CACHING FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  nitro_cache:init(?CACHE).

lookup({UserID, RoleSpec}=Key) ->
  nitro_cache:get(?CACHE, ?EXPIRE_TIME, Key,
                  fun() -> nxo_db:q(has_role, [UserID, [RoleSpec]]) end).

flush() ->
  %% Yes, this flushes the entire cache.  Not great if roles are
  %% changing a lot but probably not a show stopper here.
  nitro_cache:flush(?CACHE).
