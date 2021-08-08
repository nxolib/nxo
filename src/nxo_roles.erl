-module(nxo_roles).
-include("nxo.hrl").
-export([
          all/1
        , has_role/2
        , has_role/3
        , init/0
        , lookup/1
        , flush/0
        ]).

-define(GLOBAL_ORG, <<"global">>).
-define(CACHE, nxo_role_cahce).
-define(EXPIRE_TIME, 60*60*1000).

all(UserID) ->
  nxo_db:q(all_roles, [UserID]).

has_role(UserID, Role) ->
  RoleSpec = wf:to_binary(Role),
  case binary:matches(RoleSpec, <<"::">>) of
    [] ->
      has_role(UserID, ?GLOBAL_ORG, Role);
    _ ->
      %nxo_db:q(has_role, [UserID, [RoleBin]])
      lookup({UserID, RoleSpec})
  end.

has_role(UserID, Org, Role) ->
  RoleBin = wf:to_binary(Role),
  OrgBin = wf:to_binary(Org),
  RoleSpec = << OrgBin/binary, "::", RoleBin/binary >>,
  lookup({UserID, RoleSpec}).
  %nxo_db:q(has_role, [UserID, [RoleSpec]]).

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
