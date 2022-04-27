-module(nxo_realms).
-include("nxo.hrl").
-export([
          all/0
        , all/1
        , realm_map/0
        , is_member_of/2
        , roles_for_realm/1
        , realms_for_role/1
        ]).

%% @doc A list of the realms.
-spec all() -> [ binary() ].
all() ->
  maps:keys(realm_map()).

%% @doc A map of realms and associated roles.
-spec realm_map() -> #{ binary() => [ binary() ] }.
realm_map() ->
  maps:from_list([ {maps:get(<<"realm">>, M), maps:get(<<"groups">>, M)}
                   || M <- nxo_db:q(all_realms) ]).

%% @doc A list of realms the user belongs to.
-spec all(string()) -> boolean().
all(EmailOrID) ->
  Realms = lists:foldl(fun(Role, Acc) ->
                           Realms = realms_for_role(Role),
                           Realms ++ Acc
                       end, [], nxo_roles:all(EmailOrID)),
  lists:usort(Realms).

%% @doc Is the user a member of the realm?
-spec is_member_of(string(), string()) -> boolean().
is_member_of(EmailOrID, Realm) ->
  lists:member(wf:to_binary(Realm), all(EmailOrID)).

%% @doc A list of roles comprising the realm.
-spec roles_for_realm(string()) -> [ binary() ].
roles_for_realm(Realm) ->
  maps:get(wf:to_binary(Realm), realm_map(), []).

%% @doc A list of realms the role participates in.
-spec realms_for_role(string()) -> [ binary() ].
realms_for_role(Role) ->
  R = wf:to_binary(Role),
  [ Realm || {Realm, Roles} <- maps:to_list(realm_map()),
             lists:member(R, Roles) ].
