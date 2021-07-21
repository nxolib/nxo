-module(nxo_auth_group).
-include("nxo.hrl").
-export([
          all/0
        , find_group/1
        , ids/0
        , delete_group/1
        , add_to_group/2
        , remove_from_group/2
        ]).

-type group_data() :: map().


%% @doc Return all groups in the group table.
-spec all() -> [group_data()].
all() ->
  nxo_db:map_query(group_all_groups, []).

%% @doc Find a group by name or ID.
-spec find_group(NameOrID :: string()) -> {ok, group_data()} | not_found | err.
find_group(NameOrID) ->
  nxo_db:map_query(group_find, [NameOrID]).

%% @doc Return all the group IDs as strings.
-spec ids() -> [string()].
ids() ->
  lists:foldl(fun(G, Acc) ->
                  [binary_to_list(maps:get(<<"group_id">>, G)) | Acc]
              end, [], all()).


%% @doc Add a UserID to group.
-spec add_to_group(UserID :: uuid:uuid_string(), GroupName :: string()) -> ok.
add_to_group(UserID, GroupName) ->
  nxo_db:query(role_set_has_role, [UserID, GroupName]).

%% @doc Remove a UserID from group.
-spec remove_from_group(uuid:uuid_string(), string()) -> ok.
remove_from_group(UserID, GroupName) ->
  nxo_db:q(role_unset_has_role, [UserID, GroupName], raw).

%% @doc Delete a group specified by name or ID.
-spec delete_group(NameOrID :: string()) -> {ok, 1} | not_found.
delete_group(NameOrID) ->
  case find_group(NameOrID) of
    [#{<<"group_id">> := ID}] -> nxo_db:q(group_delete, [ID], raw);
    _ -> not_found
  end.
