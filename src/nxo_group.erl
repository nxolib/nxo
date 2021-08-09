-module(nxo_group).
-include("nxo.hrl").
-export([
          all/0
        , find/1
        , ids/0
        , delete_group/1
        , add_to_group/2
        , remove_from_group/2
        ]).

%% @doc Return all groups in the group table.
all() ->
  nxo_db:q(group_all_groups, []).

%% @doc Find a group by name.
find(Name) ->
  nxo_db:q(group_find, [Name]).

%% @doc Return all the group IDs as strings.
ids() ->
  lists:foldl(fun(G, Acc) ->
                  [binary_to_list(maps:get(<<"group_id">>, G)) | Acc]
              end, [], all()).


%% @doc Add a UserID to group.
add_to_group(UserID, GroupName) ->
  nxo_db:query(role_set_has_role, [UserID, GroupName]).

%% @doc Remove a UserID from group.
remove_from_group(UserID, GroupName) ->
  nxo_db:q(role_unset_has_role, [UserID, GroupName], raw).

%% @doc Delete a group specified by name or ID.
delete_group(Name) ->
  nxo_db:q(group_delete, [Name]).
