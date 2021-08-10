-module(nxo_group).
-include("nxo.hrl").
-export([
          all/0
        , all_with_role/1
        , find/1
        , delete_group/1
        , add_to_group/2
        , remove_from_group/2
        ]).

%% @doc Return all groups in the group table.
all() ->
  all_with_role("global").

all_with_role(OrgAbbrv) ->
  Groups = [ generate_role(G, OrgAbbrv) || G <- nxo_db:q(group_all_groups) ],
  %% if org is global, all groups; if not, only the non-global_only groups.
  lists:filter(fun(G) ->
                   wf:to_list(OrgAbbrv) == "global"
                     orelse (wf:to_list(OrgAbbrv) =/= "global" andalso
                             not maps:get(<<"global_only">>, G))
               end, Groups).

generate_role(G, OrgAbbrv) ->
  GroupName = maps:get(<<"group_name">>, G),
  GroupBin = wf:to_binary(GroupName),
  OrgBin = wf:to_binary(OrgAbbrv),
  Role = << OrgBin/binary, "::", GroupBin/binary >>,
  maps:put(<<"role">>, Role, G).

%% @doc Find a group by name.
find(Name) ->
  nxo_db:q(group_find, [Name]).

%% @doc Add a UserID to group.
add_to_group(UserID, GroupName) ->
  nxo_db:query(role_set_has_role, [UserID, GroupName]).

%% @doc Remove a UserID from group.
remove_from_group(UserID, GroupName) ->
  nxo_db:q(role_unset_has_role, [UserID, GroupName], raw).

%% @doc Delete a group specified by name or ID.
delete_group(Name) ->
  nxo_db:q(group_delete, [Name]).
