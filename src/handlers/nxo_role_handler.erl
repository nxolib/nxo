-module(nxo_role_handler).
-behaviour(role_handler).
-export([
          init/2
        , finish/2
        , get_has_role/3
        , set_has_role/4
        , get_roles/2
        , clear_all/2
        ]).

init(_Config, State) ->
  {ok, State}.

finish(_Config, State) ->
  {ok, State}.

get_has_role(Role, _Config, _State) ->
  nxo_roles:has_role(wf:user(), Role).

set_has_role(Role, IsInRole, _Config, State) ->
  SQL = case IsInRole of
          true -> role_set_has_role;
          false -> role_unset_has_role
        end,
  nxo_db:query(SQL, [wf:user(), Role]),
  {ok, State}.

get_roles(_Config, _State) ->
  nxo_roles:all(wf:user()).

clear_all(_Config, State) ->
  {ok, State}.
