-module(nxo_settings).
-export([
          set/3
        , set/4
        , get/2
        , set_group_description/2
        , list_groups/0
        , dump/0
        ]).

-type group() :: string() | binary() | atom().
-type setting() :: string() | binary() | atom().
-type value() :: any().
-type description() :: string() | binary().
-type group_description() :: map().
-type group_list() :: [ group_description() ].

-spec init() -> ok.
init() ->
  create_tables(),
  load_defaults().

-spec set(group(), setting(), value()) -> value().
set(Group, Setting, Value) ->
  set(Group, Setting, Value, []).

-spec set(group(), setting(), value(), description()) -> value().
set(Group, Setting, Value, Description) ->
  ok.

-spec get(group(), setting()) -> {group(), value(), description()} | undefined.
get(Group, Setting) ->
  %% check runtime store
  %% check environment
  ok.

-spec set_group_description(group(), description()) -> ok.
set_group_description(Group, Description) ->
  ok.

-spec list_groups() -> group_list().
list_groups() ->
  ok.

-spec dump() -> any().
dump() ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

create_tables() ->
  ok.

load_defaults() ->
  ok.
