-module(nxo_settings).
-include_lib("nitrogen_core/include/wf.hrl").
-export([
          set/3
        , set/4
        , get/1
        , get/2
        , set_group_description/2
        , list_groups/0
        , group_settings/1
        , init/0
        ]).

-type group() :: string() | binary() | atom().
-type setting() :: string() | binary() | atom().
-type value() :: any().
-type description() :: string() | binary().
-type group_description() :: map().
-type group_list() :: [ group_description() ].
-type setting_description() :: map().
-type setting_list() :: [ setting_description() ].

-define(DEFAULTS, "settings.yml").

-compile({no_auto_import, [get/1]}).

-spec init() -> ok.
init() ->
  load_defaults().

-spec set(group(), setting(), value()) -> value().
set(Group, Setting, Value) ->
  set(Group, Setting, Value, []).

-spec set(group(), setting(), value(), description()) -> value().
set(Group, Setting, Value, Description) ->
  Params = #{ group => Group, setting => Setting,
              value => Value, desc => Description },
  ?PRINT(Params),
  nxo_db:q(nxo_insert_setting, Params).

get(Setting) when is_atom(Setting) ->
  get(atom_to_list(Setting));
get(Setting) when is_binary(Setting) ->
  get(binary_to_list(Setting));
get(Setting) when is_list(Setting) ->
  [Group, SettingName] = string:split(Setting, "@"),
  get(list_to_atom(Group), list_to_atom(SettingName)).

-spec get(group(), setting()) -> binary() | undefined.
get(Group, Setting) ->
  Params = #{ group => Group, setting => Setting },
  case nxo_db:q(nxo_select_setting_value, Params, scalar) of
    [] ->
      application:get_env(Group, Setting, undefined);
    Value ->
      Value
  end.

-spec set_group_description(group(), description()) -> ok.
set_group_description(Group, Description) ->
  Params = #{ group => Group, label => Description },
  nxo_db:q(nxo_insert_setting_group, Params).

-spec list_groups() -> group_list().
list_groups() ->
  nxo_db:q(nxo_select_setting_groups).

-spec group_settings(group()) -> setting_list().
group_settings(Group) ->
  nxo_db:q(nxo_group_settings, #{ group => Group }, map).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

load_defaults() ->
  Filename = application:get_env(nxo:application(), defaults_file, ?DEFAULTS),
  DefaultsFile = filename:join(code:priv_dir(nxo:application()), Filename),
  case filelib:is_file(DefaultsFile) of
    true ->
      logger:info("NXO: loading defaults file ~s", [DefaultsFile]),
      load_defaults(DefaultsFile);
    false ->
      logger:info("NXO: no defaults file configured")
  end.

load_defaults(DefaultsFile) ->
  P = hd(yamerl:decode_file(DefaultsFile)),
  Groups = proplists:get_value("groups", P, []),
  insert_groups(Groups),
  Settings = proplists:get_value("settings", P, []),
  insert_settings(Settings).

insert_groups(Groups) ->
  lists:foreach(fun({Group, Label}) ->
                    set_group_description(Group, Label)
                end, Groups).

insert_settings(Settings) ->
  lists:foreach(
    fun({Setting, PList}) ->
        [Group, Desc, Value] = [ proplists:get_value(K, PList, "")
                                 || K <- ["group", "desc", "value"] ],
        set(Group, Setting, Value, Desc)
    end, Settings).
