-module(nxo_authz).
-include("nxo.hrl").

-export([
          may/1
        , is/1
        , is_pending/1
        , init/0
        ]).

-define(DEFAULTS, "authz_default.yml").

init() ->
  load_defaults().

may(RealmOrGroup) ->
  wf:role(RealmOrGroup).

is(RealmOrGroup) ->
  wf:role(RealmOrGroup).

%% We're asking about a 3rd party ID here, so wf:role/1
%% won't work.
is_pending(ID) ->
  case nxo_db:scalar_query(user_is_pending, [ID]) of
    0 -> false;
    _ -> true
  end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

load_defaults() ->
  Filename = application:get_env(nxo:application(), authz_defaults, ?DEFAULTS),
  DefaultsFile = filename:join(code:priv_dir(nxo:application()), Filename),
  case filelib:is_file(DefaultsFile) of
    true ->
      logger:info("NXO: loading authz_defaults file ~s", [DefaultsFile]),
      load_defaults(DefaultsFile);
    false ->
      logger:info("NXO: no authz_defaults file available.")
  end.

load_defaults(DefaultsFile) ->
  Realms = proplists:get_value("realms", hd(yamerl:decode_file(DefaultsFile))),
  lists:map(fun insert_realm/1, Realms).

%% NB: non-existent groups will be silently skipped!
insert_realm({Realm, PList}) ->
  Desc = proplists:get_value("desc", PList),
  Required = proplists:get_value("required", PList, false),
  Groups = lists:filter(fun([]) -> false;
                           (_)    -> true end,
                        [ nxo_db:q(group_id, [G]) ||
                          G <- proplists:get_value("groups", PList, []) ]),
  nxo_db:q(insert_realm, [Realm, Desc, Required, Groups]).
