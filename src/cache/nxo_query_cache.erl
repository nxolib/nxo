-module(nxo_query_cache).
-include("nxo.hrl").
-export([
          init/0
        , lookup/1
        , clear/0
        , stash/2
        ]).

-define(CACHE, nxo_sql_cache).
-define(EXPIRE_TIME, 60 * 60 * 1000).

init() ->
  nitro_cache:init(?CACHE).

lookup(Key) ->
  nitro_cache:get(?CACHE, ?EXPIRE_TIME, Key, fun() -> undefined end).

stash(Key, Value) ->
  nitro_cache:get(?CACHE, ?EXPIRE_TIME, Key, fun() -> Value end).

clear() ->
  nitro_cache:flush(?CACHE).
