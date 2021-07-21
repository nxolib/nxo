-module(nxo_sql_cache).
-include("nxo.hrl").

-export([
          init/0
        , lookup/1
        , flush/0
        , dump/0
        ]).

-define(CACHE, nxo_sql_cache).
-define(EXPIRE_TIME, 24 * 60 * 60 * 1000).

init() ->
  nitro_cache:init(?CACHE).

flush() ->
  nitro_cache:flush(?CACHE).

lookup(Template) ->
  case eqlite:get_query(wf:to_atom(Template)) of
    undefined ->
      nitro_cache:get(?CACHE, ?EXPIRE_TIME, Template,
                      fun() -> find_template(Template) end);
    Query -> Query
  end.

dump() ->
  not_implemented.

find_template(Template) ->
  Pattern = filename:join(["**", wf:to_list(Template) ++ ?SQL_EXT]),
  Filename =
    case filelib:wildcard(Pattern, ?SQL_DIR) of
      [] -> error("SQL Template " ++ wf:to_list(Template) ++ " not found");
      [X | _] -> filename:join([?SQL_DIR, X])
    end,
  {ok, SQL} = file:read_file(Filename),
  SQL.
