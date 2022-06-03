-module(nxo_template_name_cache).
-export([
          init/0
        , lookup/1
        , flush/0
        ]).

-define(CACHE, nxo_template_name_cache).
-define(EXPIRE_TIME, 24 * 60 * 60 * 1000).


init() ->
  nitro_cache:init(?CACHE).

flush() ->
  nitro_cache:flush(?CACHE).

lookup(Template) ->
  nitro_cache:get(?CACHE, ?EXPIRE_TIME, Template,
                  fun() -> find_template(Template) end).


find_template(Template) when is_binary(Template); is_atom(Template) ->
  find_template(wf:to_list(Template));
find_template(Template) when is_list(Template) ->
  Filename = case lists:suffix(".html", Template) of
               true -> Template;
               false -> Template ++ ".html"
             end,
  find_template(filename:join(["**", Filename]), html_dirs()).

find_template(Template, []) ->
  error("Template " ++ Template ++ " not found.");
find_template(Template, [H|T]) ->
  case filelib:wildcard(Template, H) of
    [X | _] -> filename:join([H, X]);
    []      -> find_template(Template, T)
  end.

html_dirs() ->
  case application:get_env(nxo, html_template_dir) of
    undefined ->
      error('html_template_dir not defined');
    {ok, Paths} ->
      parse_paths(lists:reverse(Paths), [])
  end.

parse_paths([], Acc) ->
  lists:reverse(Acc);
parse_paths([{priv_dir, App, SubDir}|T], Acc) ->
  Path = filename:join(code:priv_dir(App), SubDir),
  parse_paths(T, [Path | Acc]);
parse_paths([{path, Path}|T], Acc) ->
  parse_paths(T, [Path | Acc]).
