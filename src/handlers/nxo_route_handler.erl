-module(nxo_route_handler).
-behaviour(route_handler).
-include("nxo.hrl").

-export([
         init/2,
         finish/2
        ]).

-define(NXO_MODULE_PREFIX,
        application:get_env(nxo, nxo_module_prefix, nxopage)).
-define(NXO_STATIC_ASSETS,
        filename:join(code:priv_dir(nxo),
                      application:get_env(nxo, nxo_static_assets, nxostatic))).
-define(APPLICAION_STATIC_ASSETS,
        filename:join(code:priv_dir(nxo:application()),
                      application:get_env(nxo:application(),
                                          static_assets, "static"))).

init(Config, State) ->
  Bridge = wf_context:bridge(),
  Path = sbw:path(Bridge),
  case string:prefix(Path, "/api/") of
    nomatch ->
      default_init(Config, State, Path);
    APIReq ->
      do_api_init(Config, State, APIReq)
  end.

finish(_Config, State) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%% STANDARD ROUTING %%
%%%%%%%%%%%%%%%%%%%%%%

default_init(_Config, State, Path) ->
  %% Convert the path to a module. If there are no routes defined,
  %% then just convert everything without an extension to a module.
  %% Otherwise, look through all routes for the first matching route.

  %% NXO augments the default handler by checking a second prefix (the
  %% ?NXO_MODULE_PREFIX defined above) if the original route search
  %% yielded no results.  I think the depth first approach here is
  %% correct.

  ModulePrefix = wf:config_default(module_prefix, ""),
  {Module, PathInfo} = route(ModulePrefix, Path),

  {Module1, PathInfo1} =
    case {Module, code:ensure_loaded(Module)} of
      {static_file, _}      -> {Module, PathInfo};
      {_, {module, Module}} -> {Module, PathInfo};
      {_, {error, _}} ->
        {NXOModule, NXOPathInfo} = route(wf:to_list(?NXO_MODULE_PREFIX), Path),
        check_for_404(NXOModule, NXOPathInfo, Path)
    end,

  wf_context:page_module(Module1),
  wf_context:path_info(PathInfo1),
  {ok, State}.


serve_static_file(Path) ->
  CleanPath = string:trim(Path, leading, "/"),
  ApplicationPath = filename:join(?APPLICAION_STATIC_ASSETS, CleanPath),
  case filelib:is_file(ApplicationPath) of
    true ->
      {static_file, ApplicationPath};
    false ->
      NXOPath = filename:join(?NXO_STATIC_ASSETS, CleanPath),
      case filelib:is_file(NXOPath) of
        true ->
          {static_file, NXOPath};
        false ->
          {static_file, "$not_found"}
      end
  end.
%%  {static_file, Path}.

%% First, check if this is a request for the root path. If so then
%% just send it to index.  Check if there is an extension, if so, it's
%% static.  Otherwise, try to load a module according to each part of
%% the path.  First, cycle through code:all_loaded(). If not there,
%% then check erl_prim_loader:get_file() If still not there, then 404.
route(ModulePrefix, "/") ->
  {list_to_atom(module_name(ModulePrefix, ["index"])), []};

route(ModulePrefix, Path) ->
  IsStatic = (filename:extension(Path) /= []),
  case IsStatic of
    true ->
      serve_static_file(Path);

    false ->
      Path1 = string:strip(Path, both, $/),
      Tokens = string:tokens(Path1, "/"),
      %% Check for a loaded module. If not found, then try to load it.
      case try_load_module(ModulePrefix, Tokens) of
        {Module, PathInfo} ->
          {Module, PathInfo};
        undefined ->
          {web_404, Path1}
      end
  end.

module_name(ModulePrefix, Tokens) ->
%%  ModulePrefix = wf:config_default(module_prefix, ""),
  AllTokens = case ModulePrefix of
                "" -> Tokens;
                _ -> [ ModulePrefix | Tokens ]
              end,
  _ModuleName = string:join(AllTokens, "_").

try_load_module(ModulePrefix, Tokens) ->
  try_load_module(ModulePrefix, Tokens, []).
try_load_module(_ModulePrefix, [], _ExtraTokens) -> undefined;
try_load_module(ModulePrefix, Tokens, ExtraTokens) ->
  ModuleName = module_name(ModulePrefix, Tokens),
  Module = try
             list_to_existing_atom(ModuleName)
           catch _:_ ->
               case erl_prim_loader:get_file(ModuleName ++ ".beam") of
                 {ok, _, _} -> list_to_atom(ModuleName);
                 _ -> list_to_atom("$not_found")
               end
           end,

  %% Load the module, check if it exports the right method...
  code:ensure_loaded(Module),
  case erlang:function_exported(Module, main, 0) of
    true ->
      PathInfo = string:join(ExtraTokens, "/"),
      {Module, PathInfo};
    false ->
      next_try_load_module(ModulePrefix, Tokens, ExtraTokens)
  end.


next_try_load_module(ModulePrefix, Tokens, ExtraTokens) ->
  Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
  ExtraTokens1 = [hd(lists:reverse(Tokens))|ExtraTokens],
  try_load_module(ModulePrefix, Tokens1, ExtraTokens1).

check_for_404(static_file, _PathInfo, Path) ->
  {static_file, Path};

check_for_404(Module, PathInfo, Path) ->
  %% Make sure the requested module is loaded. If it is not, then try
  %% to load the web_404 page. If that is not available, then default
  %% to the 'file_not_found_page' module.
  case code:ensure_loaded(Module) of
    {module, Module} -> {Module, PathInfo};
    _ ->
      case code:ensure_loaded(web_404) of
        {module, web_404} -> {web_404, Path};
        _ -> {file_not_found_page, Path}
      end
  end.


%%%%%%%%%%%%%%%%%
%% API ROUTING %%
%%%%%%%%%%%%%%%%%
do_api_init(Config, State, APIReq) ->
  [Version, Action | Info] = string:split(APIReq, "/", all),
  Token = wf:header(authorization),
  case is_valid_token(Token) of
    {true, UserData} ->
      Module = wf:to_atom(string:join(["api", Version, Action], "_")),
      case is_rest_module(Module) of
        true  ->
          case is_valid_content_type() of
            true -> dispatch_to_rest(Module, Info, State, UserData);
            false -> do_api_error(not_acceptable, Config, State,
                                  "invalid or missing content-type")
          end;
        false -> do_api_error(no_resource, Config, State, "invalid resource")
      end;
    false ->
      do_api_error(not_authorized, Config, State, "invalid token")
  end.


dispatch_to_rest(Module, Info, State, UserData) ->
  Entry = fun() -> nitrogen_rest:handle_request(Module) end,
  PathInfo = string:join(Info, "/"),
  wf_context:page_module(Module),
  wf_context:path_info(PathInfo),
  wf_context:entry_point(Entry),
  set_session_info([org_abbrv, org_id, org_name, user_id], UserData),
  nxo_api:log(Module, PathInfo),
  {ok, State}.


do_api_error(ErrorFn, _Config, State, Msg) ->
  wf_context:page_module(api_v1_error),
  wf_context:path_info(Msg),
  wf_context:entry_point(ErrorFn),
  {ok, State}.


is_valid_token(_Token) -> unimplemented.
%% try string:split(Token, ":") of
%%   [APIKey, OrgAbbrv] -> nxo_api:login(APIKey, OrgAbbrv)
%% catch
%%   _:_ -> false
%% end.


set_session_info(KeyList, Data) ->
  lists:foreach(
    fun(Key) ->
        Value = maps:get(atom_to_binary(Key, latin1), Data),
        wf:session(Key, Value) end,
    KeyList).

is_rest_module(Module) ->
  wf_utils:has_behaviour(Module, nitrogen_rest).

is_valid_content_type() ->
  lists:member(wf:header('content-type'), ?API_CONTENT_TYPES).
