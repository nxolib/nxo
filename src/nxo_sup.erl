-module(nxo_sup).
-behaviour(supervisor).
-include("nxo.hrl").
-export([init/1, start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  nxo_template:compile_all(),
  nxo_settings:init(),

  %% We're only allowed one document_root yet we might need to serve
  %% up files from priv_dir(nxo)/nxostatic or priv_dir(app)/static.
  %% The known common directory in both cases is the root dir.  Set
  %% that but know that the file path will be checked against these
  %% two directories; other paths (to arbitrary files in the system)
  %% will be not found.
  application:set_env(simple_bridge, document_root, "/"),

  lists:foreach(fun(App) -> ok = application:ensure_started(App) end,
                [ crypto
                , nitro_cache
                , nprocreg
                , simple_bridge
                ]),

  %% initialize the caches
  nxo_template_name_cache:init(),
  nxo_roles:init(),

  %% manage event handlers
  start_event_handler(),
  nxo_event:add_handler(nxo_audit_handler),

  case nxo:is_development() of
    true ->
      nxo_event:add_handler(nxo_development_handler),
      sync:go();
    false -> ok
  end,

  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, []}}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%
start_event_handler() ->
  {ok, Pid} = gen_event:start_link(),
  nprocreg:register_pid(?EVENT, Pid).
