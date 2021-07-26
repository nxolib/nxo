-module(nxo_sup).
-behaviour(supervisor).
-include("nxo.hrl").
-export([init/1, ping_db/0, start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  nxo_db:start(),
  nxo_db:apply_full_ddl(),
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
                , bcrypt
                , erlpass
                ]),

  case nxo:is_development() of
    true -> sync:go();
    false -> ok
  end,

  %% a db keep-alive process
  spawn(?MODULE, ping_db, []),

  %% initialize the caches
  nxo_template_name_cache:init(),

  %% manage event handlers
  start_event_handler(),
  nxo:add_handler(nxo_audit_handler),

  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},

  {ok, {SupFlags, []}}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%
start_event_handler() ->
  {ok, Pid} = gen_event:start_link(),
  nprocreg:register_pid(?EVENT, Pid).

ping_db() ->
  timer:sleep(10 * 60 * 1000),
  nxo_db:q(ping, []),
  ping_db().
