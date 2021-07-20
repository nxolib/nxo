-module(nxo_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  nxo_db:start(),
  nxo_db:apply_full_ddl(),

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

  case nxo:is_development() of
    true ->
      sync:go();
    false ->
      ok
  end,

  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},

  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
