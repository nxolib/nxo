-module(nxo_db_delegate).
-include("nxo.hrl").
-export([ event/1 ]).

-postback_security({group, administrators}).

%% These postbacks power the buttons in the development-only footer.

event(reload_ddl) ->
  nxo_db:apply_full_ddl(),
  restart_pgpool(),
  nxo_view:reload_page();

event(clear_sql_cache) ->
  nxo_db:query_refresh(),
  restart_pgpool(),
  nxo_view:reload_page().

restart_pgpool() ->
  pgpool:stop(),
  pgpool:start().
