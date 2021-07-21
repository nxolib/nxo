-module(nxo_template_delegate).
-include("nxo.hrl").
-export([ event/1 ]).

-postback_security({group, administrators}).

%% Allow the UI to recompile the templates.
event(recompile_dtl) ->
  nxo_template:compile_all().
