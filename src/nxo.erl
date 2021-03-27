-module(nxo).
-export([
          application/0
        , application/1

        ]).


%% Retrieve the name of the NXO enabled application.
application() ->
  {ok, App} = application:get_env(nxo, application),
  App.

%% Set the name fo the NXO enabled application.
%%
%% This can be set in the config file:
%%   {nxo, [{application, myapp}]}
application(App) ->
  ok = application:set_env(nxo, application, App),
  application().
