%% -*- mode: erlang -*-
-module(nitrogen_main_handler).
-export([
         run/0,
         ws_init/0
        ]).
-include("nxo.hrl").

handlers() ->
%%  nitrogen:handler(nxo_role_handler, []),
  nitrogen:handler(nxo_route_handler, []),
%%  nitrogen:handler(nxo_postback_handler, []),
%%  nitrogen:handler(nxo_security_handler, []),

  case nxo:is_development() of
    true -> nitrogen:handler(debug_crash_handler, []);
    false -> ok
  end,

  try apply(nxo:application(), handlers, []) of
    _ -> ok
  catch
    _ -> ok
  end,
  ok.

ws_init() ->
  handlers().

run() ->
  handlers(),
  wf_core:run().
