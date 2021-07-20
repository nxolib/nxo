%% -*- mode: erlang -*-

-module(nxopage_index).
-include("nxo.hrl").
-export([main/0, title/0, body/0]).

-security(none).
-postback_security(none).

main() -> #template { file=nxo:template("index.html") }.

title() -> "Welcome to NXO".

body() -> "".
