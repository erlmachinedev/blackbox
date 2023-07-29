-module(blackbox_app).

-export([start/2]).
-export([stop/1]).

-behaviour(application).

start(_Type, _Args) ->
    blackbox_sup:start_link().

stop(_State) ->
    ok.
