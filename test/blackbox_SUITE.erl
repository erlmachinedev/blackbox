-module(blackbox_SUITE).

-import(erlbox, [is_success/1]).

-export([suite/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).

-export([start_tcp/1]).
-export([start_udp/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [].

all() ->
    [ start_tcp,
      start_udp
    ].

init_per_suite(Config) ->
    application:ensure_all_started(blackbox),

    Res = Config,
    Res.

end_per_suite(_) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_tcp(Config) ->
    {ok, Pid} = blackbox:start_tcp(_URI = "test:2201"),

    blackbox:trace(Pid, Config),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

start_udp(Config) ->
    {ok, Pid} = blackbox:start_udp(_URI = "test:2201"),

    blackbox:trace(Pid, Config),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

