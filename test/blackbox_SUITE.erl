-module(blackbox_SUITE).

-export([suite/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [].

all() ->
    [ test

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

test(Config) ->
    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

