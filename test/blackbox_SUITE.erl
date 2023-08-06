-module(blackbox_SUITE).

-import(erlbox, [is_success/1]).

-export([suite/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [].

all() -> [test].

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
    X0 = blackbox:tcp("127.0.0.1", 5044, [binary], _Timeout = 5000),

    X1 = fun () -> fun ct:print/1 end,

    X2 = blackbox:udp("127.0.0.1", 5044, [binary]),

    [ begin {ok, Pid} = blackbox:start_child(X, fun () -> ok end, []),

            Res = blackbox:trace(test, [], Pid, <<"ping">>, []),
            Res

      end || X <- [X0, X1, X2]
    ],

    %% TODO Trace action

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

