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
    bootstrap(),

    Res = Config,
    Res.

end_per_suite(_) ->
    shutdown().

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test(Config) ->
    X0 = blackbox:tcp("127.0.0.1", 5044, [binary], _Timeout = 5000),

    X1 = fun () -> fun ct:print/1 end,

    X2 = blackbox:udp("127.0.0.1", 5044, [binary]),

    [ begin Modules = blackbox:modules(),

            {ok, Pid} = blackbox:start_child(Modules, X, fun () -> ok end, []),

            Res = blackbox:trace(test, [], Pid, <<"ping">>, []),
            Res

      end || X <- [X0, X1, X2]
    ],

    blackbox_ct:test([]),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------


bootstrap() ->
    Mod = blackbox,

    meck:new(Mod, [passthrough, no_link]),

    Fun = modules,

    meck:expect(Mod, Fun, [], [blackbox_ct]),

    application:ensure_all_started(blackbox).

shutdown() ->
    meck:unload(blackbox),

    application:stop(blackbox).
