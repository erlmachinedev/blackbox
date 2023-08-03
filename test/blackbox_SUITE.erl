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
    X0 = fun () -> {ok, Pid} = gen_tcp:connect("127.0.0.1", 5044, [binary]),

                   fun (Text) -> ok = gen_tcp:send(Pid, _Packet = Text)

                   end
         end,

    X1 = fun () -> {ok, Pid} = gen_udp:open(0, [binary]),

                   fun (Text) -> ok = gen_udp:send(Pid, "127.0.0.1", 5044, _Packet = Text)

                   end
         end,

    X2 = fun () -> fun (Text) -> ct:print("~n~p~n", [Text]) end

         end,


    [ begin {ok, Pid} = blackbox:start_child(X, fun () -> ok end, []),

            Res = blackbox:trace(Pid, Config),
            Res

      end || X <- [X0, X1, X2]
    ],

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

