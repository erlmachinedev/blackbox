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
    Fun = fun () -> {ok, Pid} = gen_tcp:connect("127.0.0.1", 5044, [binary]),

                    Res = fun (Text) -> gen_tcp:send(Pid, _Packet = Text),

                                        ct:print("~nTCP send (~p): ~p~n", [Pid, Text])

                          end,
                    Res
          end,

    {ok, Pid} = blackbox:start_child(Fun, fun () -> ok end, []),

    blackbox:trace(Pid, Config),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

start_udp(Config) ->
    Fun = fun () -> {ok, Pid} = gen_udp:open(0, [binary]),

                    Res = fun (Text) -> ok = gen_udp:send(Pid, "127.0.0.1", 5044, _Packet = Text),

                                        ct:print("~nUDP send (~p): ~p~n", [Pid, Text])

                          end,
                    Res
          end,

    {ok, Pid} = blackbox:start_child(Fun, fun () -> ok end, []),

    blackbox:trace(Pid, Config),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

