-module(blackbox_SUITE).

-import(erlbox, [is_success/1]).

-export([suite/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).

-export([print/1, tcp/1, udp/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [].

all() -> [print, tcp, udp].

init_per_suite(Config) ->
    bootstrap(),

    Res = Config,
    Res.

end_per_suite(_) ->
    shutdown().

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

print(Config) ->
    Command = fun () -> fun (Text) -> ct:print("~nText: ~p~n", [Text]) end end,

    inspect(Command),

    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

tcp(Config) ->
    Command = blackbox:tcp("127.0.0.1", 5044, [binary], _Timeout = 5000),

    inspect(Command),

    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

udp(Config) ->
    Command = blackbox:udp("127.0.0.1", 5044, [binary]),

    inspect(Command),

    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

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

inspect(Command) ->
    Modules = blackbox:modules(),

    %% TODO Test with timer:tc/3

    %%MatchSpec = [{'_', [], [{exception_trace}, {return_trace}]}],

    MatchSpec = [],

    %% MatchSpec = dbg:fun2ms(fun(_) -> message(process_dump()) end),

    %% MatchSpec = dbg:fun2ms(fun([A, B]) -> true end),

    ct:print("~nMatchSpec: ~p~n", [MatchSpec]),

    {ok, Pid} = blackbox:trace(Modules, Command, _Encode = blackbox:encode(80), MatchSpec),

    [ begin catch(blackbox_ct:test(X))

      end || X <- lists:seq(1, 10)
    ],

    {tracer, Pid} = erlang:trace_info(_Pid = self(), tracer),

    Res = Pid,
    Res.
