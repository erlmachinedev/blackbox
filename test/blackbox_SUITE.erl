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
    Fun = fun () -> fun (Text) -> ct:print("~nText: ~p~n", [Text]) end end,

    inspect(Fun),

    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

tcp(Config) ->
    Fun = blackbox:tcp("127.0.0.1", 5044, [binary], _Timeout = 5000),

    inspect(Fun),

    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

udp(Config) ->
    Fun = blackbox:udp("127.0.0.1", 5044, [binary]),

    inspect(Fun),

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

inspect(Fun) ->
    Modules = blackbox:modules(),

    MatchSpec = [{'_', [], [{exception_trace}, {return_trace}]}],

    blackbox:trace(Modules, Fun, MatchSpec, _Encode = blackbox:encode(80)),

    [ begin catch(blackbox_ct:test(X))

      end || X <- lists:seq(1, 10)
    ].
