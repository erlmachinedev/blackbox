-module(blackbox).

-import(erlbox, [success/0, success/1, success/2]).

-export([modules/0]).

-export([trace/1, trace/2, trace/3, trace/4]).

-export([start_link/2]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).

-export([tcp/4, udp/3]).

-export([encode/1]).

-behaviour(gen_server).

-include_lib("erlbox/include/erlbox.hrl").

-type hostname() :: inet:hostname().

-type socket_address() :: inet:socket_address().
-type port_number() :: inet:port_number().

%%% API

-spec modules() -> [module()] | [].
modules() ->
    case
        application:get_key(_Key = modules) of {ok, Modules} ->
            Modules;
        _ ->
            []
    end.

attributes(Mod) ->
    Info = Mod:module_info(attributes),

    Spec = proplists:get_value(trace, Info, []),

    Res = lists:map(fun ({F, A}) -> {Mod, F, A} end, Spec),
    Res.

-spec trace(function()) -> term().
trace(Command) ->
    trace(Command, _Encode = encode(_Depth = 80)).

-spec trace(function(), function()) -> term().
trace(Command, Encode) ->
    trace(Command, Encode, _MatchSpec = []).

-spec trace(function(), function(), [term()]) -> term().
trace(Command, Encode, MatchSpec) ->
    trace(_Modules = modules(), Command, Encode, MatchSpec).

-spec trace([module()], function(), function(), [term()]) -> term().
trace(Modules, Command, Encode, MatchSpec) when is_function(Command) ->
    Res = blackbox_sup:start_child(Command, Encode),

    erlbox:is_success(Res) andalso

        begin Pid = self(),

              erlang:trace(Pid, true, [set_on_spawn, call, {tracer, _Tracer = element(2, Res)}]),

              [ begin [ begin erlang:trace_pattern(MFA, MatchSpec, [])

                        end || MFA <- _ = attributes(M)
                      ]

                end || M <- Modules, code:is_loaded(M) /= false
              ]
        end,

    Res.

-spec start_link(function(), function()) -> success(pid()).
start_link(Command, Encode) ->

    Fun = Command(),

    gen_server:start_link(?MODULE, fun (Msg) -> Fun(_Data = Encode(Msg)) end, []).

%% gen_batch_server

-record(state, { command::function() }).

-type state() :: #state{}.

init(Command) ->
    process_flag(trap_exit, true),

    success(_State = state(Command)).

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) -> execute(State, Msg),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% State access

-spec state(function()) -> state().
state(Fun) ->
    #state{ command = Fun }.

-spec command(state()) -> function().
command(State) ->
    Res = State#state.command,
    Res.

-spec execute(state(), binary()) -> success().
execute(State, Text) ->
    Fun = command(State), true = is_function(Fun),

    Res = Fun(Text),
    Res.

%%% Command constructor API

-spec tcp(hostname() | socket_address(), port_number(), [term()], timeout()) -> function().
tcp(Host, Port, Opts, Timeout) ->
    fun () -> {ok, Pid} = gen_tcp:connect(Host, Port, Opts, Timeout),

              fun (Text) -> Packet = Text,

                            Res = gen_tcp:send(Pid, Packet),

                            Res = ok,
                            Res
              end
    end.

-spec udp(hostname() | socket_address(), port_number(), [term()]) -> function().
udp(Host, Port, Opts) ->
    fun () -> {ok, Pid} = gen_udp:open(0, Opts),

              fun (Text) -> Res = gen_udp:send(Pid, Host, Port, _Packet = Text),

                            Res = ok,
                            Res
              end
    end.

encode(Opt) ->
    fun (Msg) -> Res = io_lib:format("~s~n", [io_lib:write(Msg, Opt)]),
                 Res
    end.

%% NOTE https://en.wikipedia.org/wiki/Flight_recorder

%% NOTE https://www.erlang.org/doc/man/ms_transform.html
%% NOTE https://www.erlang.org/doc/man/dbg#fun2ms-1
%% NOTE https://www.erlang.org/doc/apps/erts/match_spec.html

%% TODO Rewrite send API using parse transformations (args, return value)
%% TODO Implement via server cast

%% TODO Introduce injected Fun as expression which returns the result

%% TODO Elaborate the right error handling (based on lager and ms modules)
