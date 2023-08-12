-module(blackbox).

-import(erlbox, [success/0, success/1, success/2]).

-export([modules/0]).

-export([trace/2, trace/3, trace/4]).

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

-spec trace(function(), function()) -> term().
trace(Fun, MatchSpec) when is_function(Fun) ->
    trace(Fun, MatchSpec, _Encode = encode(_Depth = 80)).


-spec trace(function(), function(), function()) -> term().
trace(Fun, MatchSpec, Encode) when is_function(Fun) ->
    trace(_Modules = modules(), Fun, MatchSpec, Encode).

-spec trace([module()], function(), function(), function()) -> term().
trace(Modules, Fun, MatchSpec, Encode) when is_function(Fun) ->
    Ret = blackbox_sup:start_child(Fun, Encode),

    erlbox:is_success(Ret) andalso
        begin
            Tracer = element(2, Ret),

            Pid = self(),

            erlang:trace(Pid, true, [set_on_spawn, call, {tracer, Tracer}]),

            [ begin [ begin [ begin erlang:trace_pattern({M, F, A}, MatchSpec, [global])

                              end || {F, A} <- Spec

                            ]

                      end || {trace, Spec} <- M:module_info(attributes)

                    ]

              end || M <- Modules, code:is_loaded(M) /= false
            ]

        end.

-spec start_link(function(), function()) -> success(pid()).
start_link(Fun, Encode) ->
    Command = Fun(),

    gen_server:start_link(?MODULE, fun (Msg) -> Command(_Text = Encode(Msg)) end, []).

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

handle_info(Msg, State) ->
    execute(State, Msg),

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
    %% TODO Elaborate the right format

    fun (_Msg = {trace, Pid, call, {M, F, Args}}) ->

            io_lib:format("~p ~p:~p(~s)~n", [Pid, M, F, io_lib:write(Args, Opt)]);

        (_Msg = {trace, Pid, return_from, {M, F, Arity}, Ret}) ->

            io_lib:format( "~p ~p:~p/~p -> ~s~n", [Pid, M, F, Arity, io_lib:write(Ret, Opt)]);

        (_Msg = {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}) ->

            io_lib:format( "~p ~p:~p/~p <- ~p(~s)~n", [Pid, M, F, Arity, Class, io_lib:write(Value, Opt)])
    end.

%% NOTE https://en.wikipedia.org/wiki/Flight_recorder

%% TODO Match spec (dbg) as a format facility (ms_transform)

%% TODO Match spec compilation
