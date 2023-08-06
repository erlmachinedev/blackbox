-module(blackbox).

-import(erlbox, [success/0, success/1, success/2]).

-export([start_child/3, start_link/1]).

-export([init/1]).
-export([handle_batch/2]).

-export([trace/5]).

-export([tcp/4, udp/3]).

-behaviour(gen_batch_server).

-include_lib("erlbox/include/erlbox.hrl").

-type hostname() :: inet:hostname().

-type socket_address() :: inet:socket_address().
-type port_number() :: inet:port_number().

%%% API

-spec start_child(function(), function(), [term()]) -> success(pid()).
start_child(Fun, _Match, _Opt) ->
    blackbox_sup:start_child(Fun).

-spec start_link(function()) -> success(pid()).
start_link(Connect) ->
    Opt = [],

    Fun = Connect(),

    gen_batch_server:start_link(_Name = undefined, ?MODULE, Fun, Opt).

-spec trace(term(), term(), term(), term(), term()) -> success().
trace(_TraceTag, _TracerState, Tracee, TraceTerm, _Opts) ->
    gen_batch_server:cast(Tracee, TraceTerm).

%% gen_batch_server

-record(state, { socket::function() }).

-type state() :: #state{}.

init(Fun) ->
    process_flag(trap_exit, true),

    success(_State = state(Fun)).

handle_batch(Commands, State) ->
    ct:print("~n~p(~p, ~p)~n", [?FUNCTION_NAME, Commands, State]),

    send(State, _Text = <<"ping">>),

    success(State).

%%% State access

-spec state(function()) -> state().
state(Fun) ->
    #state{ socket = Fun }.

-spec socket(state()) -> function().
socket(State) ->
    Res = State#state.socket,
    Res.

-spec send(state(), binary()) -> success().
send(State, Text) ->
    Fun = socket(State), true = is_function(Fun),

    Res = Fun(Text),
    Res.

%%% Trace constructor API

-spec tcp(hostname() | socket_address(), port_number(), [term()], timeout()) -> function().
tcp(Host, Port, Opts, Timeout) ->
    fun () -> {ok, Pid} = gen_tcp:connect(Host, Port, Opts, Timeout),

              fun (Text) -> Res = gen_tcp:send(Pid, _Packet = Text),

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

%% NOTE https://en.wikipedia.org/wiki/Flight_recorder

%% TODO Implement erl_tracer behaviour

%% TODO Match spec (dbg) as a format facility (ms_transform)

%% TODO Replace with state machine (enabled/3 feature)

%% TODO Track the task queue (the bandwidth is responsibility of transport app)
