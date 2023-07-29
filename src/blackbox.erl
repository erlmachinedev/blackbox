-module(blackbox).

-import(erlbox, [success/0, success/1, success/2]).

-export([start_tcp/1]).
-export([start_udp/1]).

-export([start_link/1, trace/2]).

-export([init/1]).
-export([handle_batch/2]).

-behaviour(gen_batch_server).

-include_lib("erlbox/include/erlbox.hrl").

%%% API

start_tcp(URI) ->
    Arg = fun () -> Pid = self(),

                    Res = fun (Text) -> ct:print("~nTCP send (~p): ~p ~p~n", [Pid, Text, URI])

                          end,
                    Res
          end,

    blackbox_sup:start_child(Arg).

start_udp(URI) ->
    Arg = fun () -> Pid = self(),

                    Res = fun (Text) -> ct:print("~nUDP send (~p): ~p ~p~n", [Pid, Text, URI])

                          end,
                    Res
          end,

    blackbox_sup:start_child(Arg).

-spec start_link(function()) -> success(pid()).
start_link(Connect) ->
    Opt = [],

    Fun = Connect(),

    gen_batch_server:start_link(_Name = undefined, ?MODULE, Fun, Opt).

-spec trace(pid(), term()) -> success().
trace(Pid, Event) ->
    gen_batch_server:cast(Pid, Event).

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

%% NOTE https://en.wikipedia.org/wiki/Flight_recorder

%% TODO Implement erl_tracer behaviour

%% TODO Format Fun (the size limit)

%% TODO Consider usage of match spec as a format facility
