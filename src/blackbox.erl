-module(blackbox).

-import(erlbox, [success/0, success/1, success/2]).

-export([start_child/3]).

-export([start_link/1, trace/2]).

-export([init/1]).
-export([handle_batch/2]).

-behaviour(gen_batch_server).

-include_lib("erlbox/include/erlbox.hrl").

%%% API

-spec start_child(function(), function(), [term()]) -> success(pid()).
start_child(Fun, _Match, _Opt) ->
    blackbox_sup:start_child(Fun).

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

%% TODO Match spec (dbg) as a format facility

%% NOTE Provide custom connect Fun

%% TODO Replace with state machine (enabled/3 feature)

%% TODO Intensity control (term size | frequency)
