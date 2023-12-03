-module(blackbox_sup).

-export([start_link/0, start_child/2]).
-export([init/1]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Fun, Encode) ->
    supervisor:start_child(?MODULE, _List = [Fun, Encode]).

init([]) ->
    M = blackbox,

    F = start_link,
    A = [],

    Procs = [ #{'id' => M, start => {M, F, A}} ],

    Flags = #{ strategy => simple_one_for_one },

    erlbox:success({Flags, Procs}).
