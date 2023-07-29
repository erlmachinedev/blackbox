-module(blackbox_sup).

-export([start_link/0, start_child/1]).

-export([init/1]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Arg) ->
    supervisor:start_child(?MODULE, _List = [Arg]).

init([]) ->
    Mod = blackbox,

    Fun = start_link,
    Arg = [],

    Procs = [ #{'id' => Mod, start => {Mod, Fun, Arg}} ],

    Flags = #{ strategy => simple_one_for_one },

    erlbox:success({Flags, Procs}).
