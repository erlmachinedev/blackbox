-module(blackbox_ct).

-export([test/1]).

-trace([test/1]).

-spec test(term()) -> term().
test(Arg) ->
    Arg.
