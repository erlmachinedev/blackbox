-module(blackbox_ct).

-export([test/1, test/2, test/3]).

-trace([test/1]).
-trace([test/2]).
-trace([test/3]).

-spec test(term()) -> term().
test(X) when X == 10 -> throw(?LINE);
test(X) -> X.

-spec test(term(), term()) -> term().
test(X, Y) -> [X, Y].

-spec test(term(), term(), term()) -> term().
test(X, Y, Z) -> X = true, [X, Y, Z].
