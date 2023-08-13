-module(blackbox_ct).

-export([test/1]).

-trace([test/1]).

-spec test(term()) -> term().
test(X) when X == 10 -> throw(?LINE);
test(X) -> X.
