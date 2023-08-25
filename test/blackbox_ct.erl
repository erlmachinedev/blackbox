-module(blackbox_ct).

-export([test/1, test/2]).

-include_lib("blackbox/include/blackbox_transform.hrl").

-trace([test/1, test/2]).
-trace([test/1]).

-trace(test/1).

-trace([test/1, ok, 1]).

-trace(ok).

-trace([]).

-spec test(term()) -> term().
test(X) when X == 10 -> throw(?LINE);
test(X) -> X.

-spec test(term(), term()) -> term().
test(X, _) when X == 10 -> throw(?LINE);
test(X, _) -> X.

