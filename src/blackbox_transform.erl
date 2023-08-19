-module(blackbox_transform).

-export([parse_transform/2]).


parse_transform(Ast, _Opt) ->
    ct:print("AST:~p~n", [Ast]),

    Res = Ast,
    Res.
