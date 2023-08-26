-module(blackbox_transform).

-import(erl_syntax_lib, [analyze_attribute/1, analyze_function/1]).

-export([parse_transform/2]).


parse_transform(Forms, _Opt) ->
    Trace = trace(Forms), ct:print("~nTrace: ~p~n", [Trace]),

    Res = process(Forms, Trace, []),
    Res.

process(Forms, [], Acc) ->
    Res = lists:append(Acc, Forms),
    Res;

process([], _, Acc) ->
    Acc;

process([Form|T], Trace0, Acc) ->
    Type = erl_syntax:type(Form),

    if Type == function ->
            Trace1 = lists:delete(_Elem = analyze_function(Form), Trace0),

            Match = Trace0 == Trace1,

            ct:print("~nMatch: ~p~n", [Match]),

            process(T, Trace1, lists:append(Acc, [Form]));
       true ->
            process(T, Trace0, lists:append(Acc, [Form]))
    end.

trace(Ast) ->
    Res0 = [ begin Args = case analyze_attribute(Node) of
                              {trace, Value} when is_list(Value) ->
                                  Value;
                              {trace, Value} ->
                                  [Value];
                              _ ->
                                  []
                          end,

                   [ begin Res = X,
                           Res

                     end || X = {_Name, _Arity} <- Args
                   ]

             end || Node <- Ast, erl_syntax:type(Node) == attribute
           ],

    Res1 = lists:flatten(Res0),

    Res2 = lists:usort(Res1),
    Res2.

%% TODO Elaborate throw and exit handling (should we handle?)

%% NOTE is_fail_expr(E::erl_syntax:syntaxTree()) -> boolean() !
