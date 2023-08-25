-module(blackbox_transform).

-import(erl_syntax_lib, [analyze_attribute/1]).

-export([parse_transform/2]).


parse_transform(Ast, _Opt) ->
    %% ct:print("AST:~p~n", [Ast])

    Test = trace(Ast), ct:print("~nTest: ~p~n", [Test]),

    Res = Ast,
    Res.

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

%% TODO is_function/1
