-module(blackbox_transform).

-import(erl_syntax_lib, [analyze_forms/1]).
-import(erl_syntax_lib, [analyze_function/1]).

-import(erl_syntax_lib, [is_fail_expr/1]).

-export([parse_transform/2]).


parse_transform(Forms, _Opt) ->
    Info = analyze_forms(Forms),

    Mod0 = proplists:get_value(module, Info),
    Mod1 = erl_syntax:atom(Mod0),

    Mod2 = erl_syntax:atom(blackbox),

    ModQ = erl_syntax:module_qualifier(Mod2, _ = erl_syntax:atom(print)),

    Exec = fun(Name, Arg0, Expr) -> Arg1 = erl_syntax:list(Arg0),

                                    Form = erl_syntax:application(ModQ, [Mod1, Name, Arg1, Expr]),
                                    Form
           end,

    Res = process(Forms, _List = extract(Info), Exec, []),
    ct:print("~nForms: ~p~nRes ~p~n", [Forms, Res]),

    Res.

process(Forms, [], _Exec, Acc) ->
    Res = lists:append(Acc, Forms),
    Res;

process([], _, _Exec, Acc) ->
    Acc;

process([Form0|T], List0, Exec, Acc) ->
    Type = erl_syntax:type(Form0),

    if Type == function ->
            List1 = lists:delete(_Elem = analyze_function(Form0), List0),

            Form3 = if List1 == List0 ->
                            Form0;
                       true ->
                            Name = erl_syntax:function_name(Form0),

                            Clauses0 = erl_syntax:function_clauses(Form0),
                            Clauses1 = produce(Name, Clauses0, Exec),

                            Form1 = erl_syntax:function(Name, Clauses1),

                            Form2 = erl_syntax:revert(Form1),
                            Form2
                    end,

            process(T, List1, Exec, lists:append(Acc, [Form3]));
       true ->
            process(T, List0, Exec, lists:append(Acc, [Form0]))
    end.

produce(Name, Clauses, Exec) ->
    [ begin Body0 = erl_syntax:clause_body(X),

            Expr0 = lists:last(Body0), IsFailed = is_fail_expr(Expr0),

            if IsFailed ->
                    X;
               true ->
                    Patterns = erl_syntax:clause_patterns(X),

                    Expr1 = Exec(Name, Patterns, Expr0),

                    Body1 = lists:droplast(Body0),
                    Body2 = lists:append(Body1, [Expr1]),

                    erl_syntax:clause(Patterns, _Guard = erl_syntax:clause_guard(X), Body2)
            end

      end || X <- Clauses
    ].

extract(Info) ->
    Res0 = proplists:get_value(attributes, Info),
    Res1 = proplists:append_values(trace, Res0),

    Res3 = lists:usort(Res1),
    Res3.
