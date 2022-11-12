-module(eval_tests).
-include_lib("eunit/include/eunit.hrl").

basic_eval_test() ->
    {ok, Env} = envir:from_records([]),
    ?assertEqual(42, eval:eval(42, Env)),
    ?assertEqual(3.14, eval:eval(3.14, Env)),
    ?assertEqual([], eval:eval({quote, []}, Env)),
    ?assertEqual({symbol, "foo"}, eval:eval({quote, {symbol, "foo"}}, Env)),
    ?assertEqual(5, eval:eval([fun([X, Y], _) -> X + Y end, 2, 3], Env)).

eval_symbol_test() ->
    {ok, Env} = envir:from_records([{"x", 1}, {"y", 2}]),
    ?assertEqual(1, eval:eval({symbol, "x"}, Env)),
    ?assertEqual(2, eval:eval({symbol, "y"}, Env)),
    ?assertThrow({unbound_symbol, "invalid"}, eval:eval({symbol, "invalid"}, Env)),
    ?assertEqual({string, "x"}, eval:eval({string, "x"}, Env)),

    {ok, Child} = envir:from_parent(Env),
    ok = envir:store("z", 3, Env),
    ?assertEqual(1, eval:eval({symbol, "x"}, Child)),
    ?assertEqual(3, eval:eval({symbol, "z"}, Child)),
    ?assertThrow({unbound_symbol, "invalid"}, eval:eval({symbol, "invalid"}, Child)).

eval_list_test() ->
    {ok, Env} = envir:from_records([{"add", fun([X, Y], _) -> X + Y end}]),
    ?assertEqual(5, eval:eval([{symbol, "add"}, 2, 3], Env)).

eval_all_test() ->
    {ok, Env} = envir:from_records([{"add", fun([X, Y], _) -> X + Y end}]),
    ?assertEqual(
        [[], 5, {symbol, "x"}, 3.14],
        eval:eval_all(
            [{quote, []}, [{symbol, "add"}, 2, 3], {quote, {symbol, "x"}}, 3.14],
            Env
        )
    ).
