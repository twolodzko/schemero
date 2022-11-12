-module(traceback_tests).
-include_lib("eunit/include/eunit.hrl").

eval_string(String) ->
    {ok, Env} = scheme:base_envir(),
    Reader = reader:from_string(String),
    Sexpr = parser:parse(Reader),
    eval:eval(Sexpr, Env).

traceback_test() ->
    ?assertThrow(
        {badarith, {traceback, ["(/ 1 0)"]}},
        eval_string("(/ 1 0)")
    ),
    ?assertThrow(
        {badarith, {traceback, ["(let ((f (lambda (x) (/ 1 x)))) (f 0))", "(f 0)", "(/ 1 x)"]}},
        eval_string("(let ((f (lambda (x) (/ 1 x)))) (f 0))")
    ).
