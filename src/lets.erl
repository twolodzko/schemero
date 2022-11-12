-module(lets).
-export([regular/2, star/2]).

regular([Bindings | Body], Env) ->
    {ok, LocalEnv} = envir:from_parent(Env),
    ok = init(Bindings, Env, LocalEnv),
    Result = eval:eval_all(Body, LocalEnv),
    lists:last(Result).

star([Bindings | Body], Env) ->
    {ok, LocalEnv} = envir:from_parent(Env),
    ok = init(Bindings, LocalEnv, LocalEnv),
    Result = eval:eval_all(Body, LocalEnv),
    lists:last(Result).

init([], _ParentEnv, _LocalEnv) ->
    ok;
init([[{symbol, Key}, Sexpr] | Rest], ParentEnv, LocalEnv) ->
    Value = eval:eval(Sexpr, ParentEnv),
    ok = envir:store(Key, Value, LocalEnv),
    init(Rest, ParentEnv, LocalEnv).
