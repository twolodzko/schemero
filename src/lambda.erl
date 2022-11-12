-module(lambda).
-export([create/2]).

create([Vars | Body], Env) ->
    fun(Args, CallEnv) ->
        {ok, LocalEnv} = envir:from_parent(Env),
        ok = init(Vars, Args, CallEnv, LocalEnv),
        Result = eval:eval_all(Body, LocalEnv),
        lists:last(Result)
    end.

init([], [], _CallEnv, _LocalEnv) ->
    ok;
init([{symbol, Key} | Keys], [Arg | Args], CallEnv, LocalEnv) ->
    Value = eval:eval(Arg, CallEnv),
    ok = envir:store(Key, Value, LocalEnv),
    init(Keys, Args, CallEnv, LocalEnv).
