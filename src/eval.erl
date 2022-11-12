-module(eval).
-export([
    eval/2,
    eval_all/2,
    eval_file/2
]).

eval({quote, Sexpr}, _Env) ->
    Sexpr;
eval({symbol, Name}, Env) ->
    case envir:find(Name, Env) of
        {_, {ok, Value}} ->
            Value;
        {_, error} ->
            throw({unbound_symbol, Name})
    end;
eval(Sexpr, Env) when is_list(Sexpr) ->
    traceback:call(fun eval_list/2, Sexpr, Env);
eval(Sexpr, _Env) ->
    Sexpr.

eval_list([Head | Args], Env) ->
    case eval(Head, Env) of
        Fun when is_function(Fun, 2) ->
            Fun(Args, Env);
        Sexpr ->
            throw({not_callable, Sexpr})
    end.

eval_all(List, Env) ->
    lists:map(fun(Sexpr) -> eval(Sexpr, Env) end, List).

eval_file(Path, Env) ->
    Reader = reader:from_file(Path),
    eval_stream(Reader, Env, nil).

eval_stream(Pid, Env, Prev) ->
    try parser:parse(Pid) of
        Sexpr ->
            Result = eval:eval(Sexpr, Env),
            eval_stream(Pid, Env, Result)
    catch
        throw:eof ->
            Prev
    end.
