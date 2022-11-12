-module(main).
-export([repl/2, eval_files/2]).

repl(Reader, Env) ->
    Sexpr = parser:parse(Reader),
    Result = eval:eval(Sexpr, Env),
    printer:print(Result),
    repl(Reader, Env).

eval_files([Path], Env) ->
    Result = eval:eval_file(Path, Env),
    printer:print(Result);
eval_files([Path | Rest], Env) ->
    eval:eval_file(Path, Env),
    eval_files(Rest, Env).
