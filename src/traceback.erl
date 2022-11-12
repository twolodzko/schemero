-module(traceback).
-export([call/3]).
-record(traceback, {trace}).

call(Fun, Sexpr, Env) ->
    try
        Fun(Sexpr, Env)
    catch
        _:{Err, #traceback{trace = Trace}} ->
            SexprString = printer:to_string(Sexpr),
            throw({Err, #traceback{trace = [SexprString | Trace]}});
        _:Err ->
            SexprString = printer:to_string(Sexpr),
            throw({Err, #traceback{trace = [SexprString]}})
    end.
