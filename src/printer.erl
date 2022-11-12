-module(printer).
-export([to_string/1, concat/1, print/1]).

print(Sexpr) ->
    io:format("~s~n", [to_string(Sexpr)]).

to_string(true) ->
    "#t";
to_string(false) ->
    "#f";
to_string({symbol, Sexpr}) ->
    Sexpr;
to_string({string, Sexpr}) ->
    "\"" ++ Sexpr ++ "\"";
to_string({quote, Sexpr}) ->
    "'" ++ to_string(Sexpr);
to_string(List) when is_list(List) ->
    "(" ++ concat(List) ++ ")";
to_string(Sexpr) ->
    first(io_lib:format("~w", [Sexpr])).

first([Head | _Rest]) ->
    Head.

concat(List) ->
    StringList = lists:map(fun to_string/1, List),
    string:join(StringList, " ").
