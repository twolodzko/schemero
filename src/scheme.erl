-module(scheme).
-export([base_envir/0]).

base_envir() ->
    envir:from_records([
        {"-", fun difference_/2},
        {"*", fun multiply_/2},
        {"/", fun divide_/2},
        {"+", fun sum_/2},
        {"<", fun num_lt_/2},
        {"=", fun num_eq_/2},
        {">", fun num_gt_/2},
        {"and", fun and_/2},
        {"begin", fun(Sexpr, Env) ->
            lists:last(eval:eval_all(Sexpr, Env))
        end},
        {"bool?", fun is_bool_/2},
        {"car", fun car_/2},
        {"cdr", fun cdr_/2},
        {"cond", fun cond_/2},
        {"cons", fun cons_/2},
        {"define", fun define_/2},
        {"else", true},
        {"eq?", fun equal_/2},
        {"equal?", fun equal_/2},
        {"error", fun error_/2},
        {"eval", fun([Sexpr], Env) ->
            eval:eval(eval:eval(Sexpr, Env), Env)
        end},
        {"float?", fun is_float_/2},
        {"if", fun if_/2},
        {"integer?", fun is_integer_/2},
        {"lambda", fun lambda:create/2},
        {"let", fun lets:regular/2},
        {"let*", fun lets:star/2},
        {"list", fun eval:eval_all/2},
        {"load", fun load_/2},
        {"not", fun not_/2},
        {"null?", fun is_null_/2},
        {"number?", fun is_number_/2},
        {"or", fun or_/2},
        {"pair?", fun is_pair_/2},
        {"procedure?", fun is_procedure_/2},
        {"quote", fun([Sexpr], _Env) -> Sexpr end},
        {"set!", fun set_/2},
        {"string?", fun is_string_/2},
        {"symbol?", fun is_symbol_/2}
    ]).

%% Special forms

define_([{symbol, Key}, Sexpr], Env) ->
    Value = eval:eval(Sexpr, Env),
    ok = envir:store(Key, Value, Env),
    Value.

set_([{symbol, Key}, Sexpr], Env) ->
    Value = eval:eval(Sexpr, Env),
    case envir:find(Key, Env) of
        {Pid, {ok, _Value}} ->
            envir:store(Key, Value, Pid);
        {_Pid, error} ->
            envir:store(Key, Value, Env)
    end,
    Value.

if_([Condition, IfTrue, IfFalse], Env) ->
    Branch =
        case is_true(eval:eval(Condition, Env)) of
            true -> IfTrue;
            false -> IfFalse
        end,
    eval:eval(Branch, Env).

cond_([[Condition] | Rest], Env) ->
    Result = eval:eval(Condition, Env),
    case is_true(Result) of
        true -> Result;
        false -> cond_(Rest, Env)
    end;
cond_([[Condition, Branch] | Rest], Env) ->
    case is_true(eval:eval(Condition, Env)) of
        true -> eval:eval(Branch, Env);
        false -> cond_(Rest, Env)
    end.

error_(Sexpr, Env) ->
    Evaluated = eval:eval_all(Sexpr, Env),
    Msg = printer:concat(Evaluated),
    throw({error, Msg}).

load_([Sexpr], Env) ->
    {string, Path} = eval:eval(Sexpr, Env),
    eval:eval_file(Path, Env).

%% Lists

car_([Sexpr], Env) ->
    car_(eval:eval(Sexpr, Env)).
car_([Head | _Rest]) ->
    Head;
car_(Sexpr) ->
    throw({unexpected, Sexpr}).

cdr_([Sexpr], Env) ->
    cdr_(eval:eval(Sexpr, Env)).
cdr_([_Head | Rest]) ->
    Rest;
cdr_(Sexpr) ->
    throw({unexpected, Sexpr}).

cons_(Sexpr, Env) ->
    case eval:eval_all(Sexpr, Env) of
        [Head, Rest] when is_list(Rest) ->
            % without the guard, it'll create an improper list
            [Head | Rest];
        [Head, Rest] ->
            [Head, Rest]
    end.

%% Logical

not_([Sexpr], Env) ->
    not is_true(eval:eval(Sexpr, Env)).

and_([], _Env) ->
    true;
and_([Head | Rest], Env) ->
    case is_true(eval:eval(Head, Env)) of
        true ->
            and_(Rest, Env);
        false ->
            false
    end.

or_([], _Env) ->
    false;
or_([Head | Rest], Env) ->
    case is_true(eval:eval(Head, Env)) of
        true ->
            true;
        false ->
            or_(Rest, Env)
    end.

%% Checkers

is_symbol_([Sexpr], Env) ->
    is_symbol_(eval:eval(Sexpr, Env)).
is_symbol_({symbol, _}) ->
    true;
is_symbol_(_) ->
    false.

is_string_([Sexpr], Env) ->
    is_string_(eval:eval(Sexpr, Env)).
is_string_({string, _}) ->
    true;
is_string_(_) ->
    false.

is_null_([Sexpr], Env) ->
    is_null_(eval:eval(Sexpr, Env)).
is_null_([]) ->
    true;
is_null_(_) ->
    false.

is_pair_([Sexpr], Env) ->
    is_pair_(eval:eval(Sexpr, Env)).
is_pair_([]) ->
    false;
is_pair_(Sexpr) ->
    is_list(Sexpr).

is_procedure_([Sexpr], Env) ->
    is_function(eval:eval(Sexpr, Env), 2).

is_float_([Sexpr], Env) ->
    is_float(eval:eval(Sexpr, Env)).

is_integer_([Sexpr], Env) ->
    is_integer(eval:eval(Sexpr, Env)).

is_number_([Sexpr], Env) ->
    Evaluated = eval:eval(Sexpr, Env),
    is_integer(Evaluated) orelse is_float(Evaluated).

is_bool_([Sexpr], Env) ->
    is_boolean(eval:eval(Sexpr, Env)).

%% Math

sum_(Sexpr, Env) ->
    lists:sum(eval:eval_all(Sexpr, Env)).

difference_(Sexpr, Env) ->
    difference_(eval:eval_all(Sexpr, Env)).
difference_([]) ->
    0;
difference_([X]) ->
    -X;
difference_([Head | Rest]) ->
    lists:foldl(fun(X, Acc) -> Acc - X end, Head, Rest).

multiply_(Sexpr, Env) ->
    multiply_(eval:eval_all(Sexpr, Env)).
multiply_(List) ->
    lists:foldl(fun(X, Acc) -> Acc * X end, 1, List).

divide_(Sexpr, Env) ->
    divide_(eval:eval_all(Sexpr, Env)).
divide_([X]) ->
    1 / X;
divide_([Head | Rest]) ->
    lists:foldl(fun(X, Acc) -> Acc / X end, Head, Rest).

%% Comparisons

equal_(Sexpr, Env) ->
    equal_(eval:eval_all(Sexpr, Env)).
equal_([X, Y | Rest]) when X =:= Y ->
    equal_([Y | Rest]);
equal_([_X, _Y | _Rest]) ->
    false;
equal_(_) ->
    true.

num_eq_(Sexpr, Env) ->
    num_eq_(eval:eval_all(Sexpr, Env)).
num_eq_([X, Y | Rest]) when X == Y ->
    num_eq_([Y | Rest]);
num_eq_([_X, _Y | _Rest]) ->
    false;
num_eq_(_) ->
    true.

num_lt_(Sexpr, Env) ->
    num_lt_(eval:eval_all(Sexpr, Env)).
num_lt_([X, Y | Rest]) when X < Y ->
    num_lt_([Y | Rest]);
num_lt_([_X, _Y | _Rest]) ->
    false;
num_lt_(_) ->
    true.

num_gt_(Sexpr, Env) ->
    num_gt_(eval:eval_all(Sexpr, Env)).
num_gt_([X, Y | Rest]) when X > Y ->
    num_gt_([Y | Rest]);
num_gt_([_X, _Y | _Rest]) ->
    false;
num_gt_(_) ->
    true.

%% Helpers

is_true(false) ->
    false;
is_true(_Sexpr) ->
    true.
