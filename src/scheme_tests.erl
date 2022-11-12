-module(scheme_tests).
-include_lib("eunit/include/eunit.hrl").

eval_string(String) ->
    {ok, Env} = scheme:base_envir(),
    Reader = reader:from_string(String),
    Sexpr = parser:parse(Reader),
    eval:eval(Sexpr, Env).

core_test() ->
    ?assertEqual(true, eval_string("#t")),
    ?assertEqual(false, eval_string("#f")),
    ?assertEqual(42, eval_string("42")),
    ?assertEqual(3.14, eval_string("3.14")),
    ?assertEqual(-5, eval_string("-5")),
    ?assertEqual({symbol, "iamasymbol"}, eval_string("'iamasymbol")).

lists_test() ->
    ?assertEqual([], eval_string("'()")),
    ?assertEqual([1, 2, 3], eval_string("'(1 2 3)")),
    ?assertEqual([], eval_string("(car '(()))")),
    ?assertEqual(1, eval_string("(car '(1))")),
    ?assertEqual(1, eval_string("(car '(1 2 3))")),
    ?assertThrow({{unexpected, []}, _}, eval_string("(car '())")),
    ?assertThrow({{unexpected, 42}, _}, eval_string("(car 42)")),

    ?assertEqual([], eval_string("(cdr '(()))")),
    ?assertEqual([], eval_string("(cdr '(1))")),
    ?assertEqual([2, 3], eval_string("(cdr '(1 2 3))")),
    ?assertThrow({{unexpected, []}, _}, eval_string("(cdr '())")),
    ?assertThrow({{unexpected, 42}, _}, eval_string("(cdr 42)")),
    ?assertEqual([2, 3], eval_string("(cdr '(1 2 3))")),

    ?assertEqual([1], eval_string("(cons 1 '())")),
    ?assertEqual([1, 2, 3], eval_string("(cons 1 '(2 3))")),
    ?assertEqual([1, 2], eval_string("(cons 1 2)")).

special_forms_test() ->
    ?assertEqual([], eval_string("'()")),
    ?assertEqual({symbol, "foo"}, eval_string("'foo")),
    ?assertEqual({symbol, "foo"}, eval_string("(quote foo)")),

    ?assertEqual({symbol, "a"}, eval_string("(begin 1 2 'a)")),
    ?assertEqual([2, 3], eval_string("(begin (car '(1 2)) (cdr '(1 2 3))))")),
    % if it evaluates all the expressions, it will fail
    ?assertThrow({badarith, _}, eval_string("(begin (/ 1 0))")),
    ?assertThrow({badarith, _}, eval_string("(begin (/ 1 0) (+ 2 2) (+ 1 2)))")),
    ?assertThrow({badarith, _}, eval_string("(begin (+ 2 2) (/ 1 0) (+ 1 2)))")),

    ?assertEqual(1, eval_string("(if #t 1 2)")),
    ?assertEqual(2, eval_string("(if #f 1 2)")),
    ?assertEqual(1, eval_string("(if else 1 2)")),
    ?assertEqual(1, eval_string("(if (not #f) 1 2)")),
    % those should not fail if evaluating only the proper branch
    ?assertEqual(4, eval_string("(if #t (+ 2 2) (/ 1 0))")),
    ?assertEqual(4, eval_string("(if #f (/ 1 0) (+ 2 2))")),

    ?assertEqual(1, eval_string("(cond (else 1))")),
    ?assertEqual(1, eval_string("(cond (#t 1))")),
    ?assertEqual(1, eval_string("(cond (#t 1) (#f 2))")),
    ?assertEqual(2, eval_string("(cond (#f 1) (#t 2))")),
    ?assertEqual(110, eval_string("(cond (#f 1) (else (+ 10 100)))")),
    ?assertEqual(true, eval_string("(cond (#f 1) (else))")),
    % those should not fail if evaluating only the proper branch
    ?assertEqual(1, eval_string("(cond (#t 1) (#f (/ 1 0)))")),
    ?assertEqual(2, eval_string("(cond (#f (/ 1 0)) (#t 2))")),

    ?assertEqual([{symbol, "+"}, 2, 2], eval_string("'(+ 2 2)")),
    ?assertEqual(4, eval_string("(eval '(+ 2 2))")),

    ?assertThrow({{error, "2 + 2 /= 5"}, _}, eval_string("(error 2 '+ 2 '/= 5)")),

    ?assertEqual([], eval_string("(let () '())")),
    ?assertEqual(4, eval_string("(let () (+ 2 2))")),
    ?assertEqual(3, eval_string("(let ((x 1) (y 2)) (+ x y))")),
    ?assertEqual(3, eval_string("(let ((x 1) (y (- 3 1))) (car '(1 2 3)) (+ x y))")),
    % it's not let* so it would fail
    ?assertThrow({{unbound_symbol, "x"}, _}, eval_string("(let ((x 1) (y (+ x 1))) (+ x y))")),

    ?assertEqual(11, eval_string("(let ((x 1) (y 1)) (let ((x (* 10 x))) (+ x y)))")),

    ?assertEqual([], eval_string("(let* () '())")),
    ?assertEqual(4, eval_string("(let* () (+ 2 2))")),
    ?assertEqual(3, eval_string("(let* ((x 1) (y 2)) (+ x y))")),
    ?assertEqual(3, eval_string("(let* ((x 1) (y (- 3 1))) (car '(1 2 3)) (+ x y))")),
    ?assertEqual(3, eval_string("(let* ((x 1) (y (+ x 1))) (+ x y))")),

    ?assertEqual(42, eval_string("((lambda (x) x) 42)")),
    ?assertEqual(5, eval_string("((lambda (x y) (+ x y)) 2 3)")),
    ?assertEqual(4, eval_string("((lambda (x) (let ((x (+ x 1))) (* x 2))) 1)")),
    ?assertEqual(0.5, eval_string("(((lambda (x) (lambda (y) (/ x y))) 1) 2)")),

    ?assertEqual(5, eval_string("(let ((x 2)) ((lambda (y) (+ y (+ x 1))) x))")),
    ?assertEqual(5, eval_string("((let ((x 2)) (lambda (y) (+ x y))) 3)")),
    ?assertEqual(6, eval_string("((let* ((x 1) (z (+ x 1))) (lambda (y) (+ x y z))) 3)")).

logical_test() ->
    ?assertEqual(true, eval_string("else")),

    ?assertEqual(false, eval_string("(not #t)")),
    ?assertEqual(true, eval_string("(not #f)")),
    ?assertEqual(false, eval_string("(not '())")),

    ?assertEqual(true, eval_string("(and)")),
    ?assertEqual(true, eval_string("(and #t)")),
    ?assertEqual(true, eval_string("(and #t '() (+ 2 2))")),
    ?assertEqual(false, eval_string("(and #t #f)")),
    % those should fail if evaluating all the branches
    ?assertThrow({badarith, _}, eval_string("(and #t #t (/ 1 0))")),

    ?assertEqual(false, eval_string("(or)")),
    ?assertEqual(true, eval_string("(or #t)")),
    ?assertEqual(true, eval_string("(or #t '() (+ 2 2))")),
    ?assertEqual(true, eval_string("(or #f #t #f)")),
    ?assertEqual(false, eval_string("(or #f (not #t))")),
    % this should not fail if evaluating only the proper branch
    ?assertEqual(true, eval_string("(or #f #f #t (/ 1 0))")).

checker_test() ->
    ?assertEqual(true, eval_string("(null? '())")),
    ?assertEqual(false, eval_string("(null? '(1 2 3))")),
    ?assertEqual(false, eval_string("(null? '(()))")),
    ?assertEqual(false, eval_string("(null? #t)")),
    ?assertEqual(true, eval_string("(procedure? procedure?)")),
    ?assertEqual(true, eval_string("(procedure? quote)")),
    ?assertEqual(false, eval_string("(procedure? 'quote)")),
    ?assertEqual(true, eval_string("(number? 42)")),
    ?assertEqual(true, eval_string("(number? 3.14)")),
    ?assertEqual(true, eval_string("(number? -100)")),
    ?assertEqual(false, eval_string("(number? 'quote)")),
    ?assertEqual(false, eval_string("(number? #t)")),
    ?assertEqual(false, eval_string("(number? '())")),
    ?assertEqual(true, eval_string("(eq?)")),
    ?assertEqual(true, eval_string("(eq? 1 1)")),
    ?assertEqual(false, eval_string("(eq? 1 2)")),
    ?assertEqual(false, eval_string("(eq? 1 #t)")),
    ?assertEqual(true, eval_string("(eq? (+ 1 1) 2 2)")),
    ?assertEqual(true, eval_string("(equal?)")),
    ?assertEqual(true, eval_string("(equal? 1 1)")),
    ?assertEqual(false, eval_string("(equal? 1 2)")),
    ?assertEqual(false, eval_string("(equal? 1 #t)")),
    ?assertEqual(true, eval_string("(equal? (+ 1 1) 2 2)")),
    ?assertEqual(true, eval_string("(symbol? 'car)")),
    ?assertEqual(true, eval_string("(symbol? 'foo)")),
    ?assertEqual(false, eval_string("(symbol? 42)")),
    ?assertEqual(false, eval_string("(symbol? \"foo\")")),
    ?assertEqual(false, eval_string("(string? 'foo)")),
    ?assertEqual(false, eval_string("(string? 42)")),
    ?assertEqual(true, eval_string("(string? \"foo\")")),
    ?assertEqual(true, eval_string("(= 2 2)")),
    ?assertEqual(false, eval_string("(= 2 -2)")),
    ?assertEqual(false, eval_string("(= 2 3)")),
    ?assertEqual(true, eval_string("(= 2 (+ 1 1.0) (* 1 2))")),
    ?assertEqual(false, eval_string("(= 2 (+ 1 1.0) (* 1 100))")),
    ?assertEqual(true, eval_string("(< 1 2)")),
    ?assertEqual(false, eval_string("(< 1 1)")),
    ?assertEqual(false, eval_string("(< 2 1)")),
    ?assertEqual(true, eval_string("(< 1.0 2 (+ 1 1 1))")),
    ?assertEqual(false, eval_string("(< 10.0 2 (+ 1 1 1))")),
    ?assertEqual(false, eval_string("(< 1 2 3 2)")),
    ?assertEqual(true, eval_string("(> 2 1)")),
    ?assertEqual(false, eval_string("(> 1 2)")),
    ?assertEqual(false, eval_string("(> 1 1)")),
    ?assertEqual(true, eval_string("(> 3 2 (- 2 1.0))")),
    ?assertEqual(false, eval_string("(> 3 2 (+ 2 1.0))")).

math_test() ->
    ?assertEqual(0, eval_string("(+)")),
    ?assertEqual(-1, eval_string("(+ -1)")),
    ?assertEqual(6, eval_string("(+ 1 2 3)")),
    ?assertEqual(33.33, eval_string("(+ 1 2 30.33)")),
    ?assertEqual(0, eval_string("(-)")),
    ?assertEqual(-5, eval_string("(- 5)")),
    ?assertEqual(10, eval_string("(- -10)")),
    ?assertEqual(-1, eval_string("(- 1 2)")),
    ?assertEqual(0, eval_string("(- 3 2 1)")),
    ?assertEqual(1, eval_string("(*)")),
    ?assertEqual(2, eval_string("(* 2)")),
    ?assertEqual(4, eval_string("(* 2 2)")),
    ?assertEqual(14.0, eval_string("(* 2 7 1.0)")),
    ?assertEqual(-6, eval_string("(* 2 (+ 1 2) -1)")),
    ?assertEqual(0.5, eval_string("(/ 2)")),
    ?assertEqual(2.0, eval_string("(/ 4 2)")),
    ?assertEqual(5.0, eval_string("(/ 100 4 5)")).

define_test() ->
    {ok, Env} = scheme:base_envir(),
    Define = parser:parse(reader:from_string("(define x 5)")),
    ?assertEqual(5, eval:eval(Define, Env)),

    Check = parser:parse(reader:from_string("x")),
    ?assertEqual(5, eval:eval(Check, Env)),

    Use = parser:parse(reader:from_string("(+ x 10)")),
    ?assertEqual(15, eval:eval(Use, Env)),

    Redefine = parser:parse(reader:from_string("(define x 100)")),
    ?assertEqual(100, eval:eval(Redefine, Env)),

    CheckAgain = parser:parse(reader:from_string("x")),
    ?assertEqual(100, eval:eval(CheckAgain, Env)).

parse_string(String) ->
    parser:parse(reader:from_string(String)).

set_test() ->
    {ok, Parent} = scheme:base_envir(),
    Define = parse_string("(define x 5)"),
    ?assertEqual(5, eval:eval(Define, Parent)),

    {ok, Child} = envir:from_parent(Parent),
    Check = parse_string("x"),
    ?assertEqual(5, eval:eval(Check, Parent)),
    ?assertEqual(5, eval:eval(Check, Child)),

    Redefine = parse_string("(set! x 10)"),
    ?assertEqual(10, eval:eval(Redefine, Child)),
    CheckAgain = parse_string("x"),
    ?assertEqual(10, eval:eval(CheckAgain, Parent)),
    ?assertEqual(10, eval:eval(CheckAgain, Child)),

    NewDefine = parse_string("(set! y 'foo)"),
    ?assertEqual({symbol, "foo"}, eval:eval(NewDefine, Child)),

    NewCheck = parse_string("y"),
    ?assertEqual({symbol, "foo"}, eval:eval(NewCheck, Child)),
    ?assertThrow({unbound_symbol, "y"}, eval:eval(NewCheck, Parent)).
