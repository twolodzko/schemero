-module(printer_tests).
-include_lib("eunit/include/eunit.hrl").

printer_test() ->
    ?assertEqual("#t", printer:to_string(true)),
    ?assertEqual("#f", printer:to_string(false)),
    ?assertEqual("42", printer:to_string(42)),
    ?assertEqual("3.14", printer:to_string(3.14)),
    ?assertEqual("foo", printer:to_string({symbol, "foo"})),
    ?assertEqual("\"hello, world!\"", printer:to_string({string, "hello, world!"})),
    ?assertEqual("'x", printer:to_string({quote, {symbol, "x"}})),
    ?assertEqual("'()", printer:to_string({quote, []})),
    ?assertEqual("(1 2 3)", printer:to_string([1, 2, 3])),
    ?assertEqual("(() (()))", printer:to_string([[], [[]]])).
