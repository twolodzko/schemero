-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

-define(parse(String),
    parser:parse(reader:from_string(String))
).

parse_test() ->
    ?assertEqual({symbol, "a"}, ?parse("a b c")),
    ?assertEqual(42, ?parse("42\t")),
    ?assertEqual(-1.3e54, ?parse("-1.3e54\n\n")),
    ?assertEqual({symbol, "-foo"}, ?parse("-foo)")),
    ?assertEqual({symbol, "2x"}, ?parse("2x ")),
    ?assertEqual({symbol, "3.14abc"}, ?parse("3.14abc\t")),
    ?assertEqual({quote, {symbol, "x"}}, ?parse("'x")),
    ?assertEqual([], ?parse("()")),
    ?assertEqual([], ?parse("(   \t\n )")),
    ?assertEqual([1, 2, 3], ?parse("(1 2 3)")),
    ?assertEqual([{symbol, "car"}, {quote, [1, 2, 3]}], ?parse("(car '(1 2 3))")),
    ?assertEqual([1, [2], 3], ?parse("(1(2)3)")),
    ?assertEqual(true, ?parse("#t")),
    ?assertEqual(false, ?parse("#f")),
    ?assertEqual({symbol, "#tttt"}, ?parse("#tttt")),
    ?assertEqual({symbol, "#f#t"}, ?parse("#f#t")),
    ?assertEqual({string, ""}, ?parse("\"\"")),
    ?assertEqual({string, "hello, world!"}, ?parse("\"hello, world!\"")),
    ?assertEqual(
        {string, "William Joseph \"Wild Bill\" [1] Donovan"},
        ?parse("\"William Joseph \\\"Wild Bill\\\" [1] Donovan\"")
    ).

comments_test() ->
    ?assertEqual(42, ?parse("42;comment")),
    ?assertEqual(
        42,
        ?parse(
            "\n"
            "  ;; first comment\n"
            "; second comment    "
            "\n"
            "42 ; third comment"
        )
    ).

parse_expected_errors_test() ->
    ?assertThrow(eof, ?parse("")),
    ?assertThrow({unexpected, ")"}, ?parse(")")),
    ?assertThrow({missing, ")"}, ?parse("(a b c")).

-define(read_word(String),
    parser:read_word(reader:from_string(String), [])
).

read_word_test() ->
    ?assertEqual("a", ?read_word("a")),
    ?assertEqual("a", ?read_word("a b c")),
    ?assertEqual("abc", ?read_word("abc(def")),
    ?assertEqual("abc", ?read_word("abc)")),
    ?assertEqual("42", ?read_word("42)")),
    ?assertEqual("3.14", ?read_word("3.14\n")),
    ?assertEqual("-1.3e-5", ?read_word("-1.3e-5\t")),
    ?assertEqual("a", ?read_word("a'a")).
