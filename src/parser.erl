-module(parser).
-export([parse/1, read_word/2]).
-define(is_number(X), X >= 48, X =< 57).

parse(Pid) ->
    skip_whitespace(Pid),
    case reader:peek(Pid) of
        $( ->
            parse_list(Pid);
        $' ->
            reader:pop(Pid),
            {quote, parse(Pid)};
        $, ->
            throw({not_implemented, ","});
        $) ->
            throw({unexpected, ")"});
        $" ->
            reader:pop(Pid),
            parse_string(Pid);
        $; ->
            skip_line(Pid),
            parse(Pid);
        eof ->
            throw(eof);
        _ ->
            parse_atom(Pid)
    end.

skip_whitespace(Pid) ->
    case is_whitespace(reader:peek(Pid)) of
        true ->
            reader:pop(Pid),
            skip_whitespace(Pid);
        false ->
            ok
    end.

skip_line(Pid) ->
    case reader:pop(Pid) of
        $\n ->
            ok;
        _ ->
            skip_line(Pid)
    end.

parse_atom(Pid) ->
    Word = read_word(Pid, []),
    case Word of
        "#t" ->
            true;
        "#f" ->
            false;
        [Char | _] when ?is_number(Char) ->
            maybe_number(Word);
        [$-, Char | _] when ?is_number(Char) ->
            maybe_number(Word);
        _ ->
            {symbol, Word}
    end.

parse_list(Pid) ->
    reader:pop(Pid),
    parse_list(Pid, []).
parse_list(Pid, Acc) ->
    skip_whitespace(Pid),
    case reader:peek(Pid) of
        $) ->
            reader:pop(Pid),
            lists:reverse(Acc);
        eof ->
            throw({missing, ")"});
        _ ->
            Elem = parse(Pid),
            parse_list(Pid, [Elem | Acc])
    end.

parse_string(Pid) ->
    String = parse_string(Pid, []),
    {string, String}.
parse_string(Pid, Acc) ->
    case reader:pop(Pid) of
        eof ->
            throw({missing, "\""});
        $" ->
            lists:reverse(Acc);
        $\\ ->
            Char = reader:pop(Pid),
            parse_string(Pid, [Char | Acc]);
        Char ->
            parse_string(Pid, [Char | Acc])
    end.

read_word(Pid, Acc) ->
    case reader:peek(Pid) of
        eof ->
            lists:reverse(Acc);
        Char ->
            case is_whitespace(Char) orelse is_special(Char) of
                true ->
                    lists:reverse(Acc);
                false ->
                    reader:pop(Pid),
                    read_word(Pid, [Char | Acc])
            end
    end.

is_whitespace(Char) ->
    lists:member(Char, [$\ , $\n, $\t]).
is_special(Char) ->
    lists:member(Char, [$(, $), $", $', $;, $,]).

maybe_number(String) ->
    case string:to_integer(String) of
        {Val, []} ->
            Val;
        _ ->
            case string:to_float(String) of
                {Val, []} ->
                    Val;
                _ ->
                    {symbol, String}
            end
    end.
