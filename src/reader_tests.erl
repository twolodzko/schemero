-module(reader_tests).
-include_lib("eunit/include/eunit.hrl").

from_file_test() ->
    Pid = reader:from_file("src/reader_tests.erl"),
    ?assertEqual($-, reader:pop(Pid)),
    ?assertEqual($m, reader:pop(Pid)),
    ?assertEqual($o, reader:pop(Pid)),
    ?assertEqual($d, reader:pop(Pid)).

from_string_test() ->
    Pid = reader:from_string("\nabc"),
    ?assertEqual($\n, reader:pop(Pid)),
    ?assertEqual($a, reader:peek(Pid)),
    ?assertEqual($a, reader:peek(Pid)),
    ?assertEqual($a, reader:pop(Pid)),
    ?assertEqual($b, reader:pop(Pid)),
    ?assertEqual($c, reader:pop(Pid)),
    ?assertEqual(eof, reader:pop(Pid)),
    ?assertEqual(eof, reader:pop(Pid)).

stop_test() ->
    Pid = reader:from_file("src/reader_tests.erl"),
    {state, Device, _Cache} = sys:get_state(Pid),
    % everything works fine
    ?assert(is_process_alive(Pid)),
    ?assert(is_process_alive(Device)),
    % kill it
    ok = gen_server:stop(Pid),
    % everything died
    ?assertNot(is_process_alive(Pid)),
    ?assertNot(is_process_alive(Device)).
