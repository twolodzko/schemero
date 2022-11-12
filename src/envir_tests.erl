-module(envir_tests).
-include_lib("eunit/include/eunit.hrl").

-define(get_from_env(Records, Key),
    envir:find(element(2, envir:from_records(Records)), Key)
).

envir_test() ->
    {ok, Empty} = envir:from_records([]),
    ?assertEqual({Empty, error}, envir:find("foo", Empty)),

    {ok, Valid} = envir:from_records([{"foo", 42}]),
    ?assertEqual({Valid, {ok, 42}}, envir:find("foo", Valid)).

find_test() ->
    {ok, Env} = envir:from_records([]),
    ok = envir:store("bar", 13, Env),
    ?assertEqual({Env, {ok, 13}}, envir:find("bar", Env)),
    ?assertEqual({Env, error}, envir:find("foo", Env)).

parent_child_test() ->
    % start parent
    {ok, Parent} = envir:from_records([{"a", 1}]),
    ?assertEqual({Parent, {ok, 1}}, envir:find("a", Parent)),
    ?assertEqual({Parent, error}, envir:find("invalid", Parent)),

    % child links to parent
    {ok, Child} = envir:from_parent(Parent),
    ok = envir:store("b", 2, Child),
    ?assertEqual({Child, {ok, 2}}, envir:find("b", Child)),
    ?assertEqual({Parent, {ok, 1}}, envir:find("a", Child)),
    ?assertEqual({Parent, error}, envir:find("invalid", Child)),

    % child can re-define keys
    ok = envir:store("a", 100, Child),
    ?assertEqual({Child, {ok, 100}}, envir:find("a", Child)),
    ?assertEqual({Parent, {ok, 1}}, envir:find("a", Parent)),

    % both are alive
    ?assert(is_process_alive(Parent)),
    ?assert(is_process_alive(Child)),

    % if child dies, parent is still alive
    ok = envir:stop(Child),
    ?assert(is_process_alive(Parent)),
    ?assertNot(is_process_alive(Child)).
