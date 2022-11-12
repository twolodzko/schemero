-module(reader).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    from_stdin/0,
    from_file/1,
    from_string/1,
    pop/1,
    peek/1
]).
-record(state, {device, cache}).

%% Client

from_stdin() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [standard_io, []], []),
    Pid.
from_file(File) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [{file, File}, []], []),
    Pid.
from_string(String) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [string, String], []),
    Pid.

pop(Pid) ->
    gen_server:call(Pid, pop, infinity).
peek(Pid) ->
    gen_server:call(Pid, peek, infinity).

%% Server

init([{file, File}, Cache]) ->
    case file:open(File, [read]) of
        {ok, Device} ->
            {ok, #state{device = Device, cache = Cache}};
        {error, Reason} ->
            throw({cannot_open, Reason})
    end;
init([Device, Cache]) ->
    {ok, #state{device = Device, cache = Cache}}.

handle_call(pop, _From, State) ->
    {Reply, NewState} = pop_state(State),
    {reply, Reply, NewState};
handle_call(peek, _From, State) ->
    {Reply, NewState} = peek_state(State),
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Helpers

pop_state(#state{device = string, cache = []} = State) ->
    {eof, State};
pop_state(#state{device = Device, cache = []} = State) ->
    case io:get_line(Device, "> ") of
        [Head | Rest] ->
            {Head, State#state{cache = Rest}};
        eof ->
            {eof, State}
    end;
pop_state(#state{cache = [Head | Rest]} = State) ->
    {Head, State#state{cache = Rest}}.

peek_state(#state{device = string, cache = []} = State) ->
    {eof, State};
peek_state(#state{device = Device, cache = []} = State) ->
    case io:get_line(Device, "> ") of
        [Head | _] = Line ->
            {Head, State#state{cache = Line}};
        eof ->
            {eof, State}
    end;
peek_state(#state{cache = [Head | _]} = State) ->
    {Head, State}.
