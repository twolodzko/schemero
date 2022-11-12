-module(envir).
-behaviour(gen_server).
-export([
    from_records/1,
    from_parent/1,
    find/2,
    store/3,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).
-record(state, {local, parent = root}).

%% Client

from_records(Records) ->
    Local = dict:from_list(Records),
    gen_server:start(?MODULE, [Local], []).
from_parent(Parent) ->
    gen_server:start(?MODULE, [dict:new(), Parent], []).

find(Key, Pid) ->
    gen_server:call(Pid, {find, Key}).
store(Key, Value, Pid) ->
    gen_server:call(Pid, {store, Key, Value}).

stop(Pid) ->
    gen_server:stop(Pid).

%% Server

init([Local]) ->
    {ok, #state{local = Local}};
init([Local, Parent]) ->
    {ok, #state{local = Local, parent = Parent}}.

handle_call({find, Key}, _From, #state{local = Local, parent = root} = State) ->
    Reply = dict:find(Key, Local),
    {reply, {self(), Reply}, State};
handle_call({find, Key}, _From, #state{local = Local, parent = Parent} = State) ->
    case dict:find(Key, Local) of
        error ->
            Reply = find(Key, Parent),
            {reply, Reply, State};
        Reply ->
            {reply, {self(), Reply}, State}
    end;
handle_call({store, Key, Value}, _From, State) ->
    Local = dict:store(Key, Value, State#state.local),
    NewState = State#state{local = Local},
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.
