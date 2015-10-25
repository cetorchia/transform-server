-module(transformation_server).
-export([start_link/0, start_link/1, stop/1]).
-export([transform/3]).
-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

%% API functions

%% Maintenance API
start_link() ->
    start_link(null).

start_link(Argument) ->
    gen_server:start_link(?MODULE, Argument, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% Client API

transform(Pid, DataTypeId, Data) ->
    gen_server:call(Pid, {transform, DataTypeId, Data}).

%% Callback functions
init(_Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({transform, DataTypeId, InputData}, _From, LoopData) ->
    DataRecords = transformation:transform(DataTypeId, InputData),
    {reply, DataRecords, LoopData}.
