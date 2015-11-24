-module(loading_server).
-export([start_link/0, start_link/1, stop/0]).
-export([load/5]).
-export([init/1, handle_cast/2]).
-behaviour(gen_server).

%% API functions

%% Maintenance API
start_link() ->
    start_link(null).

start_link(Argument) ->
    gen_server:start_link(?MODULE, Argument, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Client API
load(Pid, DataCollectionId, DataTypeId, UserId, Data) ->
    gen_server:cast(Pid, {load, DataCollectionId, DataTypeId, UserId, Data}).

%% Callback functions
init(_Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};

handle_cast({load, DataCollectionId, DataTypeId, UserId, DataRecords}, LoopData) ->
    loading:load(DataCollectionId, DataTypeId, UserId, DataRecords),
    {noreply, LoopData}.
