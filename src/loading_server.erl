-module(loading_server).
-export([start_link/0, start_link/1, stop/0]).
-export([load/4]).
-export([merge/4]).
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
load(Pid, DataCollectionId, UserProfileId, DataRecords) ->
    gen_server:cast(Pid, {load, DataCollectionId, UserProfileId, DataRecords}).

merge(Pid, DataCollectionId, UserProfileId, DataRecords) ->
    gen_server:cast(Pid, {merge, DataCollectionId, UserProfileId, DataRecords}).

%% Callback functions
init(_Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};

handle_cast({load, DataCollectionId, UserProfileId, DataRecords}, LoopData) ->
    loading:load(DataCollectionId, UserProfileId, DataRecords),
    {noreply, LoopData};

handle_cast({merge, DataCollectionId, UserProfileId, DataRecords}, LoopData) ->
    loading:merge(DataCollectionId, UserProfileId, DataRecords),
    {noreply, LoopData}.
