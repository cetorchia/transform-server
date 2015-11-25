-module(data_collection_server).

-export([start_link/0, stop/1]).
-export([create_data_collection/2]).
-export([update_data_collection/3]).
-export([get_data_collections/2]).
-export([get_data_collection/3]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).

-behaviour(gen_server).

%% API functions

%% Maintenance API
start_link() ->
    Arguments = [],
    gen_server:start_link(?MODULE, Arguments, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% Client API

create_data_collection(Pid, DataCollectionData) ->
    gen_server:call(Pid, {create_data_collection, DataCollectionData}).

update_data_collection(Pid, DataCollectionId, DataCollectionData) ->
    gen_server:call(Pid, {update_data_collection, DataCollectionId, DataCollectionData}).

get_data_collections(Pid, UserProfileId) ->
    gen_server:call(Pid, {get_data_collections, UserProfileId}).

get_data_collection(Pid, DataCollectionId, UserProfileId) ->
    gen_server:call(Pid, {get_data_collection, DataCollectionId, UserProfileId}).

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({create_data_collection, DataCollectionData}, _From, LoopData) ->
    Result = data_collection:create_data_collection(DataCollectionData),
    {reply, Result, LoopData};

handle_call({update_data_collection, DataCollectionId, DataCollectionData}, _From, LoopData) ->
    Result = data_collection:update_data_collection(DataCollectionId, DataCollectionData),
    {reply, Result, LoopData};

handle_call({get_data_collections, UserProfileId}, _From, LoopData) ->
    Result = data_collection:get_data_collections(UserProfileId),
    {reply, Result, LoopData};

handle_call({get_data_collection, DataCollectionId, UserProfileId}, _From, LoopData) ->
    Result = data_collection:get_data_collection(DataCollectionId, UserProfileId),
    {reply, Result, LoopData}.
