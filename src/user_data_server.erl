-module(user_data_server).

-export([start_link/0, stop/1]).
-export([get_user_data/2]).
-export([get_user_data/4]).
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

get_user_data(Pid, DataCollectionId) when is_integer(DataCollectionId) ->
    gen_server:call(Pid, {get_user_data, DataCollectionId}).

get_user_data(Pid, DataCollectionId, KeyName, KeyValue) ->
    gen_server:call(Pid, {get_user_data, DataCollectionId, KeyName, KeyValue}).

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({get_user_data, DataCollectionId}, _From, LoopData)
  when is_integer(DataCollectionId) ->
    Result = user_data:get_user_data(DataCollectionId),
    {reply, Result, LoopData};

handle_call({get_user_data, DataCollectionId, KeyName, KeyValue}, _From, LoopData) ->
    Result = user_data:get_user_data(DataCollectionId, KeyName, KeyValue),
    {reply, Result, LoopData}.
