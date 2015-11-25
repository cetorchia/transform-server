-module(data_type_server).

-export([start_link/0, stop/1]).
-export([create_data_type/3]).
-export([update_data_type/2]).
-export([get_data_types/2]).
-export([get_data_type/3]).
-export([delete_data_type/2]).
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

create_data_type(Pid, UserProfileId, DataTypeData) ->
    gen_server:call(Pid, {create_data_type, UserProfileId, DataTypeData}).

update_data_type(Pid, DataTypeData) ->
    gen_server:cast(Pid, {update_data_type, DataTypeData}).

get_data_types(Pid, UserProfileId) ->
    gen_server:call(Pid, {get_data_types, UserProfileId}).

get_data_type(Pid, DataTypeId, UserProfileId) ->
    gen_server:call(Pid, {get_data_type, DataTypeId, UserProfileId}).

delete_data_type(Pid, DataTypeId) ->
    gen_server:cast(Pid, {delete_data_type, DataTypeId}).

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};

handle_cast({update_data_type, DataTypeData}, LoopData) ->
    _Result = data_type:update_data_type(DataTypeData),
    {noreply, LoopData};

handle_cast({delete_data_type, DataTypeId}, LoopData) ->
    _Result = data_type:delete_data_type(DataTypeId),
    {noreply, LoopData}.

handle_call({create_data_type, UserProfileId, DataTypeData}, _From, LoopData) ->
    Result = data_type:create_data_type(UserProfileId, DataTypeData),
    {reply, Result, LoopData};

handle_call({get_data_types, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_types(UserProfileId),
    {reply, Result, LoopData};

handle_call({get_data_type, DataTypeId, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_type(DataTypeId, UserProfileId),
    {reply, Result, LoopData}.
