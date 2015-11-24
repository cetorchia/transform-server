-module(data_type_server).

-export([start_link/0, stop/1]).
-export([create_data_type/2]).
-export([update_data_type/3]).
-export([get_data_types/2]).
-export([get_data_type/3]).
-export([delete_data_type/3]).
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

create_data_type(Pid, DataTypeData) ->
    gen_server:call(Pid, {create_data_type, DataTypeData}).

update_data_type(Pid, DataTypeId, DataTypeData) ->
    gen_server:call(Pid, {update_data_type, DataTypeId, DataTypeData}).

get_data_types(Pid, UserProfileId) ->
    gen_server:call(Pid, {get_data_types, UserProfileId}).

get_data_type(Pid, DataTypeId, UserProfileId) ->
    gen_server:call(Pid, {get_data_type, DataTypeId, UserProfileId}).

delete_data_type(Pid, DataTypeId, UserProfileId) ->
    gen_server:call(Pid, {delete_data_type, DataTypeId, UserProfileId}).

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({create_data_type, DataTypeData}, _From, LoopData) ->
    Result = data_type:create_data_type(DataTypeData),
    {reply, Result, LoopData};

handle_call({update_data_type, DataTypeId, DataTypeData}, _From, LoopData) ->
    Result = data_type:update_data_type(DataTypeId, DataTypeData),
    {reply, Result, LoopData};

handle_call({get_data_types, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_types(UserProfileId),
    {reply, Result, LoopData};

handle_call({get_data_type, DataTypeId, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_type(DataTypeId, UserProfileId),
    {reply, Result, LoopData};

handle_call({delete_data_type, DataTypeId, UserProfileId}, _From, LoopData) ->
    Result = data_type:delete_data_type(DataTypeId, UserProfileId),
    {reply, Result, LoopData}.
