-module(data_type_server).

-export([start_link/0, stop/1]).
-export([create_data_type/2, get_data_types_by_user/2]).
-export([get_data_type_by_user/3]).
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

get_data_types_by_user(Pid, UserProfileId) ->
    gen_server:call(Pid, {get_data_types_by_user, UserProfileId}).

get_data_type_by_user(Pid, DataTypeId, UserProfileId) ->
    gen_server:call(Pid, {get_data_type_by_user, DataTypeId, UserProfileId}).

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

handle_call({get_data_types_by_user, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_types_by_user(UserProfileId),
    {reply, Result, LoopData};

handle_call({get_data_type_by_user, DataTypeId, UserProfileId}, _From, LoopData) ->
    Result = data_type:get_data_type_by_user(DataTypeId, UserProfileId),
    {reply, Result, LoopData}.
