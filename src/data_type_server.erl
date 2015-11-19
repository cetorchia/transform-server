-module(data_type_server).
-export([start_link/0, stop/1]).
-export([create_data_type/2]).
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

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({create_data_type, DataTypeData}, _From, LoopData) ->
    Result = data_type:create_data_type(DataTypeData),
    {reply, Result, LoopData}.
