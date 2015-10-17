-module(transformation_server).
%-export([start_link/0, start_link/1, stop/0]).
-export([transform/2]).
-export([init/1, handle_cast/2, handle_call/2]).
-behaviour(gen_server).

%% API functions

%% Maintenance API
%start_link() ->
%    start_link(null).
%
%start_link(Argument) ->
%    gen_server:start_link({local, ?MODULE}, ?MODULE, Argument, []).
%
%stop() ->
%    gen_server:cast(?MODULE, stop).

%% Client API
transform(DataTypeId, Data) ->
    gen_server:call(?MODULE, {transform, DataTypeId, Data}).

%% Callback functions
init(Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({transform, DataTypeId, InputData}, LoopData) ->
    OutputData = transformation:transform(DataTypeId, InputData),
    {reply, OutputData, LoopData}.
