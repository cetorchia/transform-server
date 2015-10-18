-module(loading_server).
-export([start_link/0, start_link/1, stop/0]).
-export([load/3]).
-export([init/1, handle_cast/2]).
-behaviour(gen_server).

%% API functions

%% Maintenance API
start_link() ->
    start_link(null).

start_link(Argument) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Argument, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Client API
load(DataTypeId, UserId, Data) ->
    gen_server:cast(?MODULE, {load, DataTypeId, UserId, Data}).

%% Callback functions
init(Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};

handle_cast({load, DataTypeId, UserId, DataRecords}, LoopData) ->
    loading:load(DataTypeId, UserId, DataRecords),
    {noreply, LoopData}.
