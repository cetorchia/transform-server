%% This file is part of the transform server
%% Copyright (c) 2015 Carlos E. Torchia
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(data_collection_server).

-export([start_link/0, stop/1]).
-export([create_data_collection/3]).
-export([update_data_collection/2]).
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

create_data_collection(Pid, UserProfileId, DataCollectionData) ->
    gen_server:call(Pid, {create_data_collection, UserProfileId, DataCollectionData}).

update_data_collection(Pid, DataCollectionData) ->
    gen_server:cast(Pid, {update_data_collection, DataCollectionData}).

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
    {stop, normal, LoopData};

handle_cast({update_data_collection, DataCollectionData}, LoopData) ->
    _Result = data_collection:update_data_collection(DataCollectionData),
    {noreply, LoopData}.

handle_call({create_data_collection, UserProfileId, DataCollectionData}, _From, LoopData) ->
    Result = data_collection:create_data_collection(UserProfileId, DataCollectionData),
    {reply, Result, LoopData};

handle_call({get_data_collections, UserProfileId}, _From, LoopData) ->
    Result = data_collection:get_data_collections(UserProfileId),
    {reply, Result, LoopData};

handle_call({get_data_collection, DataCollectionId, UserProfileId}, _From, LoopData) ->
    Result = data_collection:get_data_collection(DataCollectionId, UserProfileId),
    {reply, Result, LoopData}.
