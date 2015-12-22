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

-module(user_data_server).

-export([start_link/0, stop/1]).
-export([get_user_data/2]).
-export([get_user_data/3]).
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

get_user_data(Pid, DataCollectionId, Key) ->
    gen_server:call(Pid, {get_user_data, DataCollectionId, Key}).

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

handle_call({get_user_data, DataCollectionId, Key}, _From, LoopData) ->
    Result = user_data:get_user_data(DataCollectionId, Key),
    {reply, Result, LoopData}.
