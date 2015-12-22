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

-module(worker_sup).

-behaviour(supervisor).

-export([run/2]).
-export([start_link/2]).
-export([init/1]).

% API
run(SupRef, Fun) ->
    {ok, Pid} = supervisor:start_child(SupRef, []),
    Result = Fun(Pid),
    ok = supervisor:terminate_child(SupRef, Pid),
    Result.

start_link(SupRef, WorkerName) ->
    supervisor:start_link({local, SupRef}, ?MODULE, [WorkerName]).

% Callback functions
init([WorkerName]) ->
    WorkerModuleName = WorkerName,
    WorkerSpec = {WorkerName,
                  {WorkerModuleName, start_link, []},
                  temporary, 5000, worker,
                  [WorkerModuleName]},
    {ok, {{simple_one_for_one, 0, 30}, [WorkerSpec]}}.
