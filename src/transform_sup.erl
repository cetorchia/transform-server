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

-module(transform_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER_SUP_SPEC(SupervisorName, WorkerName),
        {SupervisorName,
         {worker_sup, start_link, [SupervisorName, WorkerName]},
         permanent, 5000, supervisor,
         [worker_sup]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    LoadingSupervisor = ?WORKER_SUP_SPEC(loading_sup, loading_server),
    TransformationSupervisor = ?WORKER_SUP_SPEC(transformation_sup, transformation_server),
    LoginSupervisor = ?WORKER_SUP_SPEC(login_sup, login_server),
    SignupSupervisor = ?WORKER_SUP_SPEC(signup_sup, signup_server),
    DataTypeSupervisor = ?WORKER_SUP_SPEC(data_type_sup, data_type_server),
    DataCollectionSupervisor = ?WORKER_SUP_SPEC(data_collection_sup, data_collection_server),
    UserDataSupervisor = ?WORKER_SUP_SPEC(user_data_sup, user_data_server),
    RestSupervisor = ?WORKER_SUP_SPEC(rest_sup, rest_server),
    WebServer = {web_server,
                 {web_server, start_link, []},
                 permanent, 5000, worker,
                 [web_server]},
    ChildSpecs = [LoadingSupervisor,
                  TransformationSupervisor,
                  LoginSupervisor,
                  SignupSupervisor,
                  DataTypeSupervisor,
                  DataCollectionSupervisor,
                  UserDataSupervisor,
                  RestSupervisor,
                  WebServer],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
