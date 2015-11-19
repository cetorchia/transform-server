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
    WebServer = {web_server,
                 {web_server, start_link, []},
                 permanent, 5000, worker,
                 [web_server]},
    ChildSpecs = [LoadingSupervisor,
                  TransformationSupervisor,
                  LoginSupervisor,
                  SignupSupervisor,
                  DataTypeSupervisor,
                  WebServer],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
