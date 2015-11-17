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
