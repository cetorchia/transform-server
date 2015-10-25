-module(transformation_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0, terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Maintenance functions
%% ===================================================================

%% Use this to start child processes, namely transformation servers.
start_child() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Pid.

%% Use this to terminate child processes, namely transformation servers.
terminate_child(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok.

%% ===================================================================
%% API functions
%% ===================================================================

%% N.B.: My understanding is that, this function is called by the supervisor
%% of transformation_sup, which happens to be transform_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    TransformationServer = {transformation_server,
                            {transformation_server, start_link, []},
                            temporary, 5000, worker,
                            [transformation_server]},
    ChildSpecs = [TransformationServer],
    {ok, {{simple_one_for_one, 0, 30}, ChildSpecs}}.
