-module(transform_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    TransformationSupervisor = {transformation_sup,
                                {transformation_sup, start_link, []},
                                permanent, infinity, supervisor,
                                [transformation_sup]},
    LoadingServer = {loading_server,
                     {loading_server, start_link, []},
                     permanent, 5000, worker,
                     [loading_server]},
    WebServer = {web_server,
                 {web_server, start_link, []},
                 permanent, 5000, worker,
                 [web_server]},
    ChildSpecs = [TransformationSupervisor, LoadingServer, WebServer],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
