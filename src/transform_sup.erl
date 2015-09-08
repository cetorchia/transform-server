
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
    TransformServer = {transform_server,
                        {transform_server, start_link, []},
                        permanent, 5000, worker,
                        [transform_server]},
    WebServer = {web_server,
                 {web_server, start_link, []},
                 permanent, 5000, worker,
                 [web_server]},
    ChildSpecs = [TransformServer, WebServer],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
