-module(login_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0, terminate_child/1]).
-export([login/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Client API
%% ===================================================================

login(Username, Password) ->
    Pid = start_child(),
    Result = login_server:login(Pid, Username, Password),
    ok = terminate_child(Pid),
    Result.

%% ===================================================================
%% Maintenance functions
%% ===================================================================

%% Use this to start child processes, namely login servers.
start_child() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Pid.

%% Use this to terminate child processes, namely login servers.
terminate_child(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok.

%% ===================================================================
%% API functions
%% ===================================================================

%% N.B.: My understanding is that, this function is called by the supervisor
%% of login_sup, which happens to be login_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    LoginServer = {login_server,
                   {login_server, start_link, []},
                   temporary, 5000, worker,
                   [login_server]},
    ChildSpecs = [LoginServer],
    {ok, {{simple_one_for_one, 0, 30}, ChildSpecs}}.
