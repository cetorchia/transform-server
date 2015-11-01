-module(login_server).
-export([start_link/0, start_link/1, stop/1]).
-export([login/3]).
-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

%% API functions

%% Maintenance API
start_link() ->
    start_link(null).

start_link(Argument) ->
    gen_server:start_link(?MODULE, Argument, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% Client API

login(Pid, Username, Password) ->
    gen_server:call(Pid, {login, Username, Password}).

%% Callback functions
init(_Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({login, Username, Password}, _From, LoopData) ->
    Result = user_profile:login(Username, Password),
    {reply, Result, LoopData}.
