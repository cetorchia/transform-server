-module(signup_server).
-export([start_link/0, stop/1]).
-export([signup/2]).
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

signup(Pid, UserProfile) ->
    gen_server:call(Pid, {signup, UserProfile}).

%% Callback functions
init(_Arguments) ->
    {ok, null}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({signup, UserProfile}, _From, LoopData) ->
    Result = signup:signup(UserProfile),
    {reply, Result, LoopData}.
