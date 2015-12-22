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

-module(login_server).
-export([start_link/0, start_link/1, stop/1]).
-export([login/2, validate_auth_token/2]).
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

login(Pid, LoginData) ->
    gen_server:call(Pid, {login, LoginData}).

validate_auth_token(Pid, AuthToken) ->
    gen_server:call(Pid, {validate_auth_token, AuthToken}).

%% Callback functions
init(_Argument) ->
    {ok, null}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({login, LoginData}, _From, LoopData) ->
    Result = login:login(LoginData),
    {reply, Result, LoopData};

handle_call({validate_auth_token, AuthToken}, _From, LoopData) ->
    Result = login:validate_auth_token(AuthToken),
    {reply, Result, LoopData}.
