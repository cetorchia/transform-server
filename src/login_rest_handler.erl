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

-module(login_rest_handler).

-export([post/1]).

post(#{data := LoginData}) ->
    case LoginData of
        #{email := _, password := _} ->
            case login(LoginData) of
                {ok, UserProfile} ->
                    {ok, json, user_profile:to_map(UserProfile)};
                error ->
                    unauthorized
            end;
        #{password := _} ->
            {bad_request, "Missing email"};
        #{} ->
            {bad_request, "Missing password"}
    end.

login(LoginData) ->
    worker_sup:run(login_sup,
                   fun (Pid) ->
                           login_server:login(Pid, LoginData)
                   end).
