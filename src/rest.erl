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

-module(rest).

-export([handle/3]).

handle(Method, Path, RequestData) when ((Method =:= get) or
                                        (Method =:= post) or
                                        (Method =:= put) or
                                        (Method =:= delete)),
                                       is_map(RequestData) ->
    [Resource|SubResources] = string:tokens(Path, "/"),
    Module = util:to_atom(Resource ++ "_rest_handler"),
    RequestDataData = util:normalize_map(maps:get(data, RequestData)),
    AuthenticatedUserProfile = case authenticate(maps:get(auth_token, RequestData)) of
                                   {ok, UserProfile} ->
                                       UserProfile;
                                   unauthorized ->
                                       undefined;
                                   error ->
                                       undefined
                               end,
    NewRequestData = #{data => RequestDataData,
                       auth_user_profile => AuthenticatedUserProfile},
    Arguments = SubResources ++ [NewRequestData],
    case code:ensure_loaded(Module) of
        {module, _} ->
            case erlang:function_exported(Module, Method, length(Arguments)) of
                true ->
                    case apply(Module, Method, Arguments) of
                        {ok, json, ResponseData} ->
                            {ok, "application/json", util:to_json(ResponseData)};
                        {ok, ContentType, ResponseData} ->
                            {ok, ContentType, ResponseData};
                        ok ->
                            ok;
                        {bad_request, Message} ->
                            {bad_request, Message};
                        unauthorized ->
                            unauthorized;
                        forbidden ->
                            forbidden;
                        not_found ->
                            not_found
                    end;
                false ->
                    not_found
            end;
        {error, _} ->
            not_found
    end.

authenticate(EncodedAuthToken) ->
    case EncodedAuthToken of
        undefined ->
            unauthorized;
        _ ->
            AuthToken = mochiweb_base64url:decode(EncodedAuthToken),
            worker_sup:run(login_sup,
                           fun (Pid) ->
                                   login_server:validate_auth_token(Pid, AuthToken)
                           end)
    end.
