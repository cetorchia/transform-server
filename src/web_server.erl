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

-module(web_server).
-export([start_link/0, start_link/1, loop/1]).

%% Maintenance API
start_link() ->
    start_link([{port, 8080}]).

start_link(Options = [{port, _Port}]) ->
     mochiweb_http:start_link([{name, ?MODULE}, {loop, {?MODULE, loop}} | Options]).

loop(Req) ->
    try 
        "/" ++ Path = Req:get(path),
        case Req:get(method) of
            'GET' ->
                handle_rest(get, Path, Req);
            'POST' ->
                handle_rest(post, Path, Req);
            'PUT' ->
                handle_rest(put, Path, Req);
            'DELETE' ->
                handle_rest(delete, Path, Req);
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Class:What ->
            error_logger:error_msg("~p~n", [{Class, What, erlang:get_stacktrace()}]),
            Req:respond({500, [], []})
    end.

handle_rest(Method, Path, Req) when Method =:= get;
                                    Method =:= post;
                                    Method =:= put;
                                    Method =:= delete ->
    % For the sake of reusability, we are normalizing things, e.g. into a map data structure.
    % We are not, however, tokenizing, e.g. regurgitating the data into a more erlangy form.
    % That is the job of the reusable rest server.
    RequestData = #{data => maps:from_list(
                              case Method of
                                  get ->
                                      Req:parse_qs();
                                  post ->
                                      Req:parse_post();
                                  put ->
                                      Req:parse_post();
                                  delete ->
                                      Req:parse_qs()
                              end),
                    auth_token => Req:get_header_value("Auth-Token")},
    Result = worker_sup:run(rest_sup,
                            fun (Pid) ->
                                    rest_server:handle(Pid, Method, Path, RequestData)
                            end),
    case Result of
        {ok, ContentType, ResponseData} ->
            Req:ok({ContentType, ResponseData});
        ok ->
            Req:respond({200, [], []});
        {bad_request, Message} ->
            Req:respond({400, [{"Content-Type", "text/plain"}], Message});
        unauthorized ->
            Req:respond({401, [], []});
        forbidden ->
            Req:respond({403, [], []});
        not_found ->
            Req:not_found()
    end.
