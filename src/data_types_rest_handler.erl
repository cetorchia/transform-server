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

-module(data_types_rest_handler).

-export([get/1]).
-export([get/2]).
-export([post/1]).
-export([post/3]).
-export([put/2]).
-export([delete/2]).

-include("user_profile.hrl").
-include("data_type.hrl").

get(#{auth_user_profile := undefined}) ->
    unauthorized;

get(#{auth_user_profile := UserProfile}) ->
    UserProfileId = UserProfile#user_profile.id,
    {ok, DataTypes} = get_data_types(UserProfileId),
    {ok, json, data_type:to_maps(DataTypes)}.

get(_, #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataTypeIdStr, #{auth_user_profile := UserProfile}) ->
    DataTypeId = list_to_integer(DataTypeIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_type(DataTypeId, UserProfileId) of
        {ok, DataType} ->
            {ok, json, data_type:to_map(DataType)};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

post(#{auth_user_profile := undefined}) ->
    unauthorized;

post(#{data := DataTypeData, auth_user_profile := UserProfile}) ->
    case DataTypeData of
        #{name := Name, matchers := MatchersJSON, unique := Unique}
          when Unique == <<"true">>; Unique == <<"false">> ->
            UserProfileId = UserProfile#user_profile.id,
            NewDataType = data_type:from_map(
                            #{name => Name,
                              matchers => util:from_json(MatchersJSON),
                              unique => case Unique of
                                            <<"true">> ->
                                                true;
                                            <<"false">> ->
                                                false
                                        end}),
            {ok, DataType} = create_data_type(UserProfileId, NewDataType),
            {ok, json, data_type:to_map(DataType)};
        #{name := _, matchers := _, unique := _} ->
            {bad_request, "Invalid unique (boolean)"};
        #{name := _, matchers := _} ->
            {bad_request, "Missing unique"};
        #{name := _} ->
            {bad_request, "Missing matchers"};
        #{} ->
            {bad_request, "Missing name"}
    end.

post(_, "transform", #{auth_user_profile := undefined}) ->
    unauthorized;

post(DataTypeIdStr, "transform", #{data := #{data := Data}, auth_user_profile := UserProfile}) ->
    DataTypeId = list_to_integer(DataTypeIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_type(DataTypeId, UserProfileId) of
        {ok, _} ->
            {ok, DataRecords} = transform(DataTypeId, Data),
            {ok, json, data_record:to_maps(DataRecords)};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end;

post(DataTypeIdStr, "transform", #{data := #{url := URL}, auth_user_profile := UserProfile}) ->
    DataTypeId = list_to_integer(DataTypeIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_type(DataTypeId, UserProfileId) of
        {ok, _} ->
            case httpc:request(binary_to_list(URL)) of
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Data}} ->
                    {ok, DataRecords} = transform(DataTypeId, Data),
                    {ok, json, data_record:to_maps(DataRecords)};
                _ ->
                    {ok, json, []}
            end;
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end;

post(_, "transform", #{data := #{}, auth_user_profile := _}) ->
    {bad_request, "Missing data or url"}.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataTypeIdStr, #{data := DataTypeData, auth_user_profile := UserProfile}) ->
    case DataTypeData of
        #{name := Name, matchers := MatchersJSON, unique := Unique}
          when Unique == <<"true">>; Unique == <<"false">> ->
            DataTypeId = list_to_integer(DataTypeIdStr),
            UserProfileId = UserProfile#user_profile.id,
            case get_data_type(DataTypeId, UserProfileId) of
                {ok, DataType} ->
                    NewDataType = DataType#data_type{
                                    name = Name,
                                    matchers = data_type:from_maps(util:from_json(MatchersJSON)),
                                    unique = case Unique of
                                                 <<"true">> ->
                                                     true;
                                                 <<"false">> ->
                                                     false
                                             end},
                    ok = update_data_type(NewDataType),
                    ok;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
        #{name := _, matchers := _, unique := _} ->
            {bad_request, "Invalid unique (boolean)"};
        #{name := _, matchers := _} ->
            {bad_request, "Missing unique"};
        #{name := _} ->
            {bad_request, "Missing matchers"};
        #{} ->
            {bad_request, "Missing name"}
    end.

delete(_, #{auth_user_profile := undefined}) ->
    unauthorized;

delete(DataTypeIdStr, #{auth_user_profile := UserProfile}) ->
    DataTypeId = list_to_integer(DataTypeIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_type(DataTypeId, UserProfileId) of
        {ok, _} ->
            ok = delete_data_type(DataTypeId),
            ok;
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

get_data_types(UserProfileId) ->
    worker_sup:run(data_type_sup,
                   fun (Pid) ->
                           data_type_server:get_data_types(Pid, UserProfileId)
                   end).

get_data_type(DataTypeId, UserProfileId) ->
    worker_sup:run(data_type_sup,
                   fun (Pid) ->
                           data_type_server:get_data_type(Pid, DataTypeId, UserProfileId)
                   end).

create_data_type(UserProfileId, NewDataType) ->
    worker_sup:run(data_type_sup,
                   fun (Pid) ->
                           data_type_server:create_data_type(Pid, UserProfileId, NewDataType)
                   end).

update_data_type(NewDataType) ->
    worker_sup:run(data_type_sup,
                   fun (Pid) ->
                           data_type_server:update_data_type(Pid, NewDataType)
                   end).

delete_data_type(DataTypeId) ->
    worker_sup:run(data_type_sup,
                   fun (Pid) ->
                           data_type_server:delete_data_type(Pid, DataTypeId)
                   end).

transform(DataTypeId, Data) ->
    worker_sup:run(transformation_sup,
                   fun (Pid) ->
                           transformation_server:transform(Pid, DataTypeId, Data)
                   end).
