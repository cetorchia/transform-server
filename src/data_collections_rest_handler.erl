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

-module(data_collections_rest_handler).
-export([post/1]).
-export([post/3]).
-export([put/2]).
-export([get/1]).
-export([get/2]).
-export([get/3]).
-export([get/4]).

-include("user_profile.hrl").
-include("data_collection.hrl").

get(#{auth_user_profile := undefined}) ->
    unauthorized;

get(#{auth_user_profile := UserProfile}) ->
    UserProfileId = UserProfile#user_profile.id,
    {ok, DataCollections} = get_data_collections(UserProfileId),
    {ok, json, data_collection:to_maps(DataCollections)}.

get(_, #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataCollectionIdStr, #{auth_user_profile := UserProfile}) ->
    DataCollectionId = list_to_integer(DataCollectionIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_collection(DataCollectionId, UserProfileId) of
        {ok, DataCollection} ->
            {ok, json, data_collection:to_map(DataCollection)};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

get(_, "data", #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataCollectionIdStr, "data", #{auth_user_profile := UserProfile}) ->
    DataCollectionId = list_to_integer(DataCollectionIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_collection(DataCollectionId, UserProfileId) of
        {ok, _} ->
            {ok, UserData} = get_user_data(DataCollectionId),
            {ok, json, user_data:to_maps(UserData)};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

get(_, "data", _, #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataCollectionIdStr, "data", KeyStr, #{auth_user_profile := UserProfile}) ->
    DataCollectionId = list_to_integer(DataCollectionIdStr),
    Key = list_to_binary(KeyStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_collection(DataCollectionId, UserProfileId) of
        {ok, _} ->
            {ok, UserData} = get_user_data(DataCollectionId, Key),
            {ok, json, user_data:to_maps(UserData)};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

post(#{auth_user_profile := undefined}) ->
    unauthorized;

post(#{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := Name, unique := Unique}
          when Unique == <<"true">>; Unique == <<"false">> ->
            UserProfileId = UserProfile#user_profile.id,
            NewDataCollection = #data_collection{name = Name,
                                                 unique = case Unique of
                                                              <<"true">> ->
                                                                  true;
                                                              <<"false">> ->
                                                                  false
                                                          end},
            {ok, DataCollection} = create_data_collection(UserProfileId, NewDataCollection),
            {ok, json, data_collection:to_map(DataCollection)};
        #{name := _, unique := _} ->
            {bad_request, "Invalid unique (boolean)"};
        #{name := _} ->
            {bad_request, "Missing unique"};
        #{} ->
            {bad_request, "Missing name"}
    end.

post(_, "data", #{auth_user_profile := undefined}) ->
    unauthorized;

post(DataCollectionIdStr, "data", #{data := Data, auth_user_profile := UserProfile}) ->
    case Data of
        #{data_records := DataRecordsJSON} ->
            DataCollectionId = list_to_integer(DataCollectionIdStr),
            UserProfileId = UserProfile#user_profile.id,
            DataRecords = data_record:from_maps(util:from_json(DataRecordsJSON)),
            case get_data_collection(DataCollectionId, UserProfileId) of
                {ok, #data_collection{unique = Unique}} ->
                    case Unique of
                        true ->
                            ok = merge(DataCollectionId, UserProfileId, DataRecords),
                            ok;
                        false ->
                            ok = load(DataCollectionId, UserProfileId, DataRecords),
                            ok
                    end;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
        #{} ->
            {bad_request, "Missing data_records"}
    end.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataCollectionIdStr, #{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := Name, unique := Unique}
          when Unique == <<"true">>; Unique == <<"false">> ->
            DataCollectionId = list_to_integer(DataCollectionIdStr),
            UserProfileId = UserProfile#user_profile.id,
            case get_data_collection(DataCollectionId, UserProfileId) of
                {ok, DataCollection} ->
                    NewDataCollection = DataCollection#data_collection{
                                          name = Name,
                                          unique = case Unique of
                                                       <<"true">> ->
                                                           true;
                                                       <<"false">> ->
                                                           false
                                                   end},
                    ok = update_data_collection(NewDataCollection),
                    ok;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
        #{name := _, unique := _} ->
            {bad_request, "Invalid unique (boolean)"};
        #{name := _} ->
            {bad_request, "Missing unique"};
        #{} ->
            {bad_request, "Missing name"}
    end.

get_data_collections(UserProfileId) ->
    worker_sup:run(data_collection_sup,
                   fun (Pid) ->
                           data_collection_server:get_data_collections(Pid, UserProfileId)
                   end).

get_data_collection(DataCollectionId, UserProfileId) ->
    worker_sup:run(data_collection_sup,
                   fun (Pid) ->
                           data_collection_server:get_data_collection(
                             Pid,
                             DataCollectionId,
                             UserProfileId)
                   end).

create_data_collection(UserProfileId, NewDataCollection) ->
    worker_sup:run(data_collection_sup,
                   fun (Pid) ->
                           data_collection_server:create_data_collection(
                             Pid,
                             UserProfileId,
                             NewDataCollection)
                   end).

update_data_collection(NewDataCollection) ->
    worker_sup:run(data_collection_sup,
                   fun (Pid) ->
                           data_collection_server:update_data_collection(Pid, NewDataCollection)
                   end).

load(DataCollectionId, UserProfileId, DataRecords) ->
    worker_sup:run(loading_sup,
                   fun (Pid) ->
                           loading_server:load(Pid,
                                               DataCollectionId,
                                               UserProfileId,
                                               DataRecords)
                   end).

merge(DataCollectionId, UserProfileId, DataRecords) ->
    worker_sup:run(loading_sup,
                   fun (Pid) ->
                           loading_server:merge(Pid,
                                                DataCollectionId,
                                                UserProfileId,
                                                DataRecords)
                   end).

get_user_data(DataCollectionId) ->
    worker_sup:run(user_data_sup,
                   fun (Pid) ->
                           user_data_server:get_user_data(Pid, DataCollectionId)
                   end).

get_user_data(DataCollectionId, Key) ->
    worker_sup:run(user_data_sup,
                   fun (Pid) ->
                           user_data_server:get_user_data(Pid, DataCollectionId, Key)
                   end).
