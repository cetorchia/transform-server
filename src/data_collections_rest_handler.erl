-module(data_collections_rest_handler).
-export([post/1]).
-export([post/3]).
-export([put/2]).
-export([get/1]).
-export([get/2]).
-export([get/3]).

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

get(DataCollectionIdStr, "data", #{data := QueryData, auth_user_profile := UserProfile}) ->
    DataCollectionId = list_to_integer(DataCollectionIdStr),
    UserProfileId = UserProfile#user_profile.id,
    case get_data_collection(DataCollectionId, UserProfileId) of
        {ok, _} ->
            case QueryData of
                #{key := Key} ->
                    {ok, UserData} = get_user_data(DataCollectionId, Key),
                    {ok, json, user_data:to_maps(UserData)};
                #{} ->
                    {ok, UserData} = get_user_data(DataCollectionId),
                    {ok, json, user_data:to_maps(UserData)}
            end;
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

post(#{auth_user_profile := undefined}) ->
    unauthorized;

post(#{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := Name} ->
            UserProfileId = UserProfile#user_profile.id,
            NewDataCollection = #data_collection{name = Name},
            {ok, DataCollection} = create_data_collection(UserProfileId, NewDataCollection),
            {ok, json, data_collection:to_map(DataCollection)};
        #{} ->
            {bad_request, "Missing name"}
    end.

post(_, "data", #{auth_user_profile := undefined}) ->
    unauthorized;

post(DataCollectionIdStr, "data", #{data := Data, auth_user_profile := UserProfile}) ->
    case Data of
        #{data_records := DataRecordsJSON, merge := Merge} ->
            DataCollectionId = list_to_integer(DataCollectionIdStr),
            UserProfileId = UserProfile#user_profile.id,
            DataRecords = data_record:from_maps(util:from_json(DataRecordsJSON)),
            case get_data_collection(DataCollectionId, UserProfileId) of
                {ok, _} ->
                    case Merge of
                        <<"true">> ->
                            ok = merge(DataCollectionId, UserProfileId, DataRecords),
                            ok;
                        <<"false">> ->
                            ok = load(DataCollectionId, UserProfileId, DataRecords),
                            ok;
                        _ ->
                            {bad_request, "Invalid merge"}
                    end;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
        #{merge := _} ->
            {bad_request, "Missing data_records"};
        #{} ->
            {bad_request, "Missing merge"}
    end.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataCollectionIdStr, #{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := Name} ->
            DataCollectionId = list_to_integer(DataCollectionIdStr),
            UserProfileId = UserProfile#user_profile.id,
            case get_data_collection(DataCollectionId, UserProfileId) of
                {ok, DataCollection} ->
                    NewDataCollection = DataCollection#data_collection{name = Name},
                    ok = update_data_collection(NewDataCollection),
                    ok;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
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
