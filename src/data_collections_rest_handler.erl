-module(data_collections_rest_handler).
-export([post/1]).
-export([put/2]).
-export([get/1]).
-export([get/2]).

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
