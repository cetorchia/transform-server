-module(data_collections_rest_handler).
-export([post/1]).
-export([put/2]).
-export([get/1]).
-export([get/2]).

-include("user_profile.hrl").

get(#{auth_user_profile := undefined}) ->
    unauthorized;

get(#{auth_user_profile := UserProfile}) ->
    UserProfileId = UserProfile#user_profile.id,
    GetDataCollections = fun (Pid) ->
                                 data_collection_server:get_data_collections(Pid, UserProfileId)
                         end,
    {ok, DataCollections} = worker_sup:run(data_collection_sup, GetDataCollections),
    {ok, json, DataCollections}.

get(_, #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataCollectionIdStr, #{auth_user_profile := UserProfile}) ->
    DataCollectionId = list_to_integer(DataCollectionIdStr),
    UserProfileId = UserProfile#user_profile.id,
    GetDataCollection =
    fun (Pid) ->
            data_collection_server:get_data_collection(Pid, DataCollectionId, UserProfileId)
    end,
    case worker_sup:run(data_collection_sup, GetDataCollection) of
        {ok, DataCollection} ->
            {ok, json, DataCollection};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

post(#{auth_user_profile := undefined}) ->
    unauthorized;

post(#{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := _} ->
            NewData = DataCollectionData#{user_profile_id => UserProfile#user_profile.id},
            Create = fun (Pid) ->
                             data_collection_server:create_data_collection(Pid, NewData)
                     end,
            {ok, DataCollection} = worker_sup:run(data_collection_sup, Create),
            {ok, json, DataCollection};
        #{} ->
            {bad_request, "Missing name"}
    end.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataCollectionIdStr, #{data := DataCollectionData, auth_user_profile := UserProfile}) ->
    case DataCollectionData of
        #{name := _} ->
            DataCollectionId = list_to_integer(DataCollectionIdStr),
            NewData = DataCollectionData#{user_profile_id => UserProfile#user_profile.id},
            Update = fun (Pid) ->
                             data_collection_server:update_data_collection(Pid,
                                                                           DataCollectionId,
                                                                           NewData)
                     end,
            case worker_sup:run(data_collection_sup, Update) of
                {ok, DataCollection} ->
                    {ok, json, DataCollection};
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
        #{} ->
            {bad_request, "Missing name"}
    end.
