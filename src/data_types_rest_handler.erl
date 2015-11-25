-module(data_types_rest_handler).

-export([get/1, get/2, post/1, put/2, delete/2]).

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
        #{name := Name, matchers := MatchersJSON} ->
            UserProfileId = UserProfile#user_profile.id,
            NewDataType = data_type:from_map(
                            #{name => Name,
                              matchers => util:from_json(MatchersJSON)}),
            {ok, DataType} = create_data_type(UserProfileId, NewDataType),
            {ok, json, data_type:to_map(DataType)};
        #{name := _} ->
            {bad_request, "Missing matchers"};
        #{} ->
            {bad_request, "Missing name"}
    end.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataTypeIdStr, #{data := DataTypeData, auth_user_profile := UserProfile}) ->
    case DataTypeData of
        #{name := Name, matchers := MatchersJSON} ->
            DataTypeId = list_to_integer(DataTypeIdStr),
            UserProfileId = UserProfile#user_profile.id,
            case get_data_type(DataTypeId, UserProfileId) of
                {ok, DataType} ->
                    NewDataType = DataType#data_type{
                                    name = Name,
                                    matchers = data_type:from_maps(util:from_json(MatchersJSON))},
                    ok = update_data_type(NewDataType),
                    ok;
                forbidden ->
                    forbidden;
                not_found ->
                    not_found
            end;
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
