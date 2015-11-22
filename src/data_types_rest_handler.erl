-module(data_types_rest_handler).

-export([get/1, get/2, post/1]).

-include("user_profile.hrl").

get(#{auth_user_profile := undefined}) ->
    unauthorized;

get(#{auth_user_profile := UserProfile}) ->
    UserProfileId = UserProfile#user_profile.id,
    GetDataTypes = fun (Pid) ->
                           data_type_server:get_data_types_by_user(Pid, UserProfileId)
                   end,
    {ok, DataTypes} = worker_sup:run(data_type_sup, GetDataTypes),
    {ok, json, DataTypes}.

get(_, #{auth_user_profile := undefined}) ->
    unauthorized;

get(DataTypeIdStr, #{auth_user_profile := UserProfile}) ->
    DataTypeId = list_to_integer(DataTypeIdStr),
    UserProfileId = UserProfile#user_profile.id,
    GetDataType =
    fun (Pid) ->
            data_type_server:get_data_type_by_user(Pid, DataTypeId, UserProfileId)
    end,
    case worker_sup:run(data_type_sup, GetDataType) of
        {ok, DataType} ->
            {ok, json, DataType};
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.

post(#{auth_user_profile := undefined}) ->
    unauthorized;

post(#{data := DataTypeData, auth_user_profile := UserProfile}) ->
    case DataTypeData of
        #{name := _} ->
            NewDataTypeData = DataTypeData#{user_profile_id => UserProfile#user_profile.id},
            Create = fun (Pid) ->
                             data_type_server:create_data_type(Pid, NewDataTypeData)
                     end,
            {ok, DataType} = worker_sup:run(data_type_sup, Create),
            {ok, json, DataType};
        #{} ->
            {bad_request, "Missing name"}
    end.
