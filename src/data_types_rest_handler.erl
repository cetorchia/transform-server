-module(data_types_rest_handler).

-export([get/1, get/2, post/1, put/2, delete/2]).

-include("user_profile.hrl").

get(#{auth_user_profile := undefined}) ->
    unauthorized;

get(#{auth_user_profile := UserProfile}) ->
    UserProfileId = UserProfile#user_profile.id,
    GetDataTypes = fun (Pid) ->
                           data_type_server:get_data_types(Pid, UserProfileId)
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
            data_type_server:get_data_type(Pid, DataTypeId, UserProfileId)
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
        #{name := _, matchers := MatchersJSON} ->
            NewDataTypeData = DataTypeData#{user_profile_id => UserProfile#user_profile.id,
                                            matchers => util:from_json(MatchersJSON)},
            Create = fun (Pid) ->
                             data_type_server:create_data_type(Pid, NewDataTypeData)
                     end,
            {ok, DataType} = worker_sup:run(data_type_sup, Create),
            {ok, json, DataType};
        #{name := _} ->
            {bad_request, "Missing matchers"};
        #{} ->
            {bad_request, "Missing name"}
    end.

put(_, #{auth_user_profile := undefined}) ->
    unauthorized;

put(DataTypeIdStr, #{data := DataTypeData, auth_user_profile := UserProfile}) ->
    case DataTypeData of
        #{name := _, matchers := MatchersJSON} ->
            DataTypeId = list_to_integer(DataTypeIdStr),
            NewDataTypeData = DataTypeData#{user_profile_id => UserProfile#user_profile.id,
                                            matchers => util:from_json(MatchersJSON)},
            Update = fun (Pid) ->
                             data_type_server:update_data_type(Pid, DataTypeId, NewDataTypeData)
                     end,
            case worker_sup:run(data_type_sup, Update) of
                {ok, DataType} ->
                    {ok, json, DataType};
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
    Delete = fun (Pid) ->
                     data_type_server:delete_data_type(Pid, DataTypeId, UserProfileId)
             end,
    case worker_sup:run(data_type_sup, Delete) of
        ok ->
            ok;
        forbidden ->
            forbidden;
        not_found ->
            not_found
    end.
