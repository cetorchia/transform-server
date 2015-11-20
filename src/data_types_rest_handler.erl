-module(data_types_rest_handler).

-export([get/1, get/2, post/1]).

-include("rest.hrl").
-include("user_profile.hrl").

get(Req) ->
    case rest:authenticate(Req) of
        {ok, UserProfile} ->
            UserProfileId = UserProfile#user_profile.id,
            GetDataTypes = fun (Pid) ->
                                   data_type_server:get_data_types_by_user(Pid, UserProfileId)
                           end,
            {ok, DataTypes} = worker_sup:run(data_type_sup, GetDataTypes),
            Req:ok({"application/json", data_type:to_json(DataTypes)});
        error ->
            Req:respond({401, [], []})
    end.

get(DataTypeIdStr, Req) ->
    case rest:authenticate(Req) of
        {ok, UserProfile} ->
            DataTypeId = list_to_integer(DataTypeIdStr),
            UserProfileId = UserProfile#user_profile.id,
            GetDataTypes =
            fun (Pid) ->
                    data_type_server:get_data_type_by_user(Pid, DataTypeId, UserProfileId)
            end,
            case worker_sup:run(data_type_sup, GetDataTypes) of
                {ok, DataType} ->
                    Req:ok({"application/json", data_type:to_json(DataType)});
                not_found ->
                    Req:not_found()
            end;
        error ->
            Req:respond({401, [], []})
    end.

post(Req) ->
    case rest:authenticate(Req) of
        {ok, UserProfile} ->
            Parameters = Req:parse_post(),
            DataTypeData = #{name => proplists:get_value("name", Parameters),
                             user_profile_id => UserProfile#user_profile.id},
            case DataTypeData of
                #{name := undefined} ->
                    Req:respond({400, [?text_plain], "Missing name"});
                _ ->
                    Create = fun (Pid) ->
                                     data_type_server:create_data_type(Pid, DataTypeData)
                             end,
                    {ok, DataType} = worker_sup:run(data_type_sup, Create),
                    Req:ok({"application/json", data_type:to_json(DataType)})
            end;
        error ->
            Req:respond({401, [], []})
    end.
