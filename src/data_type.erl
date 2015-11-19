-module(data_type).
-export([create_table/0, to_json/1, create_data_type/1, get_matchers/1]).

-include("data_type.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(data_type,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, data_type)},
                                        {index, [user_profile_id, name]}]),
    ok.

to_json(#data_type{id = Id,
                   user_profile_id = UserProfileId,
                   name = Name,
                   matchers = Matchers}) ->
    mochijson2:encode({struct, [{id, Id},
                                {user_profile_id, UserProfileId},
                                {name, list_to_binary(Name)},
                                {matchers, Matchers}]}).

create_data_type(#{name := Name,
                   user_profile_id := UserProfileId}) ->
    DataTypeId = mnesia:dirty_update_counter(counter, data_type_id, 1),
    DataType = #data_type{id = DataTypeId,
                          user_profile_id = UserProfileId,
                          name = Name},
    ok = mnesia:dirty_write(DataType),
    {ok, DataType}.

get_matchers(DataTypeId) ->
    [DataType] = mnesia:dirty_read(data_type, DataTypeId),
    #data_type{matchers = Matchers} = DataType,
    Matchers.
