-module(loading).
-export([load/3]).

-include("user_data.hrl").
-include("data_record.hrl").

load(DataTypeId, UserId, [DataRecord|Rest]) ->
    #data_record{key_name = KeyName,
                 key_value = KeyValue,
                 data = Data} = DataRecord,
    Key = #user_data_key{key_name = KeyName,
                         key_value = KeyValue},
    Id = #user_data_id{data_type_id = DataTypeId,
                       user_id = UserId,
                       key = Key},
    UpdatedDateTime = calendar:universal_time(),
    Record = #user_data{id = Id,
                        data_type_id = DataTypeId,
                        user_id = UserId,
                        key = Key,
                        updated = UpdatedDateTime,
                        data = Data},
    ok = mnesia:dirty_write(user_data, Record),
    load(DataTypeId, UserId, Rest);

load(_, _, []) ->
    ok.
