-module(loading).
-export([load/4]).

-include("user_data.hrl").
-include("data_record.hrl").

load(DataCollectionId, DataTypeId, UserProfileId, [DataRecord|Rest]) ->
    #data_record{key_name = KeyName,
                 key_value = KeyValue,
                 data = Data} = DataRecord,
    Id = mnesia:dirty_update_counter(counter, user_data_id, 1),
    Key = #user_data_key{data_collection_id = DataCollectionId,
                         key_name = KeyName,
                         key_value = KeyValue},
    UpdatedDateTime = calendar:universal_time(),
    Record = #user_data{id = Id,
                        data_collection_id = DataCollectionId,
                        data_type_id = DataTypeId,
                        user_profile_id = UserProfileId,
                        key = Key,
                        updated = UpdatedDateTime,
                        data = Data},
    ok = mnesia:dirty_write(user_data, Record),
    load(DataCollectionId, DataTypeId, UserProfileId, Rest);

load(_, _, _, []) ->
    ok.
