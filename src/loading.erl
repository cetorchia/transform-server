-module(loading).
-export([load/3]).
-export([merge/3]).

-include("user_data.hrl").
-include("data_record.hrl").

load(DataCollectionId, UserProfileId, [DataRecord|Rest]) ->
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
                        user_profile_id = UserProfileId,
                        key = Key,
                        updated = UpdatedDateTime,
                        data = Data},
    ok = mnesia:dirty_write(user_data, Record),
    load(DataCollectionId, UserProfileId, Rest);

load(_, _, []) ->
    ok.

merge(DataCollectionId, UserProfileId, [DataRecord|Rest]) ->
    #data_record{key_name = KeyName,
                 key_value = KeyValue,
                 data = Data} = DataRecord,
    Key = #user_data_key{data_collection_id = DataCollectionId,
                         key_name = KeyName,
                         key_value = KeyValue},
    UpdatedDateTime = calendar:universal_time(),
    case mnesia:dirty_index_read(user_data, Key, #user_data.key) of
        [] ->
            Id = mnesia:dirty_update_counter(counter, user_data_id, 1),
            Record = #user_data{id = Id,
                                data_collection_id = DataCollectionId,
                                user_profile_id = UserProfileId,
                                key = Key,
                                updated = UpdatedDateTime,
                                data = Data},
            ok = mnesia:dirty_write(user_data, Record);
        Records when is_list(Records) ->
            ok = merge_existing(Records, Data, UpdatedDateTime)
    end,
    merge(DataCollectionId, UserProfileId, Rest);

merge(_, _, []) ->
    ok.

merge_existing([Record|Rest], Data, UpdatedDateTime) ->
    NewData = maps:merge(Record#user_data.data, Data),
    NewRecord = Record#user_data{updated = UpdatedDateTime,
                                 data = NewData},
    ok = mnesia:dirty_write(user_data, NewRecord),
    merge_existing(Rest, Data, UpdatedDateTime);

merge_existing([], _, _) ->
    ok.
