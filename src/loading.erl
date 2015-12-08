-module(loading).
-export([load/3]).
-export([merge/3]).

-include("user_data.hrl").
-include("data_record.hrl").

load(DataCollectionId, UserProfileId, [DataRecord|Rest]) ->
    #data_record{key = Key, data = Data} = DataRecord,
    Id = mnesia:dirty_update_counter(counter, user_data_id, 1),
    UserDataKey = #user_data_key{data_collection_id = DataCollectionId,
                                 key = Key},
    UpdatedDateTime = calendar:universal_time(),
    Record = #user_data{id = Id,
                        data_collection_id = DataCollectionId,
                        user_profile_id = UserProfileId,
                        key = UserDataKey,
                        updated = UpdatedDateTime,
                        data = Data},
    ok = mnesia:dirty_write(user_data, Record),
    load(DataCollectionId, UserProfileId, Rest);

load(_, _, []) ->
    ok.

merge(DataCollectionId, UserProfileId, [DataRecord|Rest]) ->
    #data_record{key = Key, data = Data} = DataRecord,
    UserDataKey = #user_data_key{data_collection_id = DataCollectionId,
                                 key = Key},
    case mnesia:dirty_index_read(user_data, UserDataKey, #user_data.key) of
        [] ->
            Id = mnesia:dirty_update_counter(counter, user_data_id, 1),
            UpdatedDateTime = calendar:universal_time(),
            Record = #user_data{id = Id,
                                data_collection_id = DataCollectionId,
                                user_profile_id = UserProfileId,
                                key = UserDataKey,
                                updated = UpdatedDateTime,
                                data = Data},
            ok = mnesia:dirty_write(user_data, Record);
        Records when is_list(Records) ->
            ok = merge_existing(Records, Data)
    end,
    merge(DataCollectionId, UserProfileId, Rest);

merge(_, _, []) ->
    ok.

merge_existing([Record|Rest], Data) ->
    NewData = maps:merge(Record#user_data.data, Data),
    UpdatedDateTime = calendar:universal_time(),
    NewRecord = Record#user_data{updated = UpdatedDateTime,
                                 data = NewData},
    ok = mnesia:dirty_write(user_data, NewRecord),
    merge_existing(Rest, Data);

merge_existing([], _) ->
    ok.
