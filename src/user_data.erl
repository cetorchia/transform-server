-module(user_data).
-export([create_table/0]).
-export([get_user_data/1]).
-export([get_user_data/3]).
-export([to_maps/1, to_map/1]).

-include("user_data.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(user_data,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, user_data)},
                                        {index, [data_collection_id, key]}]),
    ok.

get_user_data(DataCollectionId) when is_integer(DataCollectionId) ->
    UserData = mnesia:dirty_index_read(user_data, DataCollectionId, #user_data.data_collection_id),
    {ok, UserData}.

get_user_data(DataCollectionId, KeyName, KeyValue) ->
    UserDataKey = #user_data_key{data_collection_id = DataCollectionId,
                                 key_name = KeyName,
                                 key_value = KeyValue},
    UserData = mnesia:dirty_index_read(user_data, UserDataKey, #user_data.key),
    {ok, UserData}.

to_maps([]) ->
    [];

to_maps([UserDatum|Rest]) when is_record(UserDatum, user_data) ->
    [to_map(UserDatum)|to_maps(Rest)].

to_map(#user_data{id = Id,
                  user_profile_id = UserProfileId,
                  data_collection_id = DataCollectionId,
                  key = #user_data_key{data_collection_id = DataCollectionId,
                                       key_name = KeyName,
                                       key_value = KeyValue},
                  updated = Updated,
                  data = Data}) ->
    #{id => Id,
      user_profile_id => UserProfileId,
      data_collection_id => DataCollectionId,
      key_name => KeyName,
      key_value => KeyValue,
      updated => util:format_datetime(Updated),
      data => Data}.
