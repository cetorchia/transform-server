-module(data_collection).
-export([create_table/0]).
-export([create_data_collection/1]).
-export([update_data_collection/2]).
-export([get_data_collections/1]).
-export([get_data_collection/2]).

-include("data_collection.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(data_collection,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, data_collection)},
                                        {index, [user_profile_id]}]),
    ok.

create_data_collection(#{name := Name, user_profile_id:= UserProfileId}) ->
    DataCollectionId = mnesia:dirty_update_counter(counter, data_collection_id, 1),
    DataCollection = #data_collection{id = DataCollectionId,
                                      user_profile_id = UserProfileId,
                                      name = Name},
    ok = mnesia:dirty_write(DataCollection),
    {ok, to_map(DataCollection)}.

update_data_collection(DataCollectionId, #{name := Name, user_profile_id:= UserProfileId}) ->
    case mnesia:dirty_read(data_collection, DataCollectionId) of
        [#data_collection{user_profile_id = UserProfileId}] ->
            DataCollection = #data_collection{id = DataCollectionId,
                                              user_profile_id = UserProfileId,
                                              name = Name},
            ok = mnesia:dirty_write(DataCollection),
            {ok, to_map(DataCollection)};
        [#data_collection{user_profile_id = _}] ->
            forbidden;
        [] ->
            not_found
    end.

get_data_collections(UserProfileId) ->
    DataCollections = mnesia:dirty_index_read(data_collection,
                                              UserProfileId,
                                              #data_collection.user_profile_id),
    {ok, to_maps(DataCollections)}.

get_data_collection(DataCollectionId, UserProfileId) ->
    case mnesia:dirty_read(data_collection, DataCollectionId) of
        [#data_collection{user_profile_id = UserProfileId}] = DataCollection ->
            {ok, to_map(DataCollection)};
        [#data_collection{user_profile_id = _}] ->
            forbidden;
        [] ->
            not_found
    end.

to_maps([]) ->
    [];

to_maps([DataCollection|Rest]) ->
    [to_map(DataCollection)|to_maps(Rest)].

to_map(#data_collection{id = Id, name = Name, user_profile_id = UserProfileId}) ->
    #{id => Id,
      name => Name,
      user_profile_id => UserProfileId}.
