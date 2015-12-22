%% This file is part of the transform server
%% Copyright (c) 2015 Carlos E. Torchia
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(data_collection).
-export([create_table/0]).
-export([create_data_collection/2]).
-export([update_data_collection/1]).
-export([get_data_collections/1]).
-export([get_data_collection/2]).
-export([to_maps/1, to_map/1]).

-include("data_collection.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(data_collection,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, data_collection)},
                                        {index, [user_profile_id]}]),
    ok.

create_data_collection(UserProfileId, DataCollection) ->
    DataCollectionId = mnesia:dirty_update_counter(counter, data_collection_id, 1),
    NewDataCollection = DataCollection#data_collection{id = DataCollectionId,
                                                       user_profile_id = UserProfileId},
    ok = mnesia:dirty_write(NewDataCollection),
    {ok, NewDataCollection}.

update_data_collection(#data_collection{id = Id, user_profile_id = UserProfileId} = DataCollection)
  when Id /= undefined, UserProfileId /= undefined ->
    ok = mnesia:dirty_write(DataCollection),
    ok.

get_data_collections(UserProfileId) ->
    DataCollections = mnesia:dirty_index_read(data_collection,
                                              UserProfileId,
                                              #data_collection.user_profile_id),
    {ok, DataCollections}.

get_data_collection(DataCollectionId, UserProfileId) ->
    case mnesia:dirty_read(data_collection, DataCollectionId) of
        [#data_collection{user_profile_id = UserProfileId} = DataCollection] ->
            {ok, DataCollection};
        [#data_collection{user_profile_id = _}] ->
            forbidden;
        [] ->
            not_found
    end.

to_maps([]) ->
    [];

to_maps([DataCollection|Rest]) ->
    [to_map(DataCollection)|to_maps(Rest)].

to_map(#data_collection{id = Id,
                        name = Name,
                        user_profile_id = UserProfileId,
                        unique = Unique}) ->
    #{id => Id,
      name => Name,
      user_profile_id => UserProfileId,
      unique => Unique}.
