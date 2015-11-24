-module(data_collection).
-export([create_table/0]).

-include("data_collection.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(data_collection,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, data_collection)},
                                        {index, [user_profile_id]}]),
    ok.
