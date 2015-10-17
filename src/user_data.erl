-module(user_data).
-export([create_table/0]).

-include("user_data.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(user_data,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, user_data)},
                                        {index, [data_type_id, user_id, key]}]),
    ok.
