-module(counter).
-export([create_table/0]).

-include("counter.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(counter,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, counter)}]),
    ok.

