-module(db).
-export([create_schema/0, create_tables/0, ensure_loaded/0]).

-include("db.hrl").

create_schema() ->
    mnesia:create_schema([node()|nodes()]).

create_tables() ->
    ok = data_type:create_table(),
    ok = user_data:create_table().

ensure_loaded() ->
    ok = mnesia:wait_for_tables(?tables, 60000).
