-module(db).
-export([create_schema/0, delete_schema/0, create_tables/0, ensure_loaded/0]).

-include("db.hrl").

create_schema() ->
    ok = mnesia:create_schema([node()|nodes()]).

delete_schema() ->
    ok = mnesia:delete_schema([node()|nodes()]).

create_tables() ->
    ok = create_tables(?tables).

create_tables([Name|Rest]) ->
    ok = Name:create_table(),
    create_tables(Rest);

create_tables([]) ->
    ok.

ensure_loaded() ->
    ok = mnesia:wait_for_tables(?tables, 60000).
