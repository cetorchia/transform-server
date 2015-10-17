-module(transform).
-export([start/0]).

start() ->
    mnesia:start(),
    db:create_schema(),
    db:create_tables(),
    db:ensure_loaded(),
    application:start(transform).
