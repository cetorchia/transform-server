-module(transform).
-export([start/0,init/0]).

start() ->
    application:start(mnesia),
    application:start(transform).

init() ->
    db:create_schema(),
    application:start(mnesia),
    db:create_tables(),
    db:ensure_loaded().
