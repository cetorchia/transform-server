-module(transform).
-export([start/0,init/0]).

start() ->
    application:start(mnesia),
    db:ensure_loaded(),
    application:start(transform).

init() ->
    db:create_schema(),
    application:start(mnesia),
    db:create_tables().
