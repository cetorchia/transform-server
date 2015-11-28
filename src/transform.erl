-module(transform).
-export([start/0, create/0, destroy/0]).

start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(bcrypt),
    application:start(inets),
    ssl:start(),
    application:start(mnesia),
    db:ensure_loaded(),
    application:start(transform).

create() ->
    application:start(sasl),
    db:create_schema(),
    application:start(mnesia),
    db:create_tables().

destroy() ->
    application:start(sasl),
    db:delete_schema().
