-module(transform).
-export([start/0, create/0, destroy/0]).

start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(bcrypt),
    application:start(mnesia),
    db:ensure_loaded(),
    application:start(transform).

create() ->
    db:create_schema(),
    application:start(mnesia),
    db:create_tables().

destroy() ->
    db:delete_schema().
