-module(transformer_worker).
-import(transformer, [transform/2]).
-export([start/0, worker_process/0, stop/0]).

worker_process() ->
    io:format("Waiting for requests~n", []),
    receive
        {transform, Input_Format, Data, Client_PID} ->
            Client_PID ! {transformation_result, transform(Input_Format, Data)},
            io:format("Sent result to ~w~n", [Client_PID]);
        Invalid ->
            io:format("Received invalid message ~w~n", Invalid)
    end,
    worker_process().

stop() ->
    exit(whereis(transformer_worker), ok).

start() ->
    register(transformer_worker, spawn(transformer_worker, worker_process, [])).
