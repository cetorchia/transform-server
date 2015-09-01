-module(loader).
-export([load/2]).

load(InputType, Data) ->
    io:format("TODO: For now, only displaying data instead of loading it.~n"),
    io:format("~p~p~n", [InputType, Data]).
