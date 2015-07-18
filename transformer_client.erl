-module(transformer_client).
-export([start/5, client_process/5]).

client_process(Worker_Node, Input_Format, Output_Format, Input_Filename, Output_Filename) ->
    {ok, Data} = file:read_file(Input_Filename),
    {transformer_worker, Worker_Node} ! {transform, Input_Format, Output_Format, Data, self()},
    io:format("Sent transform request, waiting for result~n", []),
    receive
        {transformation_result, Transformed_Data} ->
            ok = file:write_file(Output_Filename, Transformed_Data),
            io:format("Wrote ~p~n", [Output_Filename])
    end,
    io:format("Done waiting for result~n", []),
    ok.

start(Worker_Node, Input_Format, Output_Format, Input_Filename, Output_Filename) ->
    spawn(transformer_client, client_process,
          [Worker_Node, Input_Format, Output_Format, Input_Filename, Output_Filename]).
