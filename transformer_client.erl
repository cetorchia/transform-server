-module(transformer_client).
-export([transform/3]).

transform(Worker_Node, Input_Format, Data) ->
    {transformer_worker, Worker_Node} ! {transform, Input_Format, Data, self()},
    receive
        {transformation_result, Transformed_Data} ->
            Transformed_Data
    end.
