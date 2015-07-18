#!/usr/bin/env escript
%%! -sname transformer_client_node
%%
%% Wrapper for transformer worker
%%

-import(transformer, [transform/3]).
-import(transformer_worker, [start/0]).
-import(transformer_client, [start/5]).

main([Input_Format_Name, Output_Format_Name, Input_Filename, Output_Filename]) ->
    Input_Format = list_to_atom(Input_Format_Name),
    Output_Format = list_to_atom(Output_Format_Name),
    io:format("Reading ~p~n", [Input_Filename]),
    {ok, Data} = file:read_file(Input_Filename),
    Transformed_Data = transformer:transform(Input_Format, Output_Format, Data),
    ok = file:write_file(Output_Filename, Transformed_Data),
    io:format("Wrote ~p~n", [Output_Filename]),
    ok;

main([Worker_Node_Name, Input_Format_Name, Output_Format_Name, Input_Filename, Output_Filename]) ->
    Worker_Node = list_to_atom(Worker_Node_Name),
    Input_Format = list_to_atom(Input_Format_Name),
    Output_Format = list_to_atom(Output_Format_Name),
    transformer_client:start(Worker_Node, Input_Format, Output_Format, Input_Filename, Output_Filename),
    timer:sleep(5000).
