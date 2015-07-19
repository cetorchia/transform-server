#!/usr/bin/env escript
%%! -sname transformer_client_node
%%
%% Wrapper for transformer worker
%%

-import(transformer, [transform/3]).
-import(transformer_client, [start/4]).

main([Input_Format_Name, Input_Filename, Output_Filename]) ->
    Input_Format = list_to_atom(Input_Format_Name),
    io:format("Reading ~p~n", [Input_Filename]),
    {ok, Data} = file:read_file(Input_Filename),
    Transformed_Data = transformer:transform(Input_Format, Data),
    ok = file:write_file(Output_Filename, Transformed_Data),
    io:format("Wrote ~p~n", [Output_Filename]),
    ok;

main([Worker_Node_Name, Input_Format_Name, Input_Filename, Output_Filename]) ->
    Worker_Node = list_to_atom(Worker_Node_Name),
    Input_Format = list_to_atom(Input_Format_Name),
    io:format("Reading ~p~n", [Input_Filename]),
    {ok, Data} = file:read_file(Input_Filename),
    Transformed_Data = transformer_client:transform(Worker_Node, Input_Format, Data),
    ok = file:write_file(Output_Filename, Transformed_Data),
    io:format("Wrote ~p~n", [Output_Filename]),
    ok.
