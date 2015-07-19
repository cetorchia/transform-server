#!/usr/bin/env escript
%%! -sname transformer_client_node
%%
%% Wrapper for transformer client
%%

-import(transformer, [transform/2]).
-import(transformer_client, [start/4]).
-import(loader, [load/2]).

main([Input_Format_Name, Input_Filename]) ->
    Input_Format = list_to_atom(Input_Format_Name),
    {ok, Data} = file:read_file(Input_Filename),
    Transformed_Data = transformer:transform(Input_Format, Data),
    loader:load(Input_Format, Transformed_Data),
    ok;

main([Worker_Node_Name, Input_Format_Name, Input_Filename]) ->
    Worker_Node = list_to_atom(Worker_Node_Name),
    Input_Format = list_to_atom(Input_Format_Name),
    {ok, Data} = file:read_file(Input_Filename),
    Transformed_Data = transformer_client:transform(Worker_Node, Input_Format, Data),
    loader:load(Input_Format, Transformed_Data),
    ok.
