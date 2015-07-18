-module(transformer).
-import(rbc, [transaction_history_csv/1]).
-export([transform/3]).

transform(Input_Format, Output_Format, Data) ->
    case {Input_Format, Output_Format} of
        {rbc_transaction_history, csv} ->
            rbc:transaction_history_csv(Data)
    end.
