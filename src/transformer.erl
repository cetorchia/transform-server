-module(transformer).
-import(rbc, [transaction_history/1]).
-export([transform/2]).

transform(InputType, Data) ->
    case InputType of
        rbc_transaction_history ->
            rbc:transaction_history(Data)
    end.
