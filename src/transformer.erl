-module(transformer).
-import(rbc, [transaction_history/1]).
-export([transform/2]).

transform(InputType, Data) ->
    {ok, Ref} = odbc:connect("DSN=transform", []),
    DataTypeName = atom_to_list(InputType),
    {selected, _, MatcherRows} = odbc:param_query(
                                "SELECT t.id, regex, group_key, item_key
                                FROM data_type t
                                JOIN data_type_matcher m ON m.data_type_id = t.id
                                WHERE t.name = ?",
                                [{{sql_varchar, 32},[DataTypeName]}]),
    [{Regex, GroupKey, ItemKey} | _] = MatcherRows,
    % TODO
    Data.
