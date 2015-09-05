-module(loader).
-export([load/2]).

load(Data) ->
    {ok, Ref} = odbc:connect("DSN=transform", []),
    DataTypeIds = [DataTypeId || #{data_type_id := DataTypeId} <- Data],
    GroupKeys = [GroupKey || #{group_key := GroupKey} <- Data],
    GroupValues = [GroupValue || #{group_value := GroupValue} <- Data],
    ItemKeys = [ItemKey || #{item_key := ItemKey} <- Data],
    ItemValues = [ItemValue || #{item_value := ItemValue} <- Data],
    {updated, length(Data)} = odbc:param_query(
      "INSERT INTO raw_data(data_type_id, group_key, group_value, item_key, item_value)
      VALUES (?, ?, ?, ?, ?)",
      [{sql_integer, DataTypeIds},
       {{sql_varchar, 32}, GroupKeys},
       {{sql_varchar, 64}, GroupValue},
       {{sql_varchar, 32}, ItemKeys},
       {{sql_varchar, 256}, ItemValues}]),
    odbc:disconnect(Ref),
    ok.
