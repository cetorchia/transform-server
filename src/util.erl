-module(util).

-export([normalize_map/1]).
-export([to_atom/1, to_binary/1]).
-export([to_json/1, from_json/1]).
-export([format_datetime/1]).


normalize_map(Data) when is_map(Data) ->
    maps:from_list(normalize_map(maps:to_list(Data)));

normalize_map([{Key, Value}|Rest]) ->
    [{to_atom(Key), to_binary(Value)}|normalize_map(Rest)];

normalize_map([]) ->
    [].


to_atom(Term) when is_list(Term) ->
    list_to_atom(re:replace(Term, "-", "_", [global, {return, list}]));

to_atom(Term) when is_binary(Term) ->
    binary_to_atom(Term, utf8);

to_atom(Term) when is_atom(Term) ->
    Term.


to_binary(Term) when is_list(Term) ->
    list_to_binary(Term);

to_binary(Term) when is_binary(Term) ->
    Term.


to_json(Data) ->
    mochijson2:encode(to_json_struct(Data)).

to_json_struct([]) ->
    [];

to_json_struct([Data|Rest]) when (is_list(Data) or is_map(Data)) ->
    [to_json_struct(Data)|to_json_struct(Rest)];

to_json_struct(Data) when is_map(Data) ->
    {struct, to_json_struct(maps:to_list(Data))};

to_json_struct([{Key, Value}|Rest]) ->
    [{Key, to_json_struct(Value)}|to_json_struct(Rest)];

to_json_struct(Data) ->
    Data.


from_json(JSON) ->
    from_json_struct(mochijson2:decode(JSON)).

from_json_struct([]) ->
    [];

from_json_struct([DataStruct|Rest]) when is_list(DataStruct) ->
    [from_json_struct(DataStruct)|from_json_struct(Rest)];

from_json_struct([{struct, PropList} = DataStruct|Rest]) when is_list(PropList) ->
    [from_json_struct(DataStruct)|from_json_struct(Rest)];

from_json_struct({struct, PropList}) when is_list(PropList) ->
    maps:from_list(from_json_struct(PropList));

from_json_struct([{Key, Value}|Rest]) ->
    [{to_atom(Key), from_json_struct(Value)}|from_json_struct(Rest)];

from_json_struct(Data) ->
    Data.


format_datetime({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
    iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w",
                                   [Year, Month, Day, Hours, Minutes, Seconds])).
