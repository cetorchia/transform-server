-module(data_record).
-export([to_maps/1, to_map/1, from_maps/1, from_map/1]).

-include("data_record.hrl").

to_maps([]) ->
    [];

to_maps([DataRecord|Rest]) when is_record(DataRecord, data_record) ->
    [to_map(DataRecord)|to_maps(Rest)].

to_map(#data_record{key = Key, data = Data}) when is_map(Data) ->
    #{key => Key, data => Data}.

from_maps([]) ->
    [];

from_maps([Map|Rest]) when is_map(Map) ->
    [from_map(Map)|from_maps(Rest)].

from_map(#{key := Key, data := Data}) when is_map(Data) ->
    #data_record{key = Key, data = Data}.
