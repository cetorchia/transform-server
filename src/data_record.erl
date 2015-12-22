%% This file is part of the transform server
%% Copyright (c) 2015 Carlos E. Torchia
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(data_record).
-export([to_maps/1, to_map/1, from_maps/1, from_map/1, merge/1]).

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


merge(DataRecords) ->
    lists:reverse(merge(DataRecords, [])).

merge([], DataRecords) ->
    DataRecords;

merge([DataRecord|Rest], DataRecords) ->
    #data_record{key = Key, data = Data} = DataRecord,
    MergedDataRecords =
    case lists:keymember(Key, #data_record.key, DataRecords) of
        true ->
            Merge = fun (#data_record{key = OtherKey, data = OtherData}) when OtherKey == Key ->
                            #data_record{key = Key,
                                         data = maps:merge(OtherData, Data)};
                        (OtherDataRecord) ->
                            OtherDataRecord
                    end,
            lists:map(Merge, DataRecords);
        false ->
            [DataRecord|DataRecords]
    end,
    merge(Rest, MergedDataRecords).
