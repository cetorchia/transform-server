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

-module(transformation).
-export([transform/2]).

-include("data_type.hrl").
-include("data_record.hrl").

transform(DataTypeId, InputData) ->
    [DataType] = mnesia:dirty_read(data_type, DataTypeId),
    #data_type{matchers = Matchers, unique = Unique} = DataType,
    DataRecords = transform_with_matchers(Matchers, InputData),
    case Unique of
        true ->
            {ok, data_record:merge(DataRecords)};
        false ->
            {ok, DataRecords}
    end.

transform_with_matchers([Matcher|RestOfMatchers], InputData) ->
    #data_matcher{regex = Regex,
                  key_match_spec = KeyMatchSpec,
                  value_match_specs = ValueMatchSpecs} = Matcher,
    DataRecords = case re:run(InputData, Regex, [global, {capture, all, list}]) of
                      {match, Matches} ->
                          attribute_matches(KeyMatchSpec, ValueMatchSpecs, Matches);
                      nomatch ->
                          []
                  end,
    DataRecords ++ transform_with_matchers(RestOfMatchers, InputData);

transform_with_matchers([], _) ->
    [].

attribute_matches(KeyMatchSpec, ValueMatchSpecs, [MatchValues|RestOfMatches]) ->
    MatchSpecs = [KeyMatchSpec | ValueMatchSpecs],
    NumberedValues = maps:from_list(lists:zip(lists:seq(0, length(MatchValues) - 1), MatchValues)),
    #data_match_spec{group_number = KeyGroupNumber} = KeyMatchSpec,
    Key = list_to_binary(maps:get(KeyGroupNumber, NumberedValues)),
    GetNameValuePair = fun (#data_match_spec{group_name = GroupName,
                                             group_number = GroupNumber}) ->
                               {GroupName, list_to_binary(maps:get(GroupNumber, NumberedValues))}
                       end,
    Data = maps:from_list(lists:map(GetNameValuePair, MatchSpecs)),
    DataRecord = #data_record{key = Key, data = Data},
    [DataRecord|attribute_matches(KeyMatchSpec, ValueMatchSpecs, RestOfMatches)];

attribute_matches(_, _, []) ->
    [].
