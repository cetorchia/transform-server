-module(transformation).
-export([transform/2]).

-include("data_type.hrl").
-include("data_record.hrl").

transform(DataTypeId, InputData) ->
    Matchers = data_type:get_matchers(DataTypeId),
    DataRecords = transform_with_matchers(Matchers, InputData),
    {ok, DataRecords}.

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
    #data_match_spec{group_name = KeyName} = KeyMatchSpec,
    #data_match_spec{group_number = KeyGroupNumber} = KeyMatchSpec,
    KeyValue = list_to_binary(maps:get(KeyGroupNumber, NumberedValues)),
    GetNameValuePair = fun (#data_match_spec{group_name = GroupName,
                                             group_number = GroupNumber}) ->
                               {GroupName, list_to_binary(maps:get(GroupNumber, NumberedValues))}
                       end,
    Data = maps:from_list(lists:map(GetNameValuePair, MatchSpecs)),
    DataRecord = #data_record{key_name = KeyName,
                              key_value = KeyValue,
                              data = Data},
    [DataRecord|attribute_matches(KeyMatchSpec, ValueMatchSpecs, RestOfMatches)];

attribute_matches(_, _, []) ->
    [].
