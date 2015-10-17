-module(transformation).
-export([transform/2]).

-include("data_type.hrl").

transform(DataTypeId, Data) ->
    Matchers = data_type:get_matchers(DataTypeId),
    GetMatches = fun (Matcher) -> get_matches(Matcher, Data) end,
    lists:flatmap(GetMatches, Matchers).

attribute_match(MatchSpecs, MatchValues) ->
    NumberedValues = maps:from_list(lists:zip(lists:seq(0, length(MatchValues) - 1), MatchValues)),
    GetNameValuePair = fun (#data_match_spec{group_name = GroupName,
                                             group_number = GroupNumber}) ->
                               {GroupName, maps:get(GroupNumber, NumberedValues)}
                       end,
    maps:from_list(lists:map(GetNameValuePair, MatchSpecs)).

get_matches(#data_matcher{regex = Regex,
                          key_match_spec = KeyMatchSpec,
                          value_match_specs = ValueMatchSpecs}, Data) ->
    case re:run(Data, Regex, [global, {capture, all, list}]) of
        {match, Matches} ->
            MatchSpecs = [KeyMatchSpec | ValueMatchSpecs],
            [attribute_match(MatchSpecs, MatchValues) || MatchValues <- Matches];
        nomatch ->
            []
    end.
