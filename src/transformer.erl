-module(transformer).
-export([transform/2]).

transform(InputType, Data) ->
    DataTypeName = atom_to_list(InputType),
    Matchers = get_matchers(DataTypeName),
    GetMatches = fun ({DataTypeId, Regex, GroupKey, ItemKey}) ->
                         Matches = get_matches(DataTypeId, Regex, GroupKey, ItemKey, Data),
                         attribute_matches(DataTypeId, GroupKey, ItemKey, Matches)
                 end,
    lists:flatmap(GetMatches, Matchers).

%% Returns [{DataTypeId, Regex, GroupKey, ItemKey}, ...]
get_matchers(DataTypeName) ->
    {ok, Ref} = odbc:connect("DSN=transform", []),
    {selected, _, MatcherRows} = odbc:param_query(Ref,
                                "SELECT t.id, regex, group_key, item_key
                                FROM data_type t
                                JOIN data_type_matcher m ON m.data_type_id = t.id
                                WHERE t.name = ?",
                                [{{sql_varchar, 32},[DataTypeName]}]),
    MatcherRows.

attribute_matches(DataTypeId, GroupKey, ItemKey, Matches) ->
    [Match#{data_type_id => DataTypeId,
            group_key => GroupKey,
            item_key => ItemKey} || Match <- Matches].

get_matches(Regex, Data) ->
    case re:run(Data, Regex, [global, {capture, ['group', 'item'], list}]) of
        {match, Matches} ->
            [#{group_value => GroupValue,
               item_value => ItemValue} || [GroupValue, ItemValue] <- Matches];
        nomatch ->
            []
    end.
