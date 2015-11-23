-module(data_type).
-export([create_table/0]).
-export([get_matchers/1]).
-export([create_data_type/1, get_data_types_by_user/1]).
-export([get_data_type_by_user/2]).

-include("data_type.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(data_type,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, data_type)},
                                        {index, [user_profile_id, name]}]),
    ok.

get_matchers(DataTypeId) ->
    [DataType] = mnesia:dirty_read(data_type, DataTypeId),
    #data_type{matchers = Matchers} = DataType,
    Matchers.

create_data_type(DataType) when is_map(DataType) ->
    DataTypeId = mnesia:dirty_update_counter(counter, data_type_id, 1),
    NewDataType = from_map(DataType#{id => DataTypeId}),
    ok = mnesia:dirty_write(NewDataType),
    error_logger:info_msg("~p~n", [to_map(NewDataType)]),
    {ok, to_map(NewDataType)}.

get_data_types_by_user(UserProfileId) ->
    DataTypes = mnesia:dirty_index_read(data_type, UserProfileId, #data_type.user_profile_id),
    {ok, to_maps(DataTypes)}.

get_data_type_by_user(DataTypeId, UserProfileId) ->
    case mnesia:dirty_read(data_type, DataTypeId) of
        [#data_type{user_profile_id = UserProfileId} = DataType] ->
            {ok, to_map(DataType)};
        [#data_type{user_profile_id = _}] ->
            forbidden;
        [] ->
            not_found
    end.

to_maps([DataType|Rest]) when is_record(DataType, data_type) ->
    #data_type{id = Id,
               user_profile_id = UserProfileId,
               name = Name} = DataType,
    Map = #{id => Id,
            user_profile_id => UserProfileId,
            name => Name},
    [Map|to_maps(Rest)];

to_maps([Matcher|Rest]) when is_record(Matcher, data_matcher) ->
    [to_map(Matcher)|to_maps(Rest)];

to_maps([MatchSpec|Rest]) when is_record(MatchSpec, data_match_spec) ->
    [to_map(MatchSpec)|to_maps(Rest)];

to_maps([]) ->
    [].

to_map(#data_type{id = Id,
                  user_profile_id = UserProfileId,
                  name = Name,
                  matchers = Matchers}) ->
    #{id => Id,
      user_profile_id => UserProfileId,
      name => Name,
      matchers => to_maps(Matchers)};

to_map(#data_matcher{regex = Regex,
                     key_match_spec = KeyMatchSpec,
                     value_match_specs = ValueMatchSpecs}) ->
    #{regex => Regex,
      key_match_spec => to_map(KeyMatchSpec),
      value_match_specs => to_maps(ValueMatchSpecs)};

to_map(#data_match_spec{group_name = GroupName,
                        group_number = GroupNumber}) ->
    #{group_name => GroupName,
      group_number => GroupNumber}.

from_maps([Map|Rest]) when is_map(Map) ->
    [from_map(Map)|from_maps(Rest)];

from_maps([]) ->
    [].

from_map(#{id := DataTypeId,
           name := Name,
           user_profile_id := UserProfileId,
           matchers := Matchers}) ->
    #data_type{id = DataTypeId,
               user_profile_id = UserProfileId,
               name = Name,
               matchers = from_maps(Matchers)};

from_map(#{regex := Regex,
           key_match_spec := KeyMatchSpec,
           value_match_specs := ValueMatchSpecs}) ->
    #data_matcher{regex = Regex,
                  key_match_spec = from_map(KeyMatchSpec),
                  value_match_specs = from_maps(ValueMatchSpecs)};

from_map(#{group_name := GroupName,
           group_number := GroupNumber}) ->
    #data_match_spec{group_name = GroupName,
                     group_number = GroupNumber}.
